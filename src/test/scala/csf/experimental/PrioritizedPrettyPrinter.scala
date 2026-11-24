package csf.experimental

import scala.annotation.tailrec
import scala.collection.mutable
import scala.meta.*
import scala.util.Random

object PrioritizedPrettyPrinter {
  type Priority = Int

  object Priorities {
    private val next = new java.util.concurrent.atomic.AtomicInteger

    // Allocate the lower priority first. Pass the same value to share a decision.
    def fresh(): Priority = next.updateAndGet(value => Math.incrementExact(value))
  }

  enum Doc {
    case Empty
    case Text(value: String)
    case HardLine
    case Concat(children: Vector[Doc])
    case Nest(amount: Int, child: Doc)
    case Choice(priority: Priority, expanded: Doc, flattened: Doc)
    case ConditionalNest(priority: Priority, amount: Int, child: Doc)

    def +(other: Doc): Doc = (this, other) match {
      case (Empty, _) => other
      case (_, Empty) => this
      case (Concat(children), _) => Concat(children :+ other)
      case _ => Concat(Vector(this, other))
    }

    def nest(amount: Int): Doc = Nest(amount, this)

    def nestWhenExpanded(priority: Priority, amount: Int): Doc =
      ConditionalNest(priority, amount, this)

    def grouped(priority: Priority): Doc = {
      val flat = flatten(this)
      if (flat eq this) then this else Choice(priority, this, flat)
    }
  }

  import Doc.*

  def concat(children: IterableOnce[Doc]): Doc = Concat(Vector.from(children))
  def softLine(priority: Priority): Doc = Choice(priority, HardLine, Text(" "))
  def softBreak(priority: Priority): Doc = Choice(priority, HardLine, Empty)
  def conditionalText(priority: Priority, expanded: String, flattened: String): Doc =
    Choice(priority, Text(expanded), Text(flattened))

  private def flatten(doc: Doc): Doc = doc match {
    case Choice(_, _, flat) => flatten(flat)
    case ConditionalNest(_, _, child) => flatten(child)
    case Concat(children) =>
      val flat = children.map(flatten)
      if (children.lazyZip(flat).forall(_ eq _)) then doc else Concat(flat)
    case Nest(amount, child) =>
      val flat = flatten(child)
      if (flat eq child) then doc else Nest(amount, flat)
    case _ => doc
  }

  enum PriorityState {
    case Undecided, Flattened, Rejected
  }

  enum RejectionPolicy {
    case Stop, Continue
  }

  import PriorityState.*

  // Widths count UTF-16 code units. Breaks are structural, and indent is emitted only before text.
  final case class Measure(firstWidth: Int, lastWidth: Int, maxWidth: Int, multiline: Boolean)

  private final case class Tail(endColumn: Int, lastWidth: Int, maxWidth: Int) {
    def nest(amount: Int): Tail = Tail(
      endColumn + amount,
      if (lastWidth == 0) then 0 else lastWidth + amount,
      if (maxWidth == 0) then 0 else maxWidth + amount,
    )
  }

  // firstWidth is relative to the incoming column. Tail columns are relative to
  // the incoming indentation, which is independent of the incoming column.
  private final case class Transfer(firstWidth: Int, tail: Option[Tail]) {
    def +(right: Transfer): Transfer = tail match {
      case None => Transfer(firstWidth + right.firstWidth, right.tail)
      case Some(left) =>
        val end = left.endColumn + right.firstWidth
        val occupied = if (right.firstWidth == 0) then left.lastWidth else end
        val maximum = math.max(left.maxWidth, occupied)
        val joined = right.tail match {
          case None => Tail(end, occupied, maximum)
          case Some(last) => last.copy(maxWidth = math.max(maximum, last.maxWidth))
        }
        Transfer(firstWidth, Some(joined))
    }

    def nest(amount: Int): Transfer =
      if (amount == 0 || tail.isEmpty) then this else copy(tail = tail.map(_.nest(amount)))

    def measure: Measure = tail match {
      case None => Measure(firstWidth, firstWidth, firstWidth, multiline = false)
      case Some(last) => Measure(firstWidth, last.lastWidth, math.max(firstWidth, last.maxWidth), multiline = true)
    }
  }

  private val empty = Transfer(0, None)
  private val line = Transfer(0, Some(Tail(0, 0, 0)))

  private def checkedWidth(value: String): Int = {
    require(!value.contains('\n') && !value.contains('\r'), "Text must be single-line")
    value.length
  }

  private[experimental] def measureRenderedText(value: String): Measure = {
    val first = value.indexOf('\n')
    if (first < 0) then Measure(value.length, value.length, value.length, multiline = false)
    else {
      var max = first
      var start = first + 1
      var end = value.indexOf('\n', start)
      while (end >= 0) {
        max = math.max(max, end - start)
        start = end + 1
        end = value.indexOf('\n', start)
      }
      val last = value.length - start
      Measure(first, last, math.max(max, last), multiline = true)
    }
  }

  final case class Stats(
    passes: Int,
    accepted: Int,
    rejected: Int,
    visitedNodes: Long,
    transformedOccurrences: Long,
    recomputedSummaries: Long,
    rollbacks: Int,
    maxDepth: Int,
    solveNanos: Long,
    renderNanos: Long,
    certifiedPriorities: Int,
  )

  final case class Layout(text: String, measure: Measure, assignments: Map[Priority, PriorityState], stats: Stats) {
    def state(priority: Priority): PriorityState = assignments.getOrElse(priority, Undecided)
  }

  private type Assignments = Map[Priority, PriorityState]

  private final class Counts {
    var accepted = 0
    var rejected = 0
    var visited = 0L
    var transformed = 0L
    var recomputed = 0L
    var maxDepth = 0
    var certified = 0
    var rollbacks = 0

    def visit(depth: Int): Unit = {
      visited += 1
      maxDepth = math.max(maxDepth, depth)
    }

    def stats(solve: Long, render: Long): Stats =
      Stats(accepted + rejected, accepted, rejected, visited, transformed, recomputed, rollbacks, maxDepth, solve, render,
        certified)
  }

  private final class Output {
    private val text = new StringBuilder
    private var pendingIndent = 0

    def write(value: String): Unit = if (value.nonEmpty) {
      if (pendingIndent != 0) then text.append(" " * pendingIndent)
      pendingIndent = 0
      text.append(value)
    }

    def break(indent: Int): Unit = {
      text.append('\n')
      pendingIndent = indent
    }

    def result(): String = text.result()
  }

  private def maxPriority(left: Option[Priority], right: Option[Priority]): Option[Priority] =
    (left, right) match {
      case (Some(a), Some(b)) => if (a >= b) then left else right
      case (Some(_), None) => left
      case (None, _) => right
    }

  private def scan(doc: Doc, assignments: Assignments, counts: Counts, out: Option[Output],
    attempt: Option[Priority] = None
  ): Option[Priority] = {
    var highest: Option[Priority] = None

    def isFlat(priority: Priority): Boolean = {
      if (attempt.contains(priority)) then counts.transformed += 1
      val state = assignments.getOrElse(priority, Undecided)
      if (state == Undecided && highest.forall(_ < priority)) then highest = Some(priority)
      state == Flattened
    }

    def loop(node: Doc, indent: Int, depth: Int): Unit = {
      counts.visit(depth)
      node match {
        case Empty => ()
        case HardLine => out.foreach(_.break(indent))
        case Text(value) =>
          checkedWidth(value)
          out.foreach(_.write(value))
        case Concat(children) => children.foreach(loop(_, indent, depth + 1))
        case Nest(amount, child) =>
          require(amount >= 0, "Indent must be nonnegative")
          loop(child, indent + amount, depth + 1)
        case ConditionalNest(priority, amount, child) =>
          require(amount >= 0, "Indent must be nonnegative")
          val offset = if (isFlat(priority)) then 0 else amount
          loop(child, indent + offset, depth + 1)
        case Choice(priority, expanded, flat) =>
          val child = if (isFlat(priority)) then flat else expanded
          loop(child, indent, depth + 1)
      }
    }

    loop(doc, 0, 1)
    highest
  }

  def solveReference(doc: Doc, width: Int, policy: RejectionPolicy): Layout = {
    require(width >= 0, "Width must be nonnegative")
    val counts = new Counts
    val start = System.nanoTime()
    var assignments = Map.empty[Priority, PriorityState]
    var next = scan(doc, assignments, counts, None)
    var stopped = false

    while (!stopped && next.nonEmpty) {
      val priority = next.get
      val candidate = assignments.updated(priority, Flattened)
      val output = new Output
      val candidateNext = scan(doc, candidate, counts, Some(output), next)
      if (measureRenderedText(output.result()).maxWidth <= width) {
        counts.accepted += 1
        assignments = candidate
        next = candidateNext
      } else {
        counts.rejected += 1
        counts.rollbacks += 1
        assignments = assignments.updated(priority, Rejected)
        stopped = policy == RejectionPolicy.Stop
        if (!stopped) then next = scan(doc, assignments, counts, None)
      }
    }

    val renderStart = System.nanoTime()
    val output = new Output
    scan(doc, assignments, counts, Some(output))
    val text = output.result()
    val renderEnd = System.nanoTime()
    Layout(text, measureRenderedText(text), assignments, counts.stats(renderStart - start, renderEnd - renderStart))
  }

  private final case class Summary(measure: Transfer, nextPriority: Option[Priority], height: Int)

  private enum Node(val summary: Summary) {
    case Leaf(doc: Doc, cached: Summary) extends Node(cached)
    case Pair(left: Node, right: Node, cached: Summary) extends Node(cached)
    case Indent(amount: Int, child: Node, cached: Summary) extends Node(cached)
    case Switch(priority: Priority, flat: Doc, child: Node, cached: Summary) extends Node(cached)
    case SwitchIndent(priority: Priority, amount: Int, child: Node, cached: Summary) extends Node(cached)
  }

  private final class MeasurementTree(counts: Counts) {
    import Node.*

    private def summary(measure: Transfer, next: Option[Priority], height: Int): Summary = {
      counts.recomputed += 1
      Summary(measure, next, height)
    }

    private def pair(left: Node, right: Node): Node = {
      val a = left.summary
      val b = right.summary
      Pair(left, right, summary(a.measure + b.measure, maxPriority(a.nextPriority, b.nextPriority),
        1 + math.max(a.height, b.height)))
    }

    private def indent(amount: Int, child: Node): Node =
      if (amount == 0) then child
      else Indent(amount, child, summary(child.summary.measure.nest(amount), child.summary.nextPriority,
        child.summary.height + 1))

    private def switch(priority: Priority, flat: Doc, child: Node): Node =
      Switch(priority, flat, child, summary(child.summary.measure,
        maxPriority(Some(priority), child.summary.nextPriority), child.summary.height + 1))

    private def switchIndent(priority: Priority, amount: Int, child: Node): Node =
      SwitchIndent(priority, amount, child, summary(child.summary.measure.nest(amount),
        maxPriority(Some(priority), child.summary.nextPriority), child.summary.height + 1))

    def build(doc: Doc, assignments: Assignments, attempt: Option[Priority] = None): Node = {
      def children(docs: Vector[Doc], from: Int, until: Int): Node = until - from match {
        case 0 => loop(Empty)
        case 1 => loop(docs(from))
        case count =>
          counts.visited += 1
          val middle = from + count / 2
          pair(children(docs, from, middle), children(docs, middle, until))
      }

      def loop(value: Doc): Node = {
        counts.visited += 1
        value match {
          case Empty => Leaf(Empty, summary(empty, None, 1))
          case Text(text) => Leaf(value, summary(Transfer(checkedWidth(text), None), None, 1))
          case HardLine => Leaf(HardLine, summary(line, None, 1))
          case Concat(docs) => children(docs, 0, docs.size)
          case Nest(amount, child) =>
            require(amount >= 0, "Indent must be nonnegative")
            indent(amount, loop(child))
          case Choice(priority, expanded, flat) =>
            if (attempt.contains(priority)) then counts.transformed += 1
            assignments.getOrElse(priority, Undecided) match {
              case Undecided => switch(priority, flat, loop(expanded))
              case Flattened => loop(flat)
              case Rejected => loop(expanded)
            }
          case ConditionalNest(priority, amount, child) =>
            require(amount >= 0, "Indent must be nonnegative")
            if (attempt.contains(priority)) then counts.transformed += 1
            val active = loop(child)
            assignments.getOrElse(priority, Undecided) match {
              case Undecided => switchIndent(priority, amount, active)
              case Flattened => active
              case Rejected => indent(amount, active)
            }
        }
      }

      loop(doc)
    }

    def decide(root: Node, priority: Priority, assignments: Assignments): Node = {
      val flatten = assignments(priority) == Flattened

      def loop(node: Node): Node = {
        counts.visited += 1
        if (!node.summary.nextPriority.contains(priority)) then node
        else node match {
          case Pair(left, right, _) => pair(loop(left), loop(right))
          case Indent(amount, child, _) => indent(amount, loop(child))
          case Switch(`priority`, flat, child, _) =>
            if (flatten) {
              counts.transformed += 1
              // A newly visible branch may contain a higher priority, or already-decided priorities.
              build(flat, assignments, Some(priority))
            } else loop(child)
          case Switch(other, flat, child, _) => switch(other, flat, loop(child))
          case SwitchIndent(`priority`, amount, child, _) =>
            val changed = loop(child)
            if (flatten) {
              counts.transformed += 1
              changed
            } else indent(amount, changed)
          case SwitchIndent(other, amount, child, _) => switchIndent(other, amount, loop(child))
          case leaf: Leaf => leaf
        }
      }

      loop(root)
    }

    def render(root: Node): String = {
      val out = new Output

      def loop(node: Node, indent: Int): Unit = {
        counts.visited += 1
        node match {
          case Leaf(Text(value), _) => out.write(value)
          case Leaf(HardLine, _) => out.break(indent)
          case Leaf(_, _) => ()
          case Pair(left, right, _) =>
            loop(left, indent)
            loop(right, indent)
          case Indent(amount, child, _) => loop(child, indent + amount)
          case Switch(_, _, child, _) => loop(child, indent)
          case SwitchIndent(_, amount, child, _) => loop(child, indent + amount)
        }
      }

      loop(root, 0)
      out.result()
    }
  }

  // Column transfers are monotone in their entry column. Bounding each choice
  // independently bounds every synchronized layout as well.
  private def allFit(doc: Doc, width: Int, counts: Counts, start: Long): Option[Layout] = {
    val out = new Output
    val priorities = mutable.HashSet.empty[Priority]
    var column = 0
    var occurrences = 0L

    def remember(priority: Priority): Unit = {
      priorities += priority
      occurrences += 1
    }

    def visit(doc: Doc, indent: Int, flatIndent: Int, emit: Boolean, allowChoices: Boolean): Boolean = {
      counts.visited += 1
      doc match {
        case Empty => true
        case HardLine =>
          column = indent
          if (emit) then out.break(flatIndent)
          true
        case Text(value) =>
          val size = value.length
          if (size == 0) then true
          else if (size > width - column || value.contains('\n') || value.contains('\r')) then false
          else {
            column += size
            if (emit) then out.write(value)
            true
          }
        case Concat(children) =>
          var i = 0
          while (i < children.size) {
            if (!visit(children(i), indent, flatIndent, emit, allowChoices)) then return false
            i += 1
          }
          true
        case Nest(amount, child) =>
          amount >= 0 && amount <= Int.MaxValue - indent &&
            visit(child, indent + amount, flatIndent + amount, emit, allowChoices)
        case ConditionalNest(priority, amount, child) =>
          if (!allowChoices || amount < 0 || amount > Int.MaxValue - indent) then false
          else {
            remember(priority)
            visit(child, indent + amount, flatIndent, emit, allowChoices)
          }
        case Choice(priority, expanded, flat) =>
          if (!allowChoices) then false
          else {
            remember(priority)
            val entry = column
            if (!visit(expanded, indent, flatIndent, emit = false, allowChoices = false)) then false
            else {
              val expandedEnd = column
              column = entry
              if (!visit(flat, indent, flatIndent, emit, allowChoices = false)) then false
              else {
                column = math.max(column, expandedEnd)
                true
              }
            }
          }
      }
    }

    if (!visit(doc, 0, 0, emit = true, allowChoices = true)) then None
    else {
      counts.accepted = priorities.size
      counts.certified = priorities.size
      counts.transformed = occurrences
      val assignments = priorities.iterator.map(_ -> Flattened).toMap
      // The scan fuses rendering with the width proof. Render time here covers finalizing the text.
      val renderStart = System.nanoTime()
      val text = out.result()
      val measure = measureRenderedText(text)
      val renderEnd = System.nanoTime()
      Some(Layout(text, measure, assignments, counts.stats(renderStart - start, renderEnd - renderStart)))
    }
  }

  private final class RegionOutput(width: Int) {
    val text = new java.lang.StringBuilder
    private var column = 0
    private var pendingIndent = 0
    var fits = true

    def write(value: String): Unit = if (fits && value.nonEmpty) {
      fits = value.length <= width - column
      if (fits) {
        while (pendingIndent > 0) {
          text.append(' ')
          pendingIndent -= 1
        }
        text.append(value)
        column += value.length
      }
    }

    def break(indent: Int): Unit = if (fits) {
      text.append('\n')
      column = indent
      pendingIndent = indent
    }

    def reset(indent: Int): Unit = {
      text.setLength(0)
      column = indent
      pendingIndent = indent
      fits = true
    }
  }

  // With a fitting expanded layout, private priorities on opposite sides of a
  // fixed break cannot affect each other's Continue decisions. This scan solves
  // the special case of at most one priority in each such region.
  private def solveRegions(doc: Doc, width: Int, counts: Counts, start: Long): Option[Layout] = {
    val result = new java.lang.StringBuilder
    val expanded = new RegionOutput(width)
    val flattened = new RegionOutput(width)
    val assignments = mutable.HashMap.empty[Priority, PriorityState]
    var hasPriority = false
    var regionPriority = 0
    var occurrences = 0L
    var accepted = 0

    def remember(priority: Priority): Boolean = {
      if (hasPriority) {
        if (regionPriority != priority) then return false
      } else {
        if (assignments.contains(priority)) then return false
        regionPriority = priority
        hasPriority = true
      }
      occurrences += 1
      true
    }

    def finish(): Unit = {
      if (hasPriority) {
        assignments.update(regionPriority, if (flattened.fits) then Flattened else Rejected)
        if (flattened.fits) then accepted += 1
        hasPriority = false
      }
      result.append(if (flattened.fits) then flattened.text else expanded.text)
    }

    def visit(node: Doc, indent: Int, flatIndent: Int, depth: Int, left: Boolean, right: Boolean, cut: Boolean): Boolean = {
      counts.visit(depth)
      node match {
        case Empty => true
        case Text(value) =>
          if (value.contains('\n') || value.contains('\r')) then false
          else {
            if (left) then expanded.write(value)
            if (right) then flattened.write(value)
            expanded.fits
          }
        case HardLine =>
          if (cut) {
            finish()
            result.append('\n')
            expanded.reset(indent)
            flattened.reset(indent)
          } else {
            if (left) then expanded.break(indent)
            if (right) then flattened.break(flatIndent)
          }
          true
        case Concat(children) =>
          var i = 0
          while (i < children.size) {
            if (!visit(children(i), indent, flatIndent, depth + 1, left, right, cut)) then return false
            i += 1
          }
          true
        case Nest(amount, child) =>
          amount >= 0 && amount <= Int.MaxValue - indent &&
            visit(child, indent + amount, flatIndent + amount, depth + 1, left, right, cut)
        case ConditionalNest(priority, amount, child) =>
          left && right && amount >= 0 && amount <= Int.MaxValue - indent && remember(priority) &&
            visit(child, indent + amount, flatIndent, depth + 1, true, true, false)
        case Choice(priority, broken, flat) =>
          left && right && remember(priority) &&
            visit(broken, indent, flatIndent, depth + 1, true, false, false) &&
            visit(flat, indent, flatIndent, depth + 1, false, true, false)
      }
    }

    if (!visit(doc, 0, 0, 1, true, true, true)) then None
    else {
      finish()
      counts.maxDepth = 0
      counts.accepted = accepted
      counts.rejected = assignments.size - accepted
      counts.transformed = occurrences
      val renderStart = System.nanoTime()
      val text = result.toString
      val measure = measureRenderedText(text)
      val immutableAssignments = assignments.toMap
      val renderEnd = System.nanoTime()
      Some(Layout(text, measure, immutableAssignments, counts.stats(renderStart - start, renderEnd - renderStart)))
    }
  }

  def solve(doc: Doc, width: Int, policy: RejectionPolicy): Layout =
    solveIncrementally(doc, width, policy, None)

  private[experimental] def audit(doc: Doc, width: Int, policy: RejectionPolicy)(
    check: (Measure, String, String, Assignments) => Unit
  ): Layout = solveIncrementally(doc, width, policy, Some(check))

  private def solveIncrementally(doc: Doc, width: Int, policy: RejectionPolicy,
    observe: Option[(Measure, String, String, Assignments) => Unit]
  ): Layout = {
    require(width >= 0, "Width must be nonnegative")
    val counts = new Counts
    val start = System.nanoTime()
    if (observe.isEmpty) then allFit(doc, width, counts, start) match {
      case Some(layout) => return layout
      case None if policy == RejectionPolicy.Continue => solveRegions(doc, width, counts, start) match {
        case Some(layout) => return layout
        case None => ()
      }
      case None => ()
    }
    val tree = new MeasurementTree(counts)
    var assignments = Map.empty[Priority, PriorityState]
    var root = tree.build(doc, assignments)
    var stopped = false

    def auditRoot(): Unit = observe.foreach { check =>
      val direct = new Output
      scan(doc, assignments, new Counts, Some(direct))
      check(root.summary.measure.measure, tree.render(root), direct.result(), assignments)
    }

    counts.maxDepth = root.summary.height
    auditRoot()
    while (!stopped && root.summary.nextPriority.nonEmpty) {
      val priority = root.summary.nextPriority.get
      val tentative = assignments.updated(priority, Flattened)
      val candidate = tree.decide(root, priority, tentative)
      counts.maxDepth = math.max(counts.maxDepth, candidate.summary.height)
      if (candidate.summary.measure.measure.maxWidth <= width) {
        counts.accepted += 1
        assignments = tentative
        root = candidate
      } else {
        // The old root is still intact. Only clear eligibility on its affected paths.
        counts.rejected += 1
        counts.rollbacks += 1
        assignments = assignments.updated(priority, Rejected)
        stopped = policy == RejectionPolicy.Stop
        if (!stopped) then root = tree.decide(root, priority, assignments)
      }
      auditRoot()
    }

    val renderStart = System.nanoTime()
    val text = tree.render(root)
    val renderEnd = System.nanoTime()
    Layout(text, root.summary.measure.measure, assignments, counts.stats(renderStart - start, renderEnd - renderStart))
  }
}

object PrioritizedScalaEmitter {
  import PrioritizedPrettyPrinter.*
  import Doc.*

  private enum Suffix {
    case Select(name: Term.Name)
    case Apply(args: Term.ArgClause)
    case TypeApply(args: Type.ArgClause)
  }

  private object LambdaArgument {
    def unapply(clause: Term.ArgClause): Option[Term.Function] =
      if (clause.mod.nonEmpty) then None
      else clause.values match {
        case (lambda: Term.Function) :: Nil => Some(lambda)
        case Term.Block(List(lambda: Term.Function)) :: Nil => Some(lambda)
        case _ => None
      }
  }

  def emit(term: Term): Doc = term match {
    case _: Term.Select | _: Term.Apply | _: Term.ApplyType => emitChain(term)
    case infix: Term.ApplyInfix if infix.targClause.values.isEmpty && infix.argClause.mod.isEmpty =>
      def operand(value: Term): Doc = value match {
        case _: Term.ApplyInfix => Text("(") + emit(value) + Text(")")
        case _ => emit(value)
      }

      val right = infix.argClause.values match {
        case one :: Nil => operand(one)
        case values => Text("(") + separated(values.map(emit), Text(", ")) + Text(")")
      }
      operand(infix.lhs) + Text(" " + csf.LeafPrinter.name(infix.op) + " ") + right
    case Term.Block(Nil) => Text("{}")
    case Term.Block(stats) =>
      val body = separated(stats.map {
        case value: Term => emit(value)
        case other => leaf(other)
      }, HardLine)
      Text("{") + (HardLine + body).nest(2) + HardLine + Text("}")
    case other => leaf(other)
  }

  // This experimental emitter formats call spines, lambda args, blocks and infix
  // bodies. ScalaMeta spells other single-line terms and tokens.
  private def leaf(tree: Tree): Doc = {
    val syntax = tree match {
      case value: Name => csf.LeafPrinter.name(value)
      case value: Lit.Int => value.value.toString
      case param: Term.Param if param.mods.isEmpty && param.decltpe.isEmpty && param.default.isEmpty =>
        csf.LeafPrinter.name(param.name)
      case other => other.syntax
    }
    require(!syntax.contains('\n') && !syntax.contains('\r'), s"Unsupported multiline ${tree.productPrefix}")
    Text(syntax)
  }

  private def separated(docs: List[Doc], separator: Doc): Doc =
    concat(docs.headOption.iterator ++ docs.drop(1).iterator.flatMap(doc => Iterator(separator, doc)))

  @tailrec
  private def spine(term: Term, suffixes: List[Suffix] = Nil): (Term, List[Suffix]) = term match {
    case Term.Select(qual, name) => spine(qual, Suffix.Select(name) :: suffixes)
    case call: Term.Apply => spine(call.fun, Suffix.Apply(call.argClause) :: suffixes)
    case call: Term.ApplyType => spine(call.fun, Suffix.TypeApply(call.targClause) :: suffixes)
    case root => (root, suffixes)
  }

  private def emitChain(term: Term): Doc = {
    val (receiver, suffixes) = spine(term)
    val lambdas = suffixes.count {
      case Suffix.Apply(LambdaArgument(_)) => true
      case _ => false
    }
    val singleTerminal = lambdas == 1 && suffixes.lastOption.exists {
      case Suffix.Apply(LambdaArgument(_)) => true
      case _ => false
    }

    // Terminal lambda can wrap independently. Other chains use one decision
    // to keep closing braces and subsequent dots on separate lines.
    val lambdaPriority = Priorities.fresh()
    val chainPriority = if (singleTerminal) then Priorities.fresh() else lambdaPriority

    def suffix(value: Suffix): Doc = value match {
      case Suffix.Select(name) => softBreak(chainPriority) + Text(".") + leaf(name)
      case Suffix.Apply(LambdaArgument(lambda)) => emitLambda(lambda, lambdaPriority)
      case Suffix.Apply(args) =>
        val open = if (args.mod.isEmpty) then "(" else "(using "
        Text(open) + separated(args.values.map(emit), Text(", ")) + Text(")")
      case Suffix.TypeApply(args) =>
        Text("[") + separated(args.values.map(leaf), Text(", ")) + Text("]")
    }

    val root = receiver match {
      case _: Term.ApplyInfix | _: Term.FunctionTerm | _: Term.If | _: Term.Match | _: Term.ApplyUnary =>
        Text("(") + emit(receiver) + Text(")")
      case other => emit(other)
    }
    val (base, dots) = suffixes.span {
      case Suffix.Select(_) => false
      case _ => true
    }
    val prefix = root + concat(base.map(suffix))
    if (dots.isEmpty) then prefix else prefix + concat(dots.map(suffix)).nestWhenExpanded(chainPriority, 2)
  }

  private def emitLambda(lambda: Term.Function, priority: Priority): Doc = {
    val params = lambda.paramClause.values match {
      case param :: Nil if param.decltpe.isEmpty && param.mods.isEmpty => leaf(param)
      case many => Text("(") + separated(many.map(leaf), Text(", ")) + Text(")")
    }
    conditionalText(priority, "{ ", "(") + params + Text(" =>") +
      (softLine(priority) + emit(lambda.body)).nest(2) +
      softBreak(priority) + conditionalText(priority, "}", ")")
  }
}

class PrioritizedPrettyPrinterSuite extends munit.FunSuite {
  import PrioritizedPrettyPrinter.*
  import Doc.*
  import PriorityState.*
  import RejectionPolicy.*

  private given Dialect = dialects.Scala3.withAllowSignificantIndentation(false)

  private def checkSolvers(doc: Doc, width: Int, policy: RejectionPolicy = Continue): Layout = {
    val result = solve(doc, width, policy)
    val reference = solveReference(doc, width, policy)
    assertEquals(result.text, reference.text)
    assertEquals(result.measure, reference.measure)
    assertEquals(result.assignments, reference.assignments)
    result
  }

  test("empty, concatenation, hardlines and softlines") {
    assertEquals(checkSolvers(Empty, 0).text, "")
    val doc = Text("ab") + Empty + Text("cd") + HardLine + Text("e") + softLine(1) + Text("f")
    assertEquals(checkSolvers(doc, 4).text, "abcd\ne f")
    assertEquals(checkSolvers(doc, 2).text, "abcd\ne\nf")
    assertEquals(checkSolvers(doc, 2).measure, Measure(4, 1, 4, multiline = true))
  }

  test("hardlines capture lexical indentation and blank lines discard pending spaces") {
    val doc = Text("a") + (HardLine + Text("b") + (HardLine + Text("c")).nest(3)).nest(2) +
      (HardLine.nest(8) + HardLine + Text("d")) + HardLine.nest(10)
    val result = checkSolvers(doc, 6)
    assertEquals(result.text, "a\n  b\n     c\n\nd\n")
    assertEquals(result.measure, Measure(1, 0, 6, multiline = true))

    val pending = HardLine.nest(4) + Text("x").nest(3)
    assertEquals(checkSolvers(pending, 5).text, "\n    x")
    assertEquals(checkSolvers(pending, 5).measure, Measure(0, 5, 5, multiline = true))
  }

  test("grouped selects the flattened alternatives while preserving hardlines") {
    val left = Text("aa") + softLine(1) + Text("bb")
    val right = Text("cc") + conditionalText(1, "{", "(") + Text("x") + conditionalText(1, "}", ")")
    val doc = (left + HardLine + right).grouped(2)
    assertEquals(checkSolvers(doc, 5).text, "aa bb\ncc(x)")
    assertEquals(checkSolvers(doc, 4).text, "aa\nbb\ncc{x}")
  }

  test("adjacent and distant occurrences choose punctuation atomically") {
    val adjacent = conditionalText(5, "{", "(") + conditionalText(5, "}", ")")
    val distant = (HardLine + Text("abc") + softLine(5) + Text("def")).nest(2)
    val doc = adjacent + distant + HardLine + conditionalText(5, "E", "F").nest(4)
    assertEquals(checkSolvers(doc, 9).text, "()\n  abc def\nF")
    val rejected = checkSolvers(doc, 8)
    assertEquals(rejected.text, "{}\n  abc\n  def\nE")
    assertEquals(rejected.state(5), Rejected)
    assertEquals(rejected.stats.transformedOccurrences, 4L)
    assertEquals(rejected.stats.rollbacks, 1)
  }

  test("large atomic groups measure once after every occurrence changes") {
    val size = 2048
    val doc = concat(Vector.fill(size)(conditionalText(9, "x", "xx")))
    val accepted = checkSolvers(doc, size * 2)
    val rejected = checkSolvers(doc, size * 2 - 1)
    assertEquals(accepted.text, "xx" * size)
    assertEquals(rejected.text, "x" * size)
    assertEquals(accepted.stats.passes, 1)
    assertEquals(accepted.stats.transformedOccurrences, size.toLong)
    assert(accepted.stats.maxDepth <= 13)
    assert(accepted.stats.recomputedSummaries < size * 6L)

    val shrink = conditionalText(9, "x", "xxxx") + conditionalText(9, "xxxx", "x")
    assertEquals(checkSolvers(shrink, 5).text, "xxxxx")
    assertEquals(checkSolvers(shrink, 5).state(9), Flattened)
  }

  test("all-fit scan certifies static priorities with conditional indentation") {
    val doc = Text("a") + (softLine(2) + Text("b") + softBreak(1) + Text("c")).nestWhenExpanded(3, 2)
    val result = checkSolvers(doc, 8, Stop)
    assertEquals(result.text, "a bc")
    assertEquals(result.stats.recomputedSummaries, 0L)
    assertEquals(result.stats.certifiedPriorities, 3)
  }

  test("all-fit scan preserves rejection when the final layout could still fit") {
    val doc = conditionalText(2, "a", "long") + conditionalText(1, "long", "a")
    val stopped = checkSolvers(doc, 5, Stop)
    assertEquals(stopped.text, "along")
    assertEquals(stopped.state(2), Rejected)
    assertEquals(stopped.state(1), Undecided)
    assertEquals(stopped.stats.certifiedPriorities, 0)
    assertEquals(checkSolvers(doc, 5, Continue).text, "aa")
  }

  test("regions decide private priorities independently and keep lexical indentation") {
    val first = (HardLine + Text("a") + conditionalText(5, "b", "c")).nest(2)
    val second = (HardLine + Text("long") + softLine(7) + Text("words")).nest(1)
    val result = checkSolvers(first + second, 6, Continue)
    assertEquals(result.text, "\n  ac\n long\n words")
    assertEquals(result.stats.recomputedSummaries, 0L)
    assertEquals(result.stats.accepted, 1)
    assertEquals(result.stats.rejected, 1)
  }

  test("regions cannot separate shared priorities") {
    val first = Text("a") + conditionalText(7, "b", "c")
    val second = Text("long") + softLine(7) + Text("words")
    val result = checkSolvers(first + HardLine + second, 5, Continue)
    assertEquals(result.text, "ab\nlong\nwords")
    assertEquals(result.state(7), Rejected)
    assert(result.stats.recomputedSummaries > 0L)
  }

  test("regions fall back when the initial layout overflows elsewhere") {
    val wide = conditionalText(1, "too wide", "b")
    val small = conditionalText(2, "a", "c")
    val result = checkSolvers(small + HardLine + wide, 4, Continue)
    assertEquals(result.text, "a\nb")
    assertEquals(result.state(2), Rejected)
    assertEquals(result.state(1), Flattened)
  }

  test("separator inside conditional nesting keeps its pending indentation local") {
    val doc = ((HardLine + Text("a")).nestWhenExpanded(1, 4) + Text("b")) + HardLine +
      Text("long") + softLine(2) + Text("words")
    val result = checkSolvers(doc, 6, Continue)
    assertEquals(result.text, "\nab\nlong\nwords")
    assertEquals(result.stats.recomputedSummaries, 0L)
  }

  test("root fit includes the left ending column and the right first line") {
    val left = Text("head") + (HardLine + Text("ab")).nest(2) + softBreak(3) + Text("cd")
    val right = Text("ef") + HardLine + Text("tail")
    assertEquals(checkSolvers(left, 7).state(3), Flattened)
    assertEquals(checkSolvers(right, 7).measure.maxWidth, 4)
    assertEquals(checkSolvers(left + right, 7).text, "head\n  ab\ncdef\ntail")
    assertEquals(checkSolvers(left + right, 8).text, "head\n  abcdef\ntail")
    assertEquals(checkSolvers(left + HardLine + right, 7).state(3), Flattened)
  }

  test("conditional nesting adjusts columns without changing child choices") {
    val doc = Text("a") +
      (softBreak(2) + Text(".b") + (softLine(1) + Text("body")).nest(2)).nestWhenExpanded(2, 2)
    assertEquals(checkSolvers(doc, 7).text, "a.b\n  body")
    assertEquals(checkSolvers(doc, 8).text, "a.b body")
  }

  test("Stop leaves lower priorities undecided; Continue can recover initial overflow") {
    val doc = conditionalText(9, "short", "too long") + HardLine + conditionalText(5, "also too long", "ok")
    val stopped = checkSolvers(doc, 5, Stop)
    assertEquals(stopped.text, "short\nalso too long")
    assertEquals(stopped.state(9), Rejected)
    assertEquals(stopped.state(5), Undecided)
    val continued = checkSolvers(doc, 5, Continue)
    assertEquals(continued.text, "short\nok")
    assertEquals(continued.state(5), Flattened)
    assertEquals(checkSolvers(Text("unbreakable"), 3).stats.passes, 0)
  }

  test("newly visible choices honor accepted, rejected, current and newly eligible priorities") {
    val existing = conditionalText(9, "A", "a") + conditionalText(8, "R", "too long") + HardLine
    val revealed = conditionalText(9, "W", "a") + conditionalText(8, "R", "too long") +
      conditionalText(5, "W", "b") + conditionalText(100, "Q", "q")
    val doc = existing + Choice(5, Text("E") + conditionalText(4, "H", "h"), revealed)
    val result = checkSolvers(doc, 5)
    assertEquals(result.text, "aR\naRbq")
    assertEquals(result.state(100), Flattened)
    assertEquals(result.state(4), Undecided)
    assertEquals(result.stats.passes, 4)
  }

  test("rollback discards speculative visibility and preserves eligibility in the old branch") {
    val doc = Choice(10, conditionalText(3, "old", "ok"), Text("overflow") + conditionalText(20, "E", "F"))
    val result = checkSolvers(doc, 3)
    assertEquals(result.text, "ok")
    assertEquals(result.state(10), Rejected)
    assertEquals(result.state(20), Undecided)
    assertEquals(result.state(3), Flattened)
  }

  test("exhausted subtrees retain measurement and are pruned from discovery") {
    val size = 1024
    val doc = concat(Vector.tabulate(size)(p => Choice(p, HardLine, Text("x"))))
    val fast = checkSolvers(doc, size)
    val slow = solveReference(doc, size, Continue)
    assertEquals(fast.text, "x" * size)
    assertEquals(fast.stats.passes, size)
    assert(fast.stats.visitedNodes < size * 30L)
    assert(fast.stats.visitedNodes * 30 < slow.stats.visitedNodes)
  }

  private def structure(value: Any): Any = value match {
    case Term.Block(List(term: Term)) => structure(term)
    case tree: Tree => (tree.productPrefix, tree.productIterator.map(structure).toList)
    case values: List[?] => values.map(structure)
    case value: Option[?] => value.map(structure)
    case other => other
  }

  private def scalaLayout(source: String, width: Int, policy: RejectionPolicy = Stop): String = {
    val ast = source.parse[Term].get
    val result = checkSolvers(PrioritizedScalaEmitter.emit(ast), width, policy)
    assertEquals(structure(result.text.parse[Term].get), structure(ast), result.text)
    result.text
  }

  test("simple chains split every dot at the exact width boundary") {
    val source = "xs.left.right()"
    assertEquals(scalaLayout(source, source.length), source)
    assertEquals(scalaLayout(source, source.length - 1), "xs\n  .left\n  .right()")
  }

  test("a terminal lambda wraps before dots and follows their conditional indent") {
    val source = "a.b.c.d.f(x => x * 2)"
    val header = "a.b.c.d.f{ x =>"
    assertEquals(scalaLayout(source, source.length), source)
    assertEquals(scalaLayout(source, source.length - 1), s"$header\n  x * 2\n}")
    assertEquals(scalaLayout(source, header.length), s"$header\n  x * 2\n}")
    assertEquals(scalaLayout(source, header.length - 1), "a\n  .b\n  .c\n  .d\n  .f{ x =>\n    x * 2\n  }")
  }

  test("Continue can compact a terminal lambda after rejecting its chain") {
    val source = "receiverName.b.c.d.f(x => x * 2)"
    val width = "receiverName.b.c.d.f{ x =>".length - 1
    assertEquals(scalaLayout(source, width, Stop), "receiverName\n  .b\n  .c\n  .d\n  .f{ x =>\n    x * 2\n  }")
    assertEquals(scalaLayout(source, width, Continue), "receiverName\n  .b\n  .c\n  .d\n  .f(x => x * 2)")
  }

  test("non-terminal and multiple lambdas share the expanded or flattened layout") {
    val one = "a.b.f(x => x * 2).e.g()"
    assertEquals(scalaLayout(one, one.length), one)
    assertEquals(scalaLayout(one, one.length - 1), "a\n  .b\n  .f{ x =>\n    x * 2\n  }\n  .e\n  .g()")

    val many = "a.b.c.d.f(x => x * 2).e.g(y => y + 2)"
    assertEquals(scalaLayout(many, many.length), many)
    val expanded = "a\n  .b\n  .c\n  .d\n  .f{ x =>\n    x * 2\n  }\n  .e\n  .g{ y =>\n    y + 2\n  }"
    assertEquals(scalaLayout(many, many.length - 1, Continue), expanded)
    assertEquals(scalaLayout(many, 10, Continue), expanded)
  }

  test("nested chains stay inside lambda indentation") {
    val source = "a.b.map(x => x.longFields.filter(p).map(y => y + 2))"
    assertEquals(scalaLayout(source, source.length, Continue), source)
    assertEquals(scalaLayout(source, 19, Stop),
      "a\n  .b\n  .map{ x =>\n    x\n      .longFields\n      .filter(p)\n      .map{ y =>\n        y + 2\n      }\n  }")
    assertEquals(scalaLayout(source, 19, Continue),
      "a.b.map(x => x\n    .longFields\n    .filter(p)\n    .map{ y =>\n      y + 2\n    })")
  }

  test("generic calls and typed lambda parameters round-trip across delimiter changes") {
    val source = "xs.map[Int]((value: Int) => value + 2)"
    assertEquals(scalaLayout(source, source.length), source)
    assertEquals(scalaLayout(source, source.length - 1), "xs.map[Int]{ (value: Int) =>\n  value + 2\n}")
  }

  test("direct leaf emission preserves quoted identifiers") {
    val source = "xs.`match`(`type` => `type` * 2)"
    assertEquals(scalaLayout(source, source.length), source)
    assertEquals(scalaLayout(source, source.length - 1), "xs.`match`{ `type` =>\n  `type` * 2\n}")
  }

  test("separately emitted chains allocate independent priorities") {
    val short = "x.a.b".parse[Term].get
    val long = "longReceiver.first.second()".parse[Term].get
    val doc = PrioritizedScalaEmitter.emit(short) + HardLine + PrioritizedScalaEmitter.emit(long)
    val result = checkSolvers(doc, 14, Continue)
    assertEquals(result.text, "x.a.b\nlongReceiver\n  .first\n  .second()")
    assertEquals(result.stats.accepted, 1)
    assertEquals(result.stats.rejected, 1)
  }

  private def randomDoc(random: Random, depth: Int): Doc = {
    def child = randomDoc(random, depth - 1)
    def priority = random.nextInt(8) - 4
    random.nextInt(if (depth == 0) then 3 else 8) match {
      case 0 => Empty
      case 1 => Text("x" * random.nextInt(7))
      case 2 => HardLine
      case 3 => concat(Vector.fill(random.nextInt(5))(child))
      case 4 => child.nest(random.nextInt(4))
      case 5 => Choice(priority, child, child)
      case 6 => child.nestWhenExpanded(priority, random.nextInt(4))
      case _ => Text("[") + child + softLine(priority) + child + Text("]")
    }
  }

  test("random measurement composition is associative") {
    val random = new Random(0x52f1L)
    for (_ <- 0 until 300) {
      val a = randomDoc(random, 3)
      val b = randomDoc(random, 3)
      val c = randomDoc(random, 3)
      val left = concat(Vector(concat(Vector(a, b)), c)).nest(3)
      val right = concat(Vector(a, concat(Vector(b, c)))).nest(3)
      val width = random.nextInt(35)
      val first = solve(left, width, Continue)
      val second = solve(right, width, Continue)
      assertEquals(first.text, second.text)
      assertEquals(first.measure, second.measure)
      assertEquals(first.assignments, second.assignments)
    }
  }

  test("random cached measurements agree with rendering at initialization, acceptance and rollback") {
    val random = new Random(0x619eL)
    for (_ <- 0 until 400; policy <- RejectionPolicy.values) {
      val doc = randomDoc(random, 5)
      val width = random.nextInt(35)
      var previous = ""
      var rejected = 0
      var checks = 0
      val solved = audit(doc, width, policy) { (measure, cached, direct, assignments) =>
        assertEquals(cached, direct)
        assertEquals(measure, measureRenderedText(cached))
        val rejectionCount = assignments.valuesIterator.count(_ == Rejected)
        if (rejectionCount > rejected) then assertEquals(cached, previous)
        previous = cached
        rejected = rejectionCount
        checks += 1
      }
      val reference = solveReference(doc, width, policy)
      assertEquals(checks, solved.stats.passes + 1)
      assertEquals(solved.text, reference.text)
      assertEquals(solved.assignments, reference.assignments)
      val optimized = solve(doc, width, policy)
      assertEquals(optimized.text, reference.text)
      assertEquals(optimized.measure, reference.measure)
      assertEquals(optimized.assignments, reference.assignments)
    }
  }
}

object PrioritizedPrettyPrinterBenchmark {
  import PrioritizedPrettyPrinter.*
  import Doc.*

  private given Dialect = dialects.Scala3.withAllowSignificantIndentation(false)

  @volatile private var rendered: String = ""

  private def benchmarkPaiges(samples: Int): Unit = {
    import org.typelevel.paiges.{Doc as Paiges}

    def compare(label: String, repetitions: Int)(paiges: => String, prototype: => String): Unit = {
      require(paiges == prototype, s"Different output for $label")

      def time(run: => String): Double = {
        val start = System.nanoTime()
        var i = 0
        while (i < repetitions) {
          rendered = run
          i += 1
        }
        (System.nanoTime() - start).toDouble / repetitions / 1000
      }

      val timings = Vector.tabulate(samples + 6) { i =>
        if (i % 2 == 0) then (time(paiges), time(prototype))
        else {
          val experimental = time(prototype)
          (time(paiges), experimental)
        }
      }.drop(6)
      val old = timings.map(_._1).sorted.apply(samples / 2)
      val next = timings.map(_._2).sorted.apply(samples / 2)
      println(f"$label%-33s Paiges $old%8.2f us  prototype $next%8.2f us  prototype/Paiges ${next / old}%5.2fx")
    }

    def scalaCase(label: String, samples: Vector[String], width: Int, declarations: Boolean): Unit = {
      val lines = Vector.tabulate(256) { i =>
        val expr = samples(i % samples.size)
        if (declarations) then s"val v$i = $expr" else expr
      }
      val source = lines.mkString("object Bench {\n", "\n", "\n}").parse[Source].get
      val stats = source.stats.collect { case obj: Defn.Object => obj }.head.templ.body.stats
      val options = csf.FormatOptions(columns = width)

      def doc(): Doc = {
        val body = stats.iterator.flatMap { stat =>
          val statement = stat match {
            case value: Defn.Val =>
              val name = value.pats.head.asInstanceOf[Pat.Var].name.value
              Text(s"val $name = ") + PrioritizedScalaEmitter.emit(value.rhs)
            case term: Term => PrioritizedScalaEmitter.emit(term)
            case other => throw new IllegalArgumentException(other.productPrefix)
          }
          Iterator(HardLine, statement)
        }
        Text("object Bench {") + concat(body).nest(2) + HardLine + Text("}")
      }

      def wadler(): Paiges = csf.Printer.print(source, options).fold(throw _, identity)
      def render(doc: Doc): String = solve(doc, width, RejectionPolicy.Continue).text
      val prebuiltPaiges = wadler()
      val prebuiltPrototype = doc()
      compare(s"$label layout", 64)(prebuiltPaiges.render(width), render(prebuiltPrototype))
      compare(s"$label AST+layout", 32)(wadler().render(width), render(doc()))
    }

    scalaCase("identical Scala chains", Vector("receiver.left.right.map(x => x * 2)"), 100, declarations = true)
    scalaCase("varied Scala chains", Vector("receiver.left.right.map(x => x * 2)",
      "records.filter(r => r.enabled).toList", "env.context.commands.addHandler[Unit](request)",
      "jobs.queue.map(job => job.result.value)", "input.values.map(x => x + 1).map(y => y * 2)",
      "path.context.lookup().getOrElse(defaultValue)", "xs.filter(x => x != 0)",
      "ctx.commands.entry[A](id, value)"), 100, declarations = true)
    scalaCase("mixed-fit Scala chains", Vector("xs.map(f).filter(p)",
      "receiver.longFirstName().secondName().thirdName()"), 30, declarations = false)

    val words = Vector.tabulate(256)(i => if (i % 2 == 0) then "short" else "abcdefghijkl")
    val paiges = Paiges.intercalate(Paiges.hardLine, words.map { word =>
      (Paiges.text(word) + Paiges.line + Paiges.text(word)).grouped
    })
    val prototype = concat(words.zipWithIndex.flatMap { (word, i) =>
      val row = Text(word) + softLine(i) + Text(word)
      if (i == 0) then Vector(row) else Vector(HardLine, row)
    })
    compare("mixed-fit lines", 96)(paiges.render(20), solve(prototype, 20, RejectionPolicy.Continue).text)

    val shared = concat(Vector.fill(4096)(Text("x") + softBreak(0)))
    val sharedPaiges = Paiges.cat(Vector.fill(4096)(Paiges.char('x') + Paiges.lineBreak)).grouped
    val unique = concat(Vector.tabulate(4096)(i => Text("x") + softBreak(i)))
    val uniquePaiges = Paiges.cat(Vector.fill(4096)((Paiges.char('x') + Paiges.lineBreak).grouped))
    compare("4096 accepted shared breaks", 48)(sharedPaiges.render(4096), solve(shared, 4096, RejectionPolicy.Continue).text)
    compare("4096 rejected shared breaks", 48)(sharedPaiges.render(40), solve(shared, 40, RejectionPolicy.Continue).text)
    compare("4096 independent breaks", 12)(uniquePaiges.render(4096), solve(unique, 4096, RejectionPolicy.Continue).text)
  }

  def main(args: Array[String]): Unit = {
    if (args.headOption.contains("--paiges")) {
      val samples = args.lift(1).fold(9)(_.toInt)
      require(samples > 0)
      benchmarkPaiges(samples)
      return
    }
    val size = args.headOption.fold(4096)(_.toInt)
    val samples = args.lift(1).fold(3)(_.toInt)
    require(size >= 32 && samples >= 1)

    val unique = concat(Vector.tabulate(size)(p => Choice(p, HardLine, Text("x"))))
    val shared = concat(Vector.tabulate(size)(p => Choice(p % 8, HardLine, Text("x"))))
    val deep = (0 until 320).foldLeft(Text("end"): Doc) { (inner, p) =>
      (Text("x") + softLine(p) + inner).nest(1)
    }
    val wideSource = "receiver" + (1 to 80).map(i => s".select$i()").mkString + ".map(x => x * 2)"
    val wide = PrioritizedScalaEmitter.emit(wideSource.parse[Term].get)
    val chain = "receiver.left.right.map(x => x * 2)".parse[Term].get
    val independent = concat(Vector.fill(size / 16)(PrioritizedScalaEmitter.emit(chain) + HardLine))
    val rejected = concat(Vector.tabulate(size / 4)(p => Choice(p, Text("x") + HardLine, Text("x" * 81))))
    val synthetic = Vector(
      ("flat unique", unique, size), ("flat shared", shared, size), ("deep", deep, 400),
      ("wide Scala chain", wide, 80), ("independent Scala chains", independent, 32),
      ("mostly rejected", rejected, 80),
    )

    val files = args.drop(2)
    val corpus = if (files.nonEmpty) files.toList else List("examples/Example.scala", "src/main/scala/csf/Formatter.scala")
    val realWidth = 88
    val real = corpus.flatMap { path =>
      val source = java.nio.file.Files.readString(java.nio.file.Path.of(path)).parse[Source].get
      source.collect { case term: Term.Apply => term }.flatMap { term =>
        try Some(PrioritizedScalaEmitter.emit(term))
        catch { case e: IllegalArgumentException if e.getMessage.startsWith("requirement failed: Unsupported multiline") => None }
      }
    }.filter(doc => solve(doc, 0, RejectionPolicy.Stop).measure.maxWidth <= realWidth)
    println(s"size=$size samples=$samples realScalaCalls=${real.size}")

    def benchmark(label: String, doc: Doc, width: Int, policy: RejectionPolicy): Unit = {
      def runPair(): (Layout, Layout) = {
        val fast = solve(doc, width, policy)
        val ref = solveReference(doc, width, policy)
        require(fast.text == ref.text && fast.measure == ref.measure && fast.assignments == ref.assignments, label)
        (fast, ref)
      }

      runPair()
      val runs = Vector.fill(samples)(runPair())
      val fast = runs.map(_._1.stats).sortBy(stats => stats.solveNanos + stats.renderNanos).apply(samples / 2)
      val ref = runs.map(_._2.stats).sortBy(stats => stats.solveNanos + stats.renderNanos).apply(samples / 2)
      val speedup = (ref.solveNanos + ref.renderNanos).toDouble / (fast.solveNanos + fast.renderNanos)
      println(f"$label%s [$policy%s] width=$width%d $speedup%.2fx")
      for ((name, stats) <- Vector("incremental" -> fast, "reference" -> ref)) {
        val solveMs = stats.solveNanos / 1e6
        val renderMs = stats.renderNanos / 1e6
        println(f"  $name%s solve=$solveMs%.3f ms render=$renderMs%.3f ms " +
          s"passes=${stats.passes} accepted=${stats.accepted} rejected=${stats.rejected} " +
          s"visited=${stats.visitedNodes} transformed=${stats.transformedOccurrences} " +
          s"recomputed=${stats.recomputedSummaries} rollbacks=${stats.rollbacks} depth=${stats.maxDepth} " +
          s"certified=${stats.certifiedPriorities}")
      }
    }

    val realDoc = concat(real.map(_ + HardLine))
    for ((label, doc, width) <- synthetic :+ (("real Scala calls", realDoc, realWidth));
         policy <- RejectionPolicy.values) {
      benchmark(label, doc, width, policy)
    }
  }
}
