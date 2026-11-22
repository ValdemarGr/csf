package csf

import org.typelevel.paiges.Doc
import scala.collection.mutable.ArrayBuffer
import scala.meta.*
import scala.meta.internal.prettyprinters.TreeSyntacticGroup
import scala.meta.internal.tokens.Chars.isOperatorPart
import scala.meta.trees.Origin

private[csf] object Printer {
  private val assignmentSeparator = Doc.text(" =")
  private val functionArrow = Doc.text(" => ")
  private val arrowSuffix = Doc.text(" =>")
  private val casePrefix = Doc.text("case ")
  private val commaSpace = Doc.comma + Doc.space
  private val commaLine = Doc.comma + Doc.line
  private val softCloseParen = Doc.lineBreak + Doc.char(')')
  private val softCloseBrace = Doc.line + Doc.char('}')
  private val emptyParentheses = Doc.empty.tightBracketBy(Doc.char('('), Doc.char(')'))

  private enum BodyExprDoc {
    case Direct(doc: Doc)
    case Prio(header: Doc, call: CallLayout)
  }

  private final case class CallLayout(leading: Doc, callee: Doc, flat: Option[(Doc, Int)], dotIndent: Int,
      scope: Int => Doc) {
    def fittingCallee(remaining: => Int): Option[Doc] = flat.collect {
      case (doc, width) if width <= remaining => doc
    }

    def render(indent: Int, fitting: Option[Doc]): Doc = {
      val offset = if (fitting.isDefined) 0 else dotIndent
      leading + fitting.getOrElse(callee) + scope(indent + offset).nested(offset)
    }
  }

  private enum InfixWork {
    case Visit(value: Term, indent: Int, chained: Boolean)
    case Operand(outer: Term.ApplyInfix, value: Term, indent: Int)
    case Operator(value: Term.ApplyInfix, indent: Int, leading: Doc, before: Int, chained: Boolean)
    case Finish(leading: Doc, before: Int, beforeComments: Int, lhs: Doc, op: Doc, types: Doc, chained: Boolean,
        chainOperator: Boolean, attached: Boolean)
    case CloseBlock(value: Term.Block, indent: Int, leading: Doc)
    case CloseParens(before: Int)
    case CloseArgument(value: Term.ArgClause, indent: Int, before: Int)
  }

  def print(source: Source, options: FormatOptions): Either[FormatError, Doc] = {
    try Right(new Renderer(source, options).print())
    catch { case error: FormatError => Left(error) }
  }

  private final class Renderer(source: Source, options: FormatOptions) {
    private given Dialect = dialects.Scala3
    private val commentTokens = source.tokens.iterator.collect {
      case comment: Token.Comment => comment
    }.toArray
    private var commentIndex = 0
    private var hardLineCount = 0

    def print(): Doc = tree(source, 0)

    // Track mandatory breaks while building Docs, without rendering them again.
    private def hardLine: Doc = {
      hardLineCount += 1
      Doc.hardLine
    }

    private def parenthesized(body: => Doc): Doc = {
      val before = hardLineCount
      val contents = body
      parenthesized(contents, hardLineCount != before)
    }

    private def parenthesized(contents: Doc, multiline: Boolean): Doc =
      if (multiline) Doc.char('(') + (hardLine + contents).nested(2) + hardLine + Doc.char(')')
      else contents.tightBracketBy(Doc.char('('), Doc.char(')'))

    private def controlCondition(condition: Term, keyword: String, indent: Int): Doc =
      options.controlSyntax match {
        case ControlSyntax.New =>
          val leading = leadingComments(condition, indent + 2, lineStart = true)
          val contents = if (leading.isEmpty) Doc.space + syntax(condition, indent)
          else (hardLine + leading + syntax(condition, indent + 2)).nested(2)
          contents + Doc.space + Doc.text(keyword)
        case ControlSyntax.Old =>
          Doc.space + parenthesized(tree(condition, indent + 2, lineStart = true))
      }

    @annotation.tailrec
    private def expression(value: Term): Term = value match {
      case block: Term.Block if !argumentBlock(block) && !block.parent.exists {
            case _: Term.QuotedMacroExpr | _: Term.SplicedMacroExpr => true
            case _ => false
          } => block.stats match {
        case (single: Term) :: Nil => expression(single)
        case _ => block
      }
      case _ => value
    }

    private def arrowTail(indent: Int)(body: => Doc): Doc = {
      val before = hardLineCount
      val beforeComments = commentIndex
      val contents = body
      val multiline = hardLineCount != before ||
        (commentIndex > beforeComments && commentTokens(commentIndex - 1).text.startsWith("//"))
      if (contents.isEmpty) Doc.empty
      else if (multiline) (hardLine + contents).nested(indent)
      else (Doc.line + contents).nested(indent).grouped
    }

    private def assignmentBody(value: Tree, indent: Int, allowBreak: Boolean = true,
        header: Option[Doc] = None): Doc = {
      import BodyExprDoc.*
      val body = if (!allowBreak) Direct(namedAssignmentBody(value, indent)) else value match {
        case block: Term.Block if argumentBlock(block) => Direct(argumentAssignmentBody(block, indent))
        case term: Term => {
          val canonical = expression(term)
          canonical match {
            case _: Term.Block => Direct(Doc.space + requiredBlock(term, indent, compactEmpty = false))
            case _ if bracedLambda(canonical) => Direct(Doc.space + tree(term, indent))
            case _ if rhsCommentNeedsBlock(term, canonical) =>
              Direct(Doc.space + requiredBlock(term, indent, compactEmpty = false))
            case _: Term.PartialFunction => Direct(Doc.space + tree(term, indent))
            case call: Term.Apply =>
              header.flatMap(declaration => prioritizedCall(term, call, indent + 2).map(Prio(declaration, _))).getOrElse {
                Direct(arrowTail(2)(tree(term, indent + 2, lineStart = true)))
              }
            case _ => Direct(arrowTail(2)(tree(term, indent + 2, lineStart = true)))
          }
        }
        case _: Ctor.Block => Direct(Doc.space + tree(value, indent))
        case refinement: Type.Refine if refinement.tpe.isEmpty => Direct(Doc.space + tree(value, indent))
        case _ => Direct(arrowTail(2)(tree(value, indent + 2, lineStart = true)))
      }
      assignmentSeparator + (body match {
        case Direct(doc) => doc
        case Prio(header, call) =>
          val inline = if (call.leading.isEmpty) {
            call.fittingCallee(options.columns - assignmentHeaderWidth(header, indent) - 1)
          } else None
          val rhsOffset = if (inline.isDefined) 0 else 2
          val rhsIndent = indent + rhsOffset
          val contents = call.render(rhsIndent, inline.orElse(call.fittingCallee(options.columns - rhsIndent)))
          val footer = tokenComments(value, value.endOffset, rhsIndent)
          val start = if (rhsOffset == 0) Doc.space else hardLine
          (start + contents + (if (footer.isEmpty) Doc.empty else hardLine + footer)).nested(rhsOffset)
      })
    }

    private def expressionLine(value: Tree, indent: Int): Doc = value match {
      case call: Term.Apply if bracedCall(call.argClause) =>
        prioritizedCall(call, call, indent).fold(tree(value, indent, lineStart = true)) { layout =>
          layout.render(indent, layout.fittingCallee(options.columns - indent))
        }
      case _ => tree(value, indent, lineStart = true)
    }

    private def prioritizedCall(term: Term, call: Term.Apply, indent: Int): Option[CallLayout] = {
      val arguments = call.argClause
      val braced = arguments.values.headOption.filter(_ => bracedCall(arguments)).filter {
        case block: Term.Block => block.stats.nonEmpty
        case _ => true
      }
      val opener = braced.map(argument => bracedArgumentSpace(argument) + Doc.char('{')).orElse {
        Option.when(arguments.values.nonEmpty && openingToken(arguments).exists(_.isInstanceOf[Token.LeftParen]))(
          Doc.char('('))
      }
      opener.map { scopeOpening =>
        val leading = leadingComments(term, indent, lineStart = true)
        val before = hardLineCount
        val (callee, dotIndent) = chainParts(call.fun, indent)
        val headerIndent = indent + dotIndent + 2
        val beforeHeader = hardLineCount
        val arrow = braced.flatMap {
          case Term.Block(List(function: (Term.FunctionTerm | Term.PolyFunction))) if bracedLambda(function) =>
            Some((bracedArrowHead(lambdaHeader(function, headerIndent)),
              (depth: Int) => lambdaStatements(function, function.body, depth)))
          case function: Term.PartialFunction =>
            inlineArgumentCase(function).map { branch =>
              (bracedArrowHead(caseHeader(branch, headerIndent) + arrowSuffix),
                (depth: Int) => caseContents(branch, depth))
            }
          case _ => None
        }
        val scope = arrow match {
          case Some((head, bodyAt)) =>
            val multilineHeader = hardLineCount != beforeHeader
            (depth: Int) => {
              val body = closeArrowArgument(head + arrowTail(2)(bodyAt(depth + 2)), multiline = multilineHeader)
              val remaining = tokenComments(arguments, arguments.endOffset, depth)
              body + (if (remaining.isEmpty) Doc.empty else hardLine + remaining + hardLine)
            }
          case None => (depth: Int) => tree(arguments, depth)
        }
        val flat = Option.when(hardLineCount == before) {
          val doc = callee.flatten
          val preview = doc + arrow.fold(scopeOpening)(_._1.flatten)
          (doc, preview.maxWidth)
        }
        CallLayout(leading, callee, flat, dotIndent, scope)
      }
    }

    private def assignmentHeaderWidth(header: Doc, indent: Int): Int = {
      val prefix = (Doc.spaces(indent) + (header + assignmentSeparator).nested(indent)).render(options.columns)
      prefix.length - prefix.lastIndexOf('\n') - 1
    }

    private def argumentAssignmentBody(block: Term.Block, indent: Int): Doc = {
      val owner = block.parent.getOrElse(block)
      var index = commentIndex
      while (index < commentTokens.length && commentTokens(index).end <= block.endOffset) index += 1
      val footer = index < commentTokens.length && commentTokens(index).end <= owner.endOffset
      if (footer) Doc.space + parenthesized {
        val contents = tree(block, indent + 2, lineStart = true)
        contents + hardLine + tokenComments(owner, owner.endOffset, indent + 2)
      }
      else Doc.space + tree(block, indent)
    }

    private def namedAssignmentBody(value: Tree, indent: Int): Doc = {
      val before = hardLineCount
      // Sequence patterns cannot be parenthesized or start an indentation region.
      val repeated = value.isInstanceOf[Pat.Repeated]
      val leading = leadingComments(value, if (repeated) indent else indent + 2, lineStart = true)
      if (hardLineCount == before || repeated) {
        val body = tree(value, indent)
        (if (leading.isEmpty) Doc.space else hardLine + leading) + body
      } else {
        val body = leading + tree(value, indent + 2)
        val owner = value.parent.getOrElse(value)
        val footer = tokenComments(owner, owner.endOffset, indent + 2)
        val contents = if (footer.isEmpty) body else body + hardLine + footer
        Doc.space + parenthesized(contents, multiline = true)
      }
    }

    @annotation.tailrec
    private def namedRhsStart(value: Tree): Tree = value match {
      case pattern: Pat.Var => namedRhsStart(pattern.name)
      case pattern: Pat.Typed => namedRhsStart(pattern.lhs)
      case pattern: Pat.Bind => namedRhsStart(pattern.lhs)
      case pattern: Pat.Alternative => namedRhsStart(pattern.lhs)
      case pattern: Pat.Extract => namedRhsStart(pattern.fun)
      case pattern: Pat.ExtractInfix => namedRhsStart(pattern.lhs)
      case pattern: Pat.Repeated => namedRhsStart(pattern.name)
      case pattern: Pat.Interpolate => namedRhsStart(pattern.prefix)
      case pattern: Pat.Macro => namedRhsStart(pattern.body)
      case variable: Type.Var => namedRhsStart(variable.name)
      case lambda: Type.AnonymousLambda => namedRhsStart(lambda.tpe)
      case macroType: Type.Macro => namedRhsStart(macroType.body)
      case _ => value
    }

    private def rhsCommentNeedsBlock(value: Term, canonical: Term): Boolean = value match {
      case block: Term.Block if !(block eq canonical) && commentIndex < commentTokens.length => {
        val end = block.endOffset
        var index = commentIndex
        while (index < commentTokens.length && commentTokens(index).end <= end) index += 1
        val footer = index > commentIndex && commentTokens(index - 1).start >= canonical.endOffset &&
          commentTokens(index - 1).text.startsWith("//")
        val owner: Tree = block.parent match {
          case Some(assignment: Term.Assign) if assignment.rhs eq block => assignment
          case Some(parameter: Term.Param) if parameter.default.exists(_ eq block) => parameter
          case _ => block
        }
        footer && (owner.origin match {
          case parsed: Origin.ParsedPartial => {
            val following = parsed.allInputTokens().iterator.drop(parsed.endTokenIdx)
              .filterNot(_.isInstanceOf[Token.Trivia])
            following.nextOption().exists(_.isInstanceOf[Token.Comma]) && following.nextOption().exists {
              case _: Token.RightParen | _: Token.RightBracket | _: Token.RightBrace => false
              case _ => true
            }
          }
          case _ => false
        })
      }
      case _ => false
    }

    private def closeArrowArgument(body: => Doc, multiline: Boolean = false): Doc = {
      val before = hardLineCount
      val contents = body
      if (multiline || hardLineCount != before) contents + hardLine + Doc.char('}')
      else (contents + softCloseBrace).grouped
    }

    private def bracedArrowHead(header: => Doc): Doc = {
      val before = hardLineCount
      val contents = header
      val opening = if (hardLineCount != before) hardLine else Doc.space
      Doc.char('{') + (opening + contents).nested(2)
    }

    private def arrowBody(header: => Doc, braced: Boolean = false)(body: => Doc): Doc = {
      if (braced) closeArrowArgument(bracedArrowHead(header) + arrowTail(2)(body))
      else header + arrowTail(2)(body)
    }

    private def templateHeader(value: Template, indent: Int): Doc = {
      val givenDefinition = value.parent.collect { case definition: Defn.Given => definition }
      val isAnonymous = value.parent.exists(_.isInstanceOf[Term.NewAnonymous])
      val ancestry = value.earlyClause.toVector ++ value.inits.iterator.toVector
      val inherited = if (ancestry.isEmpty) Doc.empty
      else {
        val keyword = if (givenDefinition.nonEmpty || isAnonymous) Doc.empty else Doc.text("extends")
        val parents = ancestry.iterator.zipWithIndex.map { case (parent, index) =>
          if (keyword.isEmpty && index == 0) tree(parent, indent) else spaced(parent, indent)
        }.toVector
        keyword + Doc.intercalate(Doc.text(" with"), parents)
      }
      val derived = value.derives.iterator.map(spaced(_, indent)).toVector
      val derives = if (derived.isEmpty) Doc.empty
      else Doc.text("derives") + Doc.intercalate(Doc.comma, derived)
      val withBody = givenDefinition.exists(definition => !givenUsesArrows(definition.paramClauseGroups))
      Doc.spread(Vector(inherited, derives, if (withBody) Doc.text("with") else Doc.empty).filter(_.nonEmpty))
    }

    private def templateSuffix(value: Template, indent: Int): Doc = {
      val inherited = value.earlyClause.nonEmpty || value.inits.nonEmpty
      val (header, commented) = if (inherited) {
        (arrowTail(2) {
          leadingComments(value, indent + 2, lineStart = true) + templateHeader(value, indent + 2)
        }, false)
      } else {
        val leading = leadingComments(value, indent, lineStart = true)
        (leading + templateHeader(value, indent), leading.nonEmpty)
      }
      val body = tree(value.body, indent)
      if (inherited) header + (if (body.isEmpty) Doc.empty else Doc.space + body)
      else {
        val contents = Doc.spread(Vector(header, body).filter(_.nonEmpty))
        if (contents.isEmpty) Doc.empty else (if (commented) hardLine else Doc.space) + contents
      }
    }

    private def methodSignature(
        name: Term.Name,
        groups: Iterable[Member.ParamClauseGroup],
        result: Option[Type],
        indent: Int,
    ): Doc = {
      val spelling = LeafPrinter.name(name)
      val nameDoc = spaced(name, indent, (_, _) => Doc.text(spelling))
      val clauses = ArrayBuffer.empty[Doc]
      var prefix = Doc.empty
      groups.iterator.foreach { group =>
        prefix = prefix + leadingComments(group, indent) + tree(group.tparamClause, indent)
        group.paramClauses.iterator.foreach { parameters =>
          val leading = leadingComments(parameters, indent)
          val contents = clause(parameters, parameters.values, "(", ")", indent,
            parameters.mod.fold(Doc.empty)(mod => tree(mod, indent + 2, lineStart = true)), grouped = false)
          clauses += prefix + leading + contents
          prefix = Doc.empty
        }
      }
      var suffix = prefix + result.fold(Doc.empty) { value =>
        val colon = if (groups.isEmpty && !spelling.endsWith("`") &&
          name.value.lastOption.exists(isOperatorPart)) Doc.text(" :") else Doc.char(':')
        colon + arrowTail(2)(tree(value, indent + 2, lineStart = true))
      }
      var index = clauses.length - 1
      while (index >= 0) {
        suffix = (clauses(index) + suffix).grouped
        index -= 1
      }
      nameDoc + suffix
    }

    private def firstCommentLine(before: Int): Option[Int] = {
      if (commentIndex < commentTokens.length && commentTokens(commentIndex).end <= before) {
        Some(commentTokens(commentIndex).pos.startLine)
      } else None
    }

    private def leadingComments(value: Tree, indent: Int, lineStart: Boolean = false): Doc = {
      if (commentIndex >= commentTokens.length) return Doc.empty
      val commentsInsideBraces = value match {
        case _: Pkg.Body | _: Ctor.Block | _: Term.EnumeratorsBlock => true
        case _: Term.ArgClause | _: Type.ArgClause | _: Term.ParamClause | _: Type.ParamClause |
            _: Member.ParamClauseGroup | _: Ctor.Primary => true
        case term: (Term.Block | Term.PartialFunction) if term.parent.exists {
            case arguments: Term.ArgClause => openingToken(arguments).exists(_.isInstanceOf[Token.LeftParen])
            case _ => false
          } => false
        case _: Term.PartialFunction => true
        case block: Term.Block if expression(block) eq block => true
        case template: Template if template.earlyClause.isEmpty && template.inits.isEmpty &&
            template.derives.isEmpty && canOmitEmptyBody(template.body) => true
        case body: Template.Body if canOmitEmptyBody(body) => true
        case block: Term.Block if bracedLambda(expression(block)) =>
          expression(block).isInstanceOf[Term.AnonymousFunction | Term.FunctionTerm | Term.PolyFunction |
            Term.PartialFunction]
        case function: (Term.AnonymousFunction | Term.FunctionTerm | Term.PolyFunction) => bracedLambda(function)
        case _ => false
      }
      if (value.begOffset < value.endOffset && !commentsInsideBraces) {
        val start = value match {
          case term: Term => expression(term)
          case _ => namedRhsStart(value)
        }
        val leading = comments(start.begOffset, indent, separate = true)
        if (leading.isEmpty || lineStart) leading else hardLine + leading
      } else Doc.empty
    }

    private def tree(value: Tree, indent: Int, lineStart: Boolean = false): Doc = {
      val leading = leadingComments(value, indent, lineStart)
      val body = syntax(value, indent)
      if (leading.isEmpty) body else leading + body
    }

    private def spaced(value: Tree, indent: Int, emit: (Tree, Int) => Doc = syntax): Doc = {
      val leading = leadingComments(value, indent, lineStart = true)
      val body = emit(value, indent)
      if (leading.nonEmpty) hardLine + leading + body
      else if (body.isEmpty) Doc.empty else Doc.space + body
    }

    private def comments(before: Int, indent: Int, separate: Boolean = false): Doc = {
      val start = commentIndex
      var result = Doc.empty
      while (commentIndex < commentTokens.length && commentTokens(commentIndex).end <= before) {
        val comment = commentTokens(commentIndex)
        if (commentIndex > start) {
          result = result + hardLine
        }
        result = result + verbatim(comment.text, indent)
        commentIndex += 1
      }
      if (separate && commentIndex > start) result + hardLine else result
    }

    private def statementGap(previousLine: Int, nextLine: Int, indent: Int, separate: Boolean = false): Doc = {
      if (separate || nextLine > previousLine + 1) hardLine.nested(-indent) + hardLine
      else hardLine
    }

    private def blankLineAround(value: Tree): Boolean = value match {
      case _: Defn.Def | _: Decl.Def | _: Defn.Macro | _: Ctor.Secondary |
          _: Member.Type | _: Defn.Object => true
      case _ => false
    }

    private def tokenComments(value: Tree, before: Int, indent: Int): Doc = {
      if (commentIndex < commentTokens.length && value.begOffset < value.endOffset) comments(before, indent)
      else Doc.empty
    }

    // Multiline token contents are data; structural nesting must not change them.
    private def verbatim(value: String, indent: Int): Doc = {
      if (value.indexOf('\n') < 0) Doc.text(value)
      else Doc.intercalate(hardLine, value.split("\n", -1).iterator.map(Doc.text).toVector).nested(-indent)
    }

    private def hasStripMargin(value: Tree): Boolean = value.parent.exists {
      case select: Term.Select if (select.qual eq value) && select.name.value == "stripMargin" =>
        !select.parent.exists {
          case call: Term.Apply => call.fun eq select
          case call: Term.ApplyType => call.fun eq select
          case _ => false
        }
      case _ => false
    }

    private def marginText(value: String): Doc = {
      val lines = value.split("\n", -1).toVector
      Doc.text(lines.head) + Doc.cat(lines.tail.map { line =>
        val content = line.dropWhile(_ <= ' ')
        val normalized = if (content.startsWith("|")) content else "|" + line
        hardLine + Doc.text(normalized)
      })
    }

    private def canAlignMargins(value: Term.Interpolate): Boolean = {
      // A leading splice can supply its own margin; adding one would change its value.
      val markedSplices = value.parts.init.forall { part =>
        val text = part.value.toString
        val newline = text.lastIndexOf('\n')
        newline < 0 || !text.substring(newline + 1).forall(_ <= ' ')
      }
      // Alignment must not shift verbatim data inside interpolation expressions.
      markedSplices && value.args.forall(_.tokens.forall { token =>
        token.text.indexOf('\n') < 0 ||
          (token.isInstanceOf[Token.Trivia] && !token.isInstanceOf[Token.Comment])
      })
    }

    private def statements(
        container: Tree,
        values: Iterable[Tree],
        indent: Int,
        emit: (Tree, Int) => Doc = expressionLine,
    ): Doc = {
      val docs = ArrayBuffer.empty[Doc]
      val iterator = values.iterator.buffered
      var previousLine = -1
      var previousDeclaration = false
      var boundaryComments = false
      while (iterator.hasNext) {
        val value = iterator.next()
        val declaration = blankLineAround(value)
        val firstLine = firstCommentLine(value.begOffset).getOrElse(value.pos.startLine)
        val before = comments(value.begOffset, indent, separate = true)
        val beforeBodyComments = commentIndex
        val body = value match {
          case function: Term.FunctionLike if iterator.hasNext && argumentClause(function).nonEmpty =>
            parenthesized(emit(function, indent + 2))
          case _ => emit(value, indent)
        }
        val afterBodyComments = commentIndex
        val gap = if (previousLine < 0) Doc.empty
        else if (boundaryComments) hardLine
        else statementGap(previousLine, firstLine, indent, previousDeclaration || declaration)
        val needsSeparator = iterator.hasNext && ((value, iterator.head) match {
          case (definition: Defn.Val, next: Term)
              if expression(next).isInstanceOf[Lit.Unit] && expression(definition.rhs).isInstanceOf[Lit.Unit] => false
          case (_, next) => continuesPreviousStatement(next)
        })
        val semicolon = value.origin match {
          case parsed: Origin.ParsedPartial if needsSeparator =>
            parsed.allInputTokens().findNot(_.isInstanceOf[Token.Trivia], parsed.endTokenIdx, Int.MaxValue)
              .collect { case token: Token.Semicolon => token }
          case _ => None
        }
        val endLine = semicolon.fold(value.pos.endLine)(_.pos.endLine)
        val following = if (needsSeparator) comments(iterator.head.begOffset, indent) else Doc.empty
        val followingLeading = following.nonEmpty && semicolon.forall(_.start < commentTokens(afterBodyComments).start)
        val followingGap = if (followingLeading) {
          statementGap(endLine, commentTokens(afterBodyComments).pos.startLine, indent,
            declaration || blankLineAround(iterator.head))
        } else if (following.isEmpty) Doc.empty else hardLine
        val separator = if (!needsSeparator) Doc.empty
        else if (afterBodyComments > beforeBodyComments || (following.nonEmpty && !followingLeading)) {
          hardLine + Doc.char(';')
        } else Doc.char(';')
        if (gap.nonEmpty) docs += gap
        if (before.nonEmpty) docs += before
        if (body.nonEmpty) docs += body
        if (followingLeading && separator.nonEmpty) docs += separator
        if (followingGap.nonEmpty) docs += followingGap
        if (following.nonEmpty) docs += following
        if (!followingLeading && separator.nonEmpty) docs += separator
        previousLine = if (commentIndex == 0) endLine
        else endLine.max(commentTokens(commentIndex - 1).pos.endLine)
        previousDeclaration = declaration
        boundaryComments = followingLeading
      }
      val remaining = tokenComments(container, container.endOffset, indent)
      val gap = if (previousLine < 0) Doc.empty
      else if (remaining.nonEmpty) hardLine else Doc.empty
      if (gap.nonEmpty) docs += gap
      if (remaining.nonEmpty) docs += remaining
      Doc.cat(docs)
    }

    private def continuesPreviousStatement(value: Tree): Boolean = value match {
      case term: Term => expression(term) match {
        case _: Term.Block | _: Term.PartialFunction | _: Term.Tuple | _: Lit.Unit |
            _: Term.PolyFunction | _: Term.ApplyUnary => true
        case literal: Lit => LeafPrinter.isNegative(literal)
        case function: Term.FunctionTerm => function.paramClause.values match {
          case parameter :: Nil =>
            !function.paramClause.mod.exists(_.isInstanceOf[Mod.Implicit]) &&
              (parameter.decltpe.nonEmpty || function.paramClause.mod.nonEmpty)
          case _ => true
        }
        case call: Term.Apply =>
          TreeSyntacticGroup.groupNeedsParens(TreeSyntacticGroup.SimpleExpr1, syntacticGroup(call.fun)) ||
            continuesPreviousStatement(call.fun)
        case call: Term.ApplyType =>
          TreeSyntacticGroup.groupNeedsParens(TreeSyntacticGroup.SimpleExpr1, syntacticGroup(call.fun)) ||
            continuesPreviousStatement(call.fun)
        case select: Term.Select => expression(select.qual) match {
          case creation: Term.New if creation.init.argClauses.isEmpty => true
          case _ =>
            TreeSyntacticGroup.groupNeedsParens(TreeSyntacticGroup.SimpleExpr, syntacticGroup(select.qual)) ||
              continuesPreviousStatement(select.qual)
        }
        case infix: Term.ApplyInfix => expression(infix.lhs) match {
          case inner: Term.ApplyInfix =>
            TreeSyntacticGroup.opNeedsParens(infix.op, inner.op, true) || continuesPreviousStatement(infix.lhs)
          case _ => continuesPreviousStatement(infix.lhs)
        }
        case anonymous: Term.AnonymousFunction => continuesPreviousStatement(anonymous.body)
        case ascribed: Term.Ascribe =>
          TreeSyntacticGroup.groupNeedsParens(TreeSyntacticGroup.PostfixExpr, syntacticGroup(ascribed.expr)) ||
            continuesPreviousStatement(ascribed.expr)
        case annotated: Term.Annotate =>
          TreeSyntacticGroup.groupNeedsParens(TreeSyntacticGroup.PostfixExpr, syntacticGroup(annotated.expr)) ||
            continuesPreviousStatement(annotated.expr)
        case eta: Term.Eta =>
          TreeSyntacticGroup.groupNeedsParens(TreeSyntacticGroup.SimpleExpr1, syntacticGroup(eta.expr)) ||
            continuesPreviousStatement(eta.expr)
        case _ => false
      }
      case _ => false
    }

    private def braces(body: Doc, compactEmpty: Boolean = true): Doc = {
      if (body.isEmpty && compactEmpty) Doc.text("{}")
      else if (body.isEmpty) Doc.char('{') + hardLine + Doc.char('}')
      else Doc.char('{') + (hardLine + body).nested(2) + hardLine + Doc.char('}')
    }

    private def blockContents(value: Tree, indent: Int): Doc = value match {
      case block: Term.Block if !(expression(block) eq block) =>
        statements(block, block.stats, indent, blockContents)
      case block: Term.Block => statements(block, block.stats, indent)
      case _ => expressionLine(value, indent)
    }

    private def requiredBlock(value: Term, indent: Int, compactEmpty: Boolean = true): Doc =
      braces(blockContents(value, indent + 2), compactEmpty)

    private def clause(
        container: Tree,
        values: Iterable[Tree],
        left: String,
        right: String,
        indent: Int,
        prefix: => Doc = Doc.empty,
        grouped: Boolean = true,
        forceMultiline: Boolean = false,
    ): Doc = {
      val before = hardLineCount
      val beforeComments = commentIndex
      val leading = prefix
      val docs = ArrayBuffer.empty[Doc]
      var firstComment = false
      values.iterator.foreach { value =>
        val leading = leadingComments(value, indent + 2, lineStart = true)
        if (docs.isEmpty) firstComment = leading.nonEmpty
        docs += leading + tree(value, indent + 2, lineStart = true)
      }
      val remaining = tokenComments(container, container.endOffset, indent + 2)
      val multiline = forceMultiline || remaining.nonEmpty || commentIndex != beforeComments ||
        (left == "(" && hardLineCount != before)
      val separator = if (multiline) Doc.comma + hardLine else commaLine
      val valuesDoc = Doc.intercalate(separator, docs)
      val body = if (leading.isEmpty) valuesDoc
      else leading + (if (docs.isEmpty) Doc.empty else if (firstComment) hardLine else Doc.space) + valuesDoc
      if (!multiline && left == "(" && right == ")" && docs.isEmpty && leading.isEmpty) emptyParentheses
      else if (!multiline) {
        if (grouped) body.tightBracketBy(Doc.text(left), Doc.text(right))
        else Doc.text(left) + (Doc.lineBreak + body).nested(2) + Doc.lineBreak + Doc.text(right)
      }
      else {
        val contents = Doc.intercalate(hardLine, Vector(body, remaining).filter(_.nonEmpty))
        Doc.text(left) + (hardLine + contents).nested(2) + hardLine + Doc.text(right)
      }
    }

    private def syntacticGroup(value: Tree): TreeSyntacticGroup = {
      import TreeSyntacticGroup.*
      value match {
        case term: Term if !(expression(term) eq term) => syntacticGroup(expression(term))
        case _: Name => Path
        case _: Lit => Literal
        case _: Term.Select | _: Term.This | _: Term.Super => Path
        case matching: Term.SelectMatch if matching.mods.isEmpty => Path
        case infix: Term.ApplyInfix => new InfixExpr(infix)
        case _: Term.ApplyUnary => PrefixExpr
        case _: Term.SelectPostfix | _: Term.Eta => PostfixExpr
        case function: (Term.AnonymousFunction | Term.FunctionTerm | Term.PolyFunction)
            if bracedLambda(function) => SimpleExpr
        case _: Term.AnonymousFunction | _: Term.FunctionTerm | _: Term.PolyFunction => Expr
        case _: Term.Block | _: Term.PartialFunction | _: Term.New | _: Term.NewAnonymous |
            _: Term.MacroLike => SimpleExpr
        case _: Term.Ref | _: Term.Apply | _: Term.ApplyType | _: Term.Tuple |
            _: Term.Interpolate | _: Term.Xml | _: Term.Placeholder => SimpleExpr1
        case _: Term => Expr1
        case infix: Type.ApplyInfix => new InfixTyp(infix)
        case lambda: Type.AnonymousLambda => syntacticGroup(lambda.tpe)
        case capturing: Type.Capturing => syntacticGroup(capturing.tpe)
        case _: Type.FunctionLikeType | _: Type.PolyFunction | _: Type.Lambda | _: Type.Match => Typ
        case _: Type.Refine => RefineTyp
        case _: Type.With => WithTyp
        case _: Type.Annotate => AnnotTyp
        case _: Type => SimpleTyp
        case infix: Pat.ExtractInfix => new InfixPat(infix)
        case _: Pat.Alternative => Pattern
        case _: Pat.Typed => Pattern1
        case _: Pat.Bind => Pattern2
        case _: Pat.Given => AnyPattern3
        case _ => SimplePattern
      }
    }

    private def inContext(context: TreeSyntacticGroup, value: Tree, indent: Int): Doc = {
      val canonical = value match {
        case term: Term => expression(term)
        case _ => value
      }
      val negativeLiteral = canonical match {
        case literal: Lit => LeafPrinter.isNegative(literal) &&
          (context.categories & TreeSyntacticGroup.Expr.categories) != 0 &&
          context.precedence > TreeSyntacticGroup.PrefixExpr.precedence
        case _ => false
      }
      if (negativeLiteral || TreeSyntacticGroup.groupNeedsParens(context, syntacticGroup(value))) {
        parenthesized {
          val body = tree(value, indent + 2, lineStart = true)
          body + parenthesisFooter(value, indent + 2)
        }
      } else tree(value, indent)
    }

    private def parenthesisFooter(value: Tree, indent: Int): Doc = value match {
      case loop: Term.ForYield if commentIndex < commentTokens.length => loop.origin match {
          case parsed: Origin.ParsedPartial =>
            parsed.allInputTokens().findNot(_.isInstanceOf[Token.Trivia], parsed.endTokenIdx, Int.MaxValue) match {
              case Some(close: Token.RightParen) =>
                firstCommentLine(close.start).fold(Doc.empty) { line =>
                  val trailing = comments(close.start, indent)
                  statementGap(value.pos.endLine, line, indent) + trailing
                }
              case _ => Doc.empty
            }
          case _ => Doc.empty
        }
      case _ => Doc.empty
    }

    private def atom(value: Tree, indent: Int): Doc = {
      val context = value match {
        case _: Term => TreeSyntacticGroup.SimpleExpr1
        case _: Type => TreeSyntacticGroup.SimpleTyp
        case _: Pat => TreeSyntacticGroup.SimplePattern
        case _ => TreeSyntacticGroup.SimpleExpr1
      }
      inContext(context, value, indent)
    }

    private def selectQualifier(value: Term, indent: Int): Doc = expression(value) match {
      case creation: Term.New if creation.init.argClauses.isEmpty =>
        parenthesized(tree(value, indent + 2))
      case _ => inContext(TreeSyntacticGroup.SimpleExpr, value, indent)
    }

    @annotation.tailrec
    private def expressionChain(value: Tree): Boolean = value.parent match {
      case Some(select: Term.Select) if select.qual eq value => expressionChain(select)
      case Some(call: Term.Apply) if call.fun eq value => expressionChain(call)
      case Some(call: Term.ApplyType) if call.fun eq value => expressionChain(call)
      case Some(block: Term.Block) if !(expression(block) eq block) => expressionChain(block)
      case Some(_: Type | _: Pkg | _: Importer | _: Pat) => false
      case Some(parent: Tree.WithPats) if parent.pats.exists(_ eq value) => false
      case Some(parent: CaseTree) if parent.pat eq value => false
      case Some(parent: Enumerator.Assign) if parent.pat eq value => false
      case _ => true
    }

    private def chain(value: Term, indent: Int): Doc = {
      val before = hardLineCount
      val (doc, dotIndent) = chainParts(value, indent)
      if (dotIndent != 0 && hardLineCount == before) doc.grouped else doc
    }

    private def chainParts(value: Term, indent: Int): (Doc, Int) = {
      val suffixes = ArrayBuffer.empty[Term]
      var base = value
      var selections = 0
      var scanning = true
      while (scanning) {
        base match {
          case block: Term.Block if !(expression(block) eq block) => base = expression(block)
          case select: Term.Select =>
            suffixes += select
            selections += 1
            base = select.qual
          case call: Term.Apply =>
            suffixes += call
            base = call.fun
          case call: Term.ApplyType =>
            suffixes += call
            base = call.fun
          case _ => scanning = false
        }
      }
      val breakable = selections >= 2 && expressionChain(value)
      val baseDoc = suffixes.lastOption match {
          case Some(_: Term.Select) => selectQualifier(base, indent)
          case _ => atom(base, indent)
      }
      var prefix = baseDoc
      var continuation = Doc.empty
      var selected = false
      var index = suffixes.length - 1
      while (index >= 0) {
        val suffix = suffixes(index)
        val suffixIndent = if (breakable && (selected || suffix.isInstanceOf[Term.Select])) indent + 2 else indent
        val doc = suffix match {
          case select: Term.Select =>
            selected = true
            val commentStart = commentIndex
            val leading = comments(select.name.begOffset, suffixIndent)
            val commented = commentIndex > commentStart
            val name = tree(select.name, suffixIndent)
            (if (commented) hardLine else if (breakable) Doc.lineBreak else Doc.empty) +
              leading + (if (commented) hardLine else Doc.empty) + Doc.char('.') + name
          case call: Term.Apply => tree(call.argClause, suffixIndent)
          case call: Term.ApplyType => tree(call.targClause, suffixIndent)
          case _ => throw FormatError.Unsupported(suffix.productPrefix)
        }
        if (breakable && selected) continuation = continuation + doc
        else prefix = prefix + doc
        index -= 1
      }
      val result = if (continuation.isEmpty) prefix else prefix + continuation.nested(2)
      (result, if (breakable) 2 else 0)
    }

    private def infixOperand(outer: Member.Infix, inner: Tree, indent: Int): Doc = {
      val leading = leadingComments(inner, indent, lineStart = true)
      val canonical = inner match {
        case term: Term => expression(term)
        case _ => inner
      }
      val body = canonical match {
        case infix: Term.ApplyInfix if outer.isInstanceOf[Term.ApplyInfix] =>
          if (TreeSyntacticGroup.opNeedsParens(outer.op, infix.op, outer.lhs eq inner)) {
            parenthesized(tree(inner, indent + 2))
          } else tree(inner, indent)
        case _ => inContext(syntacticGroup(outer), inner, indent)
      }
      leading + body
    }

    private def infix(value: Term.ApplyInfix, indent: Int): Doc = {
      val work = ArrayBuffer[InfixWork](InfixWork.Visit(value, indent, chained = false))
      val docs = ArrayBuffer.empty[Doc]
      while (work.nonEmpty) work.remove(work.length - 1) match {
        case InfixWork.Visit(block: Term.Block, indent, chained) if !(expression(block) eq block) =>
          val leading = leadingComments(block, indent, lineStart = true)
          work += InfixWork.CloseBlock(block, indent, leading)
          work += InfixWork.Visit(block.stats.head.asInstanceOf[Term], indent, chained)
        case InfixWork.Visit(call: Term.ApplyInfix, indent, chained) =>
          val leading = leadingComments(call, indent, lineStart = true)
          work += InfixWork.Operator(call, indent, leading, hardLineCount, chained)
          work += InfixWork.Operand(call, call.lhs, indent)
        case InfixWork.Visit(other, indent, _) => docs += tree(other, indent, lineStart = true)
        case InfixWork.Operand(outer, inner, indent) => expression(inner) match {
          case call: Term.ApplyInfix =>
            val parens = TreeSyntacticGroup.opNeedsParens(outer.op, call.op, outer.lhs eq inner)
            if (parens) work += InfixWork.CloseParens(hardLineCount)
            val chained = !parens && (outer.lhs ne inner) && outer.op.value == "*:" && call.op.value == "*:"
            work += InfixWork.Visit(inner, if (parens) indent + 2 else indent, chained)
          case _ => docs += infixOperand(outer, inner, indent)
        }
        case InfixWork.Operator(call, indent, leading, before, chained) =>
          val lhs = docs.remove(docs.length - 1)
          val op = spaced(call.op, indent)
          val types = if (call.targClause.values.isEmpty) Doc.empty else tree(call.targClause, indent)
          val attached = bracedCall(call.argClause)
          work += InfixWork.Finish(leading, before, commentIndex, lhs, op, types, chained, call.op.value == "*:", attached)
          val rhsIndent = if (chained || attached) indent else indent + 2
          call.argClause.values match {
            case _ if attached => docs += tree(call.argClause, rhsIndent)
            case single :: Nil if expression(single).isInstanceOf[Lit.Unit | Term.Tuple] =>
              docs += tree(call.argClause, rhsIndent)
            case single :: Nil if parenthesizedInfixArgument(call.argClause, single) &&
                expression(single).isInstanceOf[Term.ApplyInfix] =>
              work += InfixWork.CloseArgument(call.argClause, rhsIndent + 2, hardLineCount)
              work += InfixWork.Visit(single, rhsIndent + 2, chained = false)
            case single :: Nil if call.argClause.mod.isEmpty && !parenthesizedInfixArgument(call.argClause, single) =>
              work += InfixWork.Operand(call, single, rhsIndent)
            case _ => docs += tree(call.argClause, rhsIndent)
          }
        case InfixWork.Finish(leading, before, beforeComments, lhs, op, types, chained, chainOperator, attached) =>
          val rhs = docs.remove(docs.length - 1)
          val multiline = hardLineCount != before
          val separator = if (attached) Doc.empty
          else if (commentIndex != beforeComments) hardLine
          else if (multiline && !chainOperator) Doc.lineOrSpace else Doc.line
          val tail = separator + rhs
          val result = lhs + op + types + (if (chained || attached) tail else tail.nested(2))
          val body = if (multiline || chained) result else result.grouped
          docs += (if (leading.isEmpty) body else leading + body)
        case InfixWork.CloseBlock(block, indent, leading) =>
          val contents = docs.remove(docs.length - 1)
          val body = statements(block, block.stats, indent, (_, _) => contents)
          docs += (if (leading.isEmpty) body else leading + body)
        case InfixWork.CloseParens(before) =>
          val contents = docs.remove(docs.length - 1)
          docs += parenthesized(contents, hardLineCount != before)
        case InfixWork.CloseArgument(value, indent, before) =>
          val contents = docs.remove(docs.length - 1)
          val footer = tokenComments(value, value.endOffset, indent)
          val body = if (footer.isEmpty) contents else contents + hardLine + footer
          docs += parenthesized(body, hardLineCount != before)
      }
      docs.head
    }

    private def openingToken(value: Tree): Option[Token] = value.origin match {
      case parsed: Origin.ParsedPartial if !parsed.isEmpty =>
        Some(parsed.allInputTokens()(parsed.begTokenIdx))
      case _ => None
    }

    private def argumentBlock(block: Term.Block): Boolean =
      openingToken(block).exists {
        case _: Token.LeftBrace => block.stats.headOption.forall(_.begOffset > block.begOffset)
        case _: Token.Colon => true
        case _ => false
      } && argumentClause(block).nonEmpty

    private def lambdaBraceOwner(value: Tree): Option[Term.Block] = value.parent.collect {
      case block: Term.Block if block.stats.lengthCompare(1) == 0 && (block.stats.head eq value) &&
          argumentBlock(block) => block
    }

    private def bracedLambda(value: Tree): Boolean = value match {
      case function: Term.FunctionTerm =>
        lambdaBraceOwner(function).nonEmpty ||
          (argumentClause(function).isEmpty && expression(function.body).isInstanceOf[Term.Block])
      case function: Term.AnonymousFunction =>
        lambdaBraceOwner(function).nonEmpty ||
          (argumentClause(function).isEmpty && expression(function.body).isInstanceOf[Term.Block])
      case function: Term.PolyFunction => lambdaBraceOwner(function).nonEmpty
      case _ => false
    }

    @annotation.tailrec
    private def argumentClause(value: Tree): Option[Term.ArgClause] = value.parent match {
      case Some(arguments: Term.ArgClause) => Some(arguments)
      case Some(block: Term.Block) => argumentClause(block)
      case Some(assignment: Term.Assign) if assignment.rhs eq value => argumentClause(assignment)
      case Some(ascribed: Term.Ascribe) if ascribed.expr eq value => argumentClause(ascribed)
      case Some(annotated: Term.Annotate) if annotated.expr eq value => argumentClause(annotated)
      case Some(function: Term.FunctionLike) if function.body eq value => argumentClause(function)
      case Some(function: Term.AnonymousFunction) if function.body eq value => argumentClause(function)
      case _ => None
    }

    private def bracedCall(arguments: Term.ArgClause): Boolean =
      arguments.values match {
        case (_: Term.Block | _: Term.PartialFunction) :: Nil =>
          openingToken(arguments).exists(_.isInstanceOf[Token.LeftBrace | Token.Colon])
        case _ => false
      }

    private def bracedArgumentSpace(argument: Term): Doc = argument match {
      case Term.Block(List(_: Term.FunctionLike | _: Term.AnonymousFunction)) => Doc.empty
      case _: Term.PartialFunction => Doc.empty
      case _ => Doc.space
    }

    private def parenthesizedInfixArgument(arguments: Term.ArgClause, value: Term): Boolean =
      arguments.begOffset < value.begOffset && openingToken(arguments).exists(_.isInstanceOf[Token.LeftParen])

    private def renderArgumentBlock(block: Term.Block, indent: Int): Doc = block.stats match {
      case (function: (Term.FunctionLike | Term.AnonymousFunction)) :: Nil => tree(function, indent)
      case (_: Term) :: Nil =>
        val before = hardLineCount
        val beforeComments = commentIndex
        val contents = statements(block, block.stats, indent + 2)
        if (hardLineCount != before || commentIndex != beforeComments) braces(contents)
        else contents.bracketBy(Doc.char('{'), Doc.char('}'))
      case _ => braces(statements(block, block.stats, indent + 2))
    }

    private def packageNeedsBraces(pkg: Pkg): Boolean = {
      @annotation.tailrec
      def solePackage(tree: Tree): Boolean = tree.parent match {
        case Some(body: Pkg.Body) if body.stats.lengthCompare(1) == 0 => solePackage(body)
        case Some(parent: Pkg) => solePackage(parent)
        case Some(source: Source) => source.stats.lengthCompare(1) == 0
        case _ => false
      }
      !solePackage(pkg)
    }

    private def lambda(function: Term.FunctionTerm, indent: Int): Doc = {
      val braced = bracedLambda(function)
      val headerIndent = if (braced) indent + 2 else indent
      arrowBody(lambdaHeader(function, headerIndent), braced) {
        if (braced) lambdaStatements(function, function.body, indent + 2)
        else lambdaBody(function, indent + 2)
      }
    }

    private def lambdaHeader(value: Term.FunctionTerm | Term.PolyFunction, indent: Int): Doc = value match {
      case function: Term.FunctionTerm =>
        val parameters = if (function.paramClause.values.lengthCompare(1) == 0) {
          val parameter = function.paramClause.values.head
          if (function.paramClause.mod.isEmpty && parameter.decltpe.isEmpty) tree(parameter, indent, lineStart = true)
          else if (function.paramClause.mod.exists(_.isInstanceOf[Mod.Implicit])) {
            val keyword = function.paramClause.mod.map(tree(_, indent, lineStart = true)).getOrElse(Doc.empty)
            keyword + spaced(parameter, indent)
          } else tree(function.paramClause, indent, lineStart = true)
        } else tree(function.paramClause, indent, lineStart = true)
        val arrow = if (function.isInstanceOf[Term.ContextFunction]) " ?=>" else " =>"
        parameters + Doc.text(arrow)
      case function: Term.PolyFunction =>
        tree(function.tparamClause, indent, lineStart = true) + arrowSuffix
    }

    @annotation.tailrec
    private def insideArgumentBraces(value: Tree): Boolean = value.parent match {
      case Some(block: Term.Block) => argumentBlock(block)
      case Some(function: Term.FunctionLike) if function.body eq value => insideArgumentBraces(function)
      case _ => false
    }

    private def lambdaBody(function: Term.FunctionLike, indent: Int): Doc = function.body match {
      case block: Term.Block if !argumentBlock(block) && insideArgumentBraces(function) =>
        blockContents(block, indent)
      case body => expressionLine(body, indent)
    }

    @annotation.tailrec
    private def lambdaContainer(value: Tree): Tree = value.parent match {
      case Some(block: Term.Block) if block.stats.lengthCompare(1) == 0 && (block.stats.head eq value) =>
        if (argumentBlock(block)) block else lambdaContainer(block)
      case _ => value
    }

    private def lambdaStatements(function: Term, body: Term, indent: Int): Doc = {
      val values = body match {
        case block: Term.Block if block.stats.isEmpty && !argumentBlock(block) => Vector.empty
        case _ => Vector(body)
      }
      statements(lambdaContainer(function), values, indent, (value, depth) => value match {
        case block: Term.Block if argumentBlock(block) => tree(block, depth, lineStart = true)
        case _ if function.isInstanceOf[Term.PolyFunction] => tree(value, depth, lineStart = true)
        case _ => blockContents(value, depth)
      })
    }

    private def caseHeader(branch: Case, indent: Int): Doc = {
      val pattern = spaced(branch.pat, indent)
      val condition = branch.cond.map(value =>
        spaced(value, indent, (tree, depth) => inContext(TreeSyntacticGroup.PostfixExpr, tree, depth)))
      val guard = condition.fold(Doc.empty)(doc => Doc.text(" if") + doc)
      Doc.text("case") + pattern + guard
    }

    private def caseContents(branch: Case, indent: Int): Doc = {
      val body = tree(branch.body, indent, lineStart = true)
      val argument = branch.parent.collect {
        case function: Term.PartialFunction if function.cases.lengthCompare(1) == 0 &&
            argumentClause(function).nonEmpty => function
      }
      branch.origin match {
        case parsed: Origin.ParsedPartial if commentIndex < commentTokens.length =>
          parsed.allInputTokens().findNot(_.isInstanceOf[Token.Trivia], parsed.endTokenIdx, Int.MaxValue) match {
            case Some(boundary: (Token.KwCase | Token.RightBrace | Token.EOF)) =>
              val endOffset = argument.fold(boundary.start)(_.endOffset)
              var end = commentIndex
              while (end < commentTokens.length && commentTokens(end).end <= endOffset) end += 1
              if (end == commentIndex) body
              else {
                val trailing = comments(commentTokens(end - 1).end, indent)
                val gap = if (body.isEmpty) Doc.empty else hardLine
                body + gap + trailing
              }
            case _ => body
          }
        case _ => body
      }
    }

    private def inlineArgumentCase(function: Term.PartialFunction): Option[Case] = function.cases match {
      case branch :: Nil if argumentClause(function).nonEmpty && firstCommentLine(branch.begOffset).isEmpty &&
          !branch.body.isInstanceOf[Term.SubMatch] => Some(branch)
      case _ => None
    }

    private def caseArgument(branch: Case, indent: Int): Doc =
      arrowBody(caseHeader(branch, indent + 2) + arrowSuffix, braced = true) {
        caseContents(branch, indent + 2)
      }

    @annotation.tailrec
    private def caseBodyBlock(value: Tree): Boolean = value.parent match {
      case Some(branch: Case) => branch.body eq value
      case Some(parent: Term.Block) if parent.stats.lengthCompare(1) == 0 &&
          (parent.stats.head eq value) => caseBodyBlock(parent)
      case _ => false
    }

    private def canOmitEmptyBody(body: Template.Body): Boolean =
      body.selfOpt.isEmpty && body.stats.isEmpty && body.parent.exists {
        case template: Template => template.parent.exists {
          case _: Defn.Trait | _: Defn.Object => true
          case definition: Defn.Class => definition.mods.exists(_.isInstanceOf[Mod.Case])
          case _ => false
        }
        case _ => false
      }

    private def elseBody(value: Term, indent: Int): Doc = expression(value) match {
      case _: Term.If => tree(value, indent)
      case _ => requiredBlock(value, indent)
    }

    private def modifiers(values: Iterable[Mod], indent: Int, suffix: Doc = Doc.space): Doc = {
      val iterator = values.iterator
      if (!iterator.hasNext) Doc.empty
      else tree(iterator.next(), indent) + Doc.cat(iterator.map(spaced(_, indent)).toVector) + suffix
    }

    private def givenUsesArrows(groups: Iterable[Member.ParamClauseGroup]): Boolean = {
      val iterator = groups.iterator
      if (!iterator.hasNext) false
      else {
        val first = iterator.next()
        iterator.hasNext || first.paramClauses.exists(_.mod.isEmpty)
      }
    }

    private def givenSignature(mods: Iterable[Mod], name: Name, groups: Iterable[Member.ParamClauseGroup],
        result: Tree, indent: Int): Doc = {
      val prefix = modifiers(mods, indent, Doc.empty)
      val first = if (name.value.nonEmpty) name else groups.headOption.getOrElse(result match {
        case template: Template => template.inits.headOption.getOrElse(template)
        case _ => result
      })
      val leading = comments(first.begOffset, indent, separate = true)
      val label = tree(name, indent)
      val arrows = givenUsesArrows(groups)
      val parameters = if (arrows) {
        val docs = groups.iterator.map { group =>
          val types = tree(group.tparamClause, indent)
          val terms = group.paramClauses.iterator.map { parameters =>
            clause(parameters, parameters.values, "(", ")", indent)
          }.toVector
          Doc.intercalate(functionArrow, (types +: terms).filter(_.nonEmpty))
        }.toVector
        Doc.intercalate(functionArrow, docs) + Doc.text(" =>")
      } else Doc.cat(groups.iterator.map(tree(_, indent)).toVector)
      val signature = label + parameters
      val header = if (arrows) (if (label.isEmpty) Doc.empty else label + Doc.text(": ")) + parameters
      else signature + (if (signature.isEmpty) Doc.empty else Doc.char(':'))
      val body = spaced(result, indent, (value, depth) => value match {
        case _: Type.TypedParam | _: Type.FunctionLikeType | _: Type.ApplyInfix |
            _: Type.Refine | _: Type.Match | _: Type.Lambda | _: Type.PolyFunction =>
          parenthesized(tree(value, depth + 2, lineStart = true))
        case _ => syntax(value, depth)
      })
      prefix + (if (prefix.isEmpty) Doc.empty else if (leading.nonEmpty) hardLine else Doc.space) +
        leading + Doc.text("given") + (if (header.isEmpty) Doc.empty else Doc.space + header) + body
    }

    private def interpolationArgument(value: Tree, indent: Int): Doc = {
      val before = hardLineCount
      val beforeComments = commentIndex
      val contents = value match {
        case block: Term.Block => statements(block, block.stats, indent + 2)
        case _ => tree(value, indent + 2)
      }
      val trailingLineComment = commentIndex > beforeComments &&
        commentTokens(commentIndex - 1).text.startsWith("//")
      if (hardLineCount != before || trailingLineComment) braces(contents)
      else contents.tightBracketBy(Doc.char('{'), Doc.char('}'))
    }

    private def literalParts(parts: Iterable[Lit], arguments: Iterable[Tree], indent: Int, interpolated: Boolean,
        stripMargin: Boolean = false): Doc = {
      val expressions = arguments.iterator.map { argument =>
        if (interpolated) interpolationArgument(argument, indent)
        else argument match {
          case term: Term => requiredBlock(term, indent)
          case pattern => braces(tree(pattern, indent + 2))
        }
      }.toVector
      val docs = ArrayBuffer.empty[Doc]
      val iterator = parts.iterator
      var index = 0
      while (iterator.hasNext) {
        val text = iterator.next().value.toString
        val escaped = if (interpolated) text.replace("$", "$$") else text
        val splice = if (index < expressions.length) {
          (if (interpolated) Doc.char('$') else Doc.empty) + expressions(index)
        } else Doc.empty
        docs += (if (stripMargin) marginText(escaped) else verbatim(escaped, indent)) + splice
        index += 1
      }
      Doc.cat(docs)
    }

    private def syntax(value: Tree, indent: Int): Doc = value match {
      case _: Term.Anonymous | _: Type.AnonymousName => Doc.empty
      case name: Name => Doc.text(LeafPrinter.name(name))
      case literal: Lit =>
        val text = LeafPrinter.literal(literal)
        if (text.startsWith("\"\"\"") && hasStripMargin(literal)) {
          marginText(text).hang(if (text.startsWith("\"\"\"|")) 3 else 2)
        } else verbatim(text, indent)
      case _: Term => termSyntax(value, indent)
      case _: Type => typeSyntax(value, indent)
      case _: Pat => patternSyntax(value, indent)
      case _ => compoundSyntax(value, indent)
    }

    private def termSyntax(value: Tree, indent: Int): Doc = value match {
      case block: Term.Block if argumentBlock(block) => renderArgumentBlock(block, indent)
      case block: Term.Block if bracedLambda(expression(block)) &&
          expression(block).isInstanceOf[Term.AnonymousFunction | Term.FunctionTerm | Term.PolyFunction |
            Term.PartialFunction] => tree(expression(block), indent)
      case block: Term.Block if !(expression(block) eq block) =>
        statements(block, block.stats, indent, (value, depth) => tree(value, depth, lineStart = true))
      case block: Term.Block if caseBodyBlock(block) => statements(block, block.stats, indent)
      case block: Term.Block => braces(statements(block, block.stats, indent + 2))
      case call: Term.Apply => chain(call, indent)
      case call: Term.ApplyType => chain(call, indent)
      case select: Term.Select => chain(select, indent)
      case call: Term.ApplyInfix => infix(call, indent)
      case unary: Term.ApplyUnary => {
        val op = tree(unary.op, indent)
        val leading = leadingComments(unary.arg, indent + 2, lineStart = true)
        val argument = if (leading.isEmpty) inContext(TreeSyntacticGroup.SimpleExpr, unary.arg, indent)
        else parenthesized(leading + tree(unary.arg, indent + 2, lineStart = true), multiline = true)
        op + argument
      }
      case function: Term.Function => lambda(function, indent)
      case tuple: Term.Tuple => clause(tuple, tuple.args, "(", ")", indent)
      case assignment: Term.Assign => {
        val lhs = tree(assignment.lhs, indent)
        val rhs = assignmentBody(assignment.rhs, indent)
        lhs + rhs
      }
      case returning: Term.Return =>
        if (returning.expr.isInstanceOf[Lit.Unit] && returning.expr.begOffset == returning.expr.endOffset) {
          Doc.text("return")
        } else {
          val leading = leadingComments(returning.expr, indent + 2, lineStart = true)
          if (leading.isEmpty) Doc.text("return") + spaced(returning.expr, indent)
          else Doc.text("return ") + parenthesized(
            leading + tree(returning.expr, indent + 2, lineStart = true), multiline = true)
        }
      case throwing: Term.Throw => Doc.text("throw") + spaced(throwing.expr, indent)
      case ascribed: Term.Ascribe => {
        val expression = inContext(TreeSyntacticGroup.PostfixExpr, ascribed.expr, indent)
        val tpe = spaced(ascribed.tpe, indent)
        expression + Doc.char(':') + tpe
      }
      case annotated: Term.Annotate => {
        val expression = inContext(TreeSyntacticGroup.PostfixExpr, annotated.expr, indent)
        val annotations = annotated.annots.iterator.map(spaced(_, indent)).toVector
        expression + Doc.char(':') + Doc.cat(annotations)
      }
      case anonymous: Term.AnonymousFunction if bracedLambda(anonymous) => {
        val before = hardLineCount
        val beforeComments = commentIndex
        val body = lambdaStatements(anonymous, anonymous.body, indent + 2)
        val multiline = hardLineCount != before ||
          (commentIndex > beforeComments && commentTokens(commentIndex - 1).text.startsWith("//"))
        if (multiline) braces(body)
        else body.bracketBy(Doc.char('{'), Doc.char('}'))
      }
      case anonymous: Term.AnonymousFunction => tree(anonymous.body, indent)
      case function: Term.ContextFunction => lambda(function, indent)
      case lambda: Term.PolyFunction => {
        val braced = bracedLambda(lambda)
        arrowBody(lambdaHeader(lambda, if (braced) indent + 2 else indent), braced) {
          if (braced) lambdaStatements(lambda, lambda.body, indent + 2)
          else lambdaBody(lambda, indent + 2)
        }
      }
      case function: Term.PartialFunction =>
        inlineArgumentCase(function).fold(braces(statements(function, function.cases, indent + 2)))(caseArgument(_, indent))
      case loop: Term.For => {
        val enumerators = tree(loop.enumsBlock, indent)
        val body = requiredBlock(loop.body, indent)
        Doc.text("for ") + enumerators + Doc.space + body
      }
      case loop: Term.ForYield => {
        val enumerators = tree(loop.enumsBlock, indent)
        val body = spaced(loop.body, indent, (value, depth) => inContext(TreeSyntacticGroup.Expr, value, depth))
        Doc.text("for ") + enumerators + Doc.text(" yield") + body
      }
      case creation: Term.New => Doc.text("new") + spaced(creation.init, indent)
      case creation: Term.NewAnonymous => Doc.text("new") + spaced(creation.templ, indent)
      case self: Term.This => {
        val qualifier = tree(self.qual, indent)
        (if (qualifier.isEmpty) Doc.empty else qualifier + Doc.char('.')) + Doc.text("this")
      }
      case parent: Term.Super => {
        val qualifier = tree(parent.thisp, indent)
        val mixin = tree(parent.superp, indent)
        (if (qualifier.isEmpty) Doc.empty else qualifier + Doc.char('.')) +
        Doc.text("super") + (if (mixin.isEmpty) Doc.empty else Doc.char('[') + mixin + Doc.char(']'))
      }
      case _: Term.Placeholder => Doc.char('_')
      case repeated: Term.Repeated => atom(repeated.expr, indent) + Doc.char('*')
      case eta: Term.Eta => atom(eta.expr, indent) + Doc.text(" _")
      case loop: Term.While => {
        val condition = controlCondition(loop.expr, "do", indent)
        val body = requiredBlock(loop.body, indent)
        Doc.text("while") + condition + Doc.space + body
      }
      case loop: Term.Do => {
        val body = requiredBlock(loop.body, indent)
        val condition = parenthesized(tree(loop.expr, indent + 2, lineStart = true))
        Doc.text("do ") + body + Doc.text(" while ") + condition
      }
      case _: Term.EndMarker => Doc.empty
      case interpolation: Term.Interpolate => {
        val prefix = tree(interpolation.prefix, indent)
        val quote = interpolation.tokens.collectFirst {
          case start: Token.Interpolation.Start => start.text
        }.getOrElse("\"")
        val stripMargin = quote == "\"\"\"" && hasStripMargin(interpolation) && canAlignMargins(interpolation)
        val contents = literalParts(interpolation.parts, interpolation.args, indent, interpolated = true,
          stripMargin = stripMargin)
        val body = Doc.text(quote) + contents + Doc.text(quote)
        prefix + (if (stripMargin) {
          body.hang(if (interpolation.parts.head.value.toString.startsWith("|")) 3 else 2)
        } else body)
      }
      case xml: Term.Xml => literalParts(xml.parts, xml.args, indent, interpolated = false)
      case conditional: Term.If => {
        val mods = modifiers(conditional.mods, indent)
        val condition = controlCondition(conditional.cond, "then", indent)
        val thenBranch = requiredBlock(conditional.thenp, indent)
        val elseBranch = if (
          conditional.elsep.isInstanceOf[Lit.Unit] &&
          conditional.elsep.begOffset == conditional.elsep.endOffset
        ) {
          Doc.empty
        } else Doc.text(" else") + spaced(conditional.elsep, indent,
          (_, depth) => elseBody(conditional.elsep, depth))
        mods + Doc.text("if") + condition + Doc.space + thenBranch + elseBranch
      }
      case matching: Term.Match => {
        val mods = modifiers(matching.mods, indent)
        val expression = inContext(TreeSyntacticGroup.PostfixExpr, matching.expr, indent)
        val cases = tree(matching.casesBlock, indent)
        mods + expression + Doc.text(" match ") + cases
      }
      case matching: Term.SelectMatch => {
        val mods = modifiers(matching.mods, indent)
        val expression = inContext(TreeSyntacticGroup.SimpleExpr, matching.expr, indent)
        val cases = tree(matching.casesBlock, indent)
        mods + expression + Doc.text(".match ") + cases
      }
      case select: Term.SelectPostfix => {
        val commented = firstCommentLine(select.name.begOffset).nonEmpty
        val qualifier = if (commented) parenthesized {
          val body = atom(select.qual, indent + 2)
          val footer = comments(select.name.begOffset, indent + 2)
          body + (if (footer.isEmpty) Doc.empty else hardLine + footer)
        } else atom(select.qual, indent)
        val name = tree(select.name, indent)
        qualifier + Doc.space + name
      }
      case quote: Term.QuotedMacroExpr => Doc.char('\'') + tree(quote.body, indent)
      case quote: Term.QuotedMacroType =>
        tree(quote.tpe, indent + 2).tightBracketBy(Doc.text("'["), Doc.char(']'))
      case splice: Term.SplicedMacroExpr => Doc.char('$') + tree(splice.body, indent)
      case splice: Term.SplicedMacroPat =>
        Doc.text("${") + tree(splice.pat, indent) + Doc.char('}')
      case matching: Term.SubMatch => {
        val mods = modifiers(matching.mods, indent)
        val expression = inContext(TreeSyntacticGroup.PostfixExpr, matching.expr, indent)
        val cases = tree(matching.casesBlock, indent)
        mods + expression + Doc.text(" match ") + cases

      }
      case _ => compoundSyntax(value, indent)
    }

    private def typeSyntax(value: Tree, indent: Int): Doc = value match {
      case applied: Type.Apply => {
        val fun = atom(applied.tpe, indent)
        val arguments = tree(applied.argClause, indent)
        fun + arguments
      }
      case select: Type.Select => {
        val qualifier = atom(select.qual, indent)
        val name = tree(select.name, indent)
        qualifier + Doc.char('.') + name
      }
      case tuple: Type.Tuple => clause(tuple, tuple.args, "(", ")", indent)
      case function: Type.Function => {
        val parameters = tree(function.paramClause, indent)
        val result = spaced(function.res, indent)
        parameters + Doc.text(" =>") + result
      }
      case tpe: Type.ApplyInfix => {
        val lhs = infixOperand(tpe, tpe.lhs, indent)
        val op = spaced(tpe.op, indent)
        val rhs = spaced(tpe.rhs, indent, (value, depth) => infixOperand(tpe, value, depth))
        lhs + op + rhs
      }
      case tpe: Type.Wildcard => Doc.char('?') + tree(tpe.bounds, indent)
      case tpe: Type.AnonymousLambda => tree(tpe.tpe, indent)
      case tpe: Type.AnonymousParam =>
        tpe.variant.fold(Doc.empty)(tree(_, indent)) + Doc.char('*')
      case tpe: Type.BoundsAlias => {
        val bound = tree(tpe.bounds, indent)
        val name = spaced(tpe.name, indent)
        bound + Doc.text(" as") + name
      }
      case tpe: Type.ByName => Doc.text("=>") + spaced(tpe.tpe, indent)
      case tpe: Type.ContextFunction => {
        val parameters = tree(tpe.paramClause, indent)
        val result = spaced(tpe.res, indent)
        parameters + Doc.text(" ?=>") + result
      }
      case tpe: Type.Repeated => atom(tpe.tpe, indent) + Doc.char('*')
      case tpe: Type.Singleton => tree(tpe.ref, indent) + Doc.text(".type")
      case tpe: Type.Var => tree(tpe.name, indent)
      case tpe: Type.Annotate => {
        val value = atom(tpe.tpe, indent)
        val annotations = tpe.annots.iterator.map(spaced(_, indent)).toVector
        value + Doc.cat(annotations)
      }
      case tpe: Type.TypedParam => {
        val mods = modifiers(tpe.mods, indent)
        val name = tree(tpe.name, indent)
        val value = spaced(tpe.typ, indent)
        mods + name + Doc.char(':') + value
      }
      case _: Type.PatWildcard => Doc.char('_')
      case projected: Type.Project => {
        val qualifier = atom(projected.qual, indent)
        val name = tree(projected.name, indent)
        qualifier + Doc.char('#') + name
      }
      case compound: Type.With => {
        val lhs = inContext(TreeSyntacticGroup.WithTyp, compound.lhs, indent)
        val rhs = spaced(compound.rhs, indent, (value, depth) => inContext(TreeSyntacticGroup.WithTyp, value, depth))
        lhs + Doc.text(" with") + rhs
      }
      case refinement: Type.Refine => {
        val base = refinement.tpe.map(inContext(TreeSyntacticGroup.WithTyp, _, indent))
        val body = tree(refinement.body, indent)
        base.fold(Doc.empty)(_ + Doc.space) + body
      }
      case lambda: Type.Lambda => {
        val parameters = tree(lambda.tparamClause, indent)
        val result = spaced(lambda.tpe, indent)
        parameters + Doc.text(" =>>") + result
      }
      case function: Type.PolyFunction => {
        val parameters = tree(function.tparamClause, indent)
        val result = spaced(function.tpe, indent)
        parameters + Doc.text(" =>") + result
      }
      case function: Type.PureFunction => {
        val parameters = tree(function.paramClause, indent)
        val result = spaced(function.res, indent)
        parameters + Doc.text(" ->") + result
      }
      case function: Type.PureContextFunction => {
        val parameters = tree(function.paramClause, indent)
        val result = spaced(function.res, indent)
        parameters + Doc.text(" ?->") + result
      }
      case byName: Type.PureByName => Doc.text("->") + spaced(byName.tpe, indent)
      case argument: Type.FunctionArg => {
        val mods = modifiers(argument.mods, indent)
        val tpe = tree(argument.tpe, indent)
        mods + tpe
      }
      case assignment: Type.Assign => {
        val name = tree(assignment.name, indent)
        val rhs = assignmentBody(assignment.rhs, indent, allowBreak = false)
        name + rhs
      }
      case matching: Type.Match => {
        val tpe = inContext(TreeSyntacticGroup.AnyInfixTyp, matching.tpe, indent)
        val cases = tree(matching.casesBlock, indent)
        tpe + Doc.text(" match ") + cases
      }
      case block: Type.Block => {
        val definitions = block.typeDefs.iterator.map(tree(_, indent)).toVector
        val result = tree(block.tpe, indent)
        Doc.intercalate(Doc.text("; "), definitions :+ result)
      }
      case capturing: Type.Capturing => {
        val captures = tree(capturing.caps, indent)
        val body = capturing.tpe match {
          case function: Type.ParamFunctionType => {
              val parameters = tree(function.paramClause, indent)
              val result = spaced(function.res, indent)
              val arrow = function match {
                case _: Type.Function => "=>"
                case _: Type.ContextFunction => "?=>"
                case _: Type.PureFunction => "->"
                case _: Type.PureContextFunction => "?->"
              }
              parameters + Doc.space + Doc.text(arrow) + captures + result
            }
          case byName: Type.ByNameType => {
              val result = spaced(byName.res, indent)
              val arrow = byName match {
                case _: Type.ByName => "=>"
                case _: Type.PureByName => "->"
              }
              Doc.text(arrow) + captures + result
            }
          case tpe => atom(tpe, indent) + Doc.char('^') + captures
        }
        body
      }
      case macroType: Type.Macro => tree(macroType.body, indent)
      case _ => compoundSyntax(value, indent)
    }

    private def patternSyntax(value: Tree, indent: Int): Doc = value match {
      case pattern: Pat.Var => tree(pattern.name, indent)
      case _: Pat.Wildcard => Doc.char('_')
      case pattern: Pat.Tuple => clause(pattern, pattern.args, "(", ")", indent)
      case pattern: Pat.Typed => {
        val lhs = inContext(TreeSyntacticGroup.SimplePattern, pattern.lhs, indent)
        val rhs = spaced(pattern.rhs, indent, (value, depth) => inContext(TreeSyntacticGroup.RefineTyp, value, depth))
        lhs + Doc.char(':') + rhs
      }
      case pattern: Pat.Extract => {
        val fun = atom(pattern.fun, indent)
        val arguments = tree(pattern.argClause, indent)
        fun + arguments
      }
      case pattern: Pat.ExtractInfix => {
        val commented = firstCommentLine(pattern.endOffset).nonEmpty
        val depth = if (commented) indent + 2 else indent
        val lhs = infixOperand(pattern, pattern.lhs, depth)
        val op = spaced(pattern.op, depth)
        val rhs = pattern.argClause.values match {
          case (_: Lit.Unit | _: Pat.Tuple) :: Nil => spaced(pattern.argClause, depth)
          case single :: Nil => spaced(single, depth, (value, depth) => infixOperand(pattern, value, depth))
          case _ => spaced(pattern.argClause, depth)
        }
        val contents = lhs + op + rhs
        if (commented) parenthesized(contents, multiline = true) else contents
      }
      case pattern: Pat.Bind => {
        val lhs = inContext(TreeSyntacticGroup.SimplePattern, pattern.lhs, indent)
        val rhs = spaced(pattern.rhs, indent, (value, depth) => inContext(TreeSyntacticGroup.AnyPattern3, value, depth))
        lhs + Doc.text(" @") + rhs
      }
      case pattern: Pat.Alternative => {
        val lhs = inContext(TreeSyntacticGroup.Pattern, pattern.lhs, indent)
        val rhs = spaced(pattern.rhs, indent, (value, depth) => inContext(TreeSyntacticGroup.Pattern, value, depth))
        lhs + Doc.text(" |") + rhs
      }
      case pattern: Pat.Given => Doc.text("given") + spaced(pattern.tpe, indent)
      case _: Pat.SeqWildcard => Doc.text("_*")
      case repeated: Pat.Repeated => tree(repeated.name, indent) + Doc.char('*')
      case assignment: Pat.Assign => {
        val name = tree(assignment.name, indent)
        val rhs = assignmentBody(assignment.rhs, indent, allowBreak = false)
        name + rhs
      }
      case macroPattern: Pat.Macro => tree(macroPattern.body, indent)
      case interpolation: Pat.Interpolate => {
        val prefix = tree(interpolation.prefix, indent)
        val contents = literalParts(interpolation.parts, interpolation.args, indent, interpolated = true)
        val quote = interpolation.tokens.collectFirst {
          case start: Token.Interpolation.Start => start.text
        }.getOrElse("\"")
        prefix + Doc.text(quote) + contents + Doc.text(quote)
      }
      case xml: Pat.Xml => literalParts(xml.parts, xml.args, indent, interpolated = false)

      case _ => compoundSyntax(value, indent)
    }

    private def compoundSyntax(value: Tree, indent: Int): Doc = value match {
      case source: Source => statements(source, source.stats, indent)
      case pkg: Pkg => {
        val name = spaced(pkg.ref, indent)
        val comment = firstCommentLine(pkg.body.stats.headOption.fold(pkg.body.endOffset)(_.begOffset))
        val firstLine = comment.orElse(pkg.body.stats.headOption.map(_.pos.startLine)).getOrElse(pkg.ref.pos.endLine)
        val body = tree(pkg.body, indent)
        val separator = if (body.isEmpty) Doc.empty
        else if (packageNeedsBraces(pkg)) Doc.space
        else if (comment.nonEmpty) hardLine else statementGap(pkg.ref.pos.endLine, firstLine, indent)
        Doc.text("package") + name + separator + body
      }
      case body: Pkg.Body if body.parent.exists {
            case pkg: Pkg => !packageNeedsBraces(pkg)
            case _ => false
          } => statements(body, body.stats, indent)
      case body: Pkg.Body => braces(statements(body, body.stats, indent + 2))
      case obj: Defn.Object => {
        val mods = modifiers(obj.mods, indent)
        val name = spaced(obj.name, indent)
        val body = templateSuffix(obj.templ, indent)
        mods + Doc.text("object") + name + body
      }
      case template: Template
          if template.earlyClause.isEmpty && template.inits.isEmpty && template.derives.isEmpty =>
        tree(template.body, indent)
      case body: Template.Body if canOmitEmptyBody(body) &&
          (body.begOffset == body.endOffset || firstCommentLine(body.endOffset).isEmpty) => Doc.empty
      case body: Template.Body if body.selfOpt.isEmpty =>
        braces(statements(body, body.stats, indent + 2))
      case definition: Defn.Val => {
        val mods = modifiers(definition.mods, indent)
        val patterns = definition.pats.iterator.map(spaced(_, indent)).toVector
        val tpe = definition.decltpe.map(value => Doc.char(':') + spaced(value, indent))
        val header = mods + Doc.text("val") + Doc.intercalate(Doc.comma, patterns) + tpe.getOrElse(Doc.empty)
        header + assignmentBody(definition.rhs, indent, header = Some(header))
      }
      case definition: Defn.Def => {
        val mods = modifiers(definition.mods, indent)
        val signature = methodSignature(definition.name, definition.paramClauseGroups, definition.decltpe, indent)
        val header = mods + Doc.text("def") + signature
        header + assignmentBody(definition.body, indent, header = Some(header))
      }
      case group: Member.ParamClauseGroup => {
        val types = tree(group.tparamClause, indent)
        val parameters = group.paramClauses.iterator.map(tree(_, indent)).toVector
        types + Doc.cat(parameters)
      }
      case parameters: Type.ParamClause if parameters.values.isEmpty => Doc.empty
      case parameters: Type.ParamClause => clause(parameters, parameters.values, "[", "]", indent)
      case parameter: Type.Param => {
        val mods = modifiers(parameter.mods, indent)
        val name = tree(parameter.name, indent)
        val parameters = tree(parameter.tparamClause, indent)
        val bounds = tree(parameter.bounds, indent)
        mods + name + parameters + bounds
      }
      case bounds: Type.Bounds if bounds.view.isEmpty => {
        val lower = bounds.lo.map(spaced(_, indent))
        val upper = bounds.hi.map(spaced(_, indent))
        val context = bounds.context.iterator.map(spaced(_, indent)).toVector
        lower.fold(Doc.empty)(Doc.text(" >:") + _) +
        upper.fold(Doc.empty)(Doc.text(" <:") + _) +
        Doc.cat(context.map(Doc.char(':') + _))
      }
      case parameters: Term.ParamClause => {
        val caseFields = parameters.values.lengthCompare(1) > 0 && parameters.parent.exists {
          case constructor: Ctor.Primary =>
            constructor.paramClauses.headOption.exists(_ eq parameters) && constructor.parent.exists {
              case definition: Defn.Class => definition.mods.exists(_.isInstanceOf[Mod.Case])
              case _ => false
            }
          case _ => false
        }
        clause(parameters, parameters.values, "(", ")", indent,
          parameters.mod.fold(Doc.empty)(mod => tree(mod, indent + 2, lineStart = true)),
          forceMultiline = caseFields)
      }
      case parameter: Term.Param => {
        val mods = modifiers(parameter.mods.iterator.filterNot(_.isInstanceOf[Mod.ParamsType]).toVector, indent)
        val name = tree(parameter.name, indent)
        val tpe = parameter.decltpe.map(value =>
          if (parameter.name.value.isEmpty) tree(value, indent) else Doc.char(':') + spaced(value, indent))
        val default = parameter.default.map(assignmentBody(_, indent))
        mods + name + tpe.getOrElse(Doc.empty) + default.getOrElse(Doc.empty)
      }
      case arguments: Term.ArgClause =>
        arguments.values match {
          case single :: Nil if bracedCall(arguments) => {
              val body = tree(single, indent)
              val remaining = tokenComments(arguments, arguments.endOffset, indent)
              bracedArgumentSpace(single) + body +
              (if (remaining.isEmpty) Doc.empty else hardLine + remaining + hardLine)
            }
          case _ => {
            clause(arguments, arguments.values, "(", ")", indent,
              arguments.mod.fold(Doc.empty)(mod => tree(mod, indent + 2, lineStart = true)))
          }
        }
      case arguments: Type.ArgClause => clause(arguments, arguments.values, "[", "]", indent)
      case cases: Term.CasesBlock => braces(statements(cases, cases.cases, indent + 2))
      case branch: Case => {
        val header = caseHeader(branch, indent)
        if (branch.body.isInstanceOf[Term.SubMatch]) {
          header + Doc.text(" if ") + tree(branch.body, indent)
        } else arrowBody(header + arrowSuffix)(caseContents(branch, indent + 2))
      }
      case parameters: Type.FuncParamClause => parameters.values match {
          case single :: Nil if !single.isInstanceOf[Type.Tuple] &&
              !single.isInstanceOf[Type.ByNameType] && !single.isInstanceOf[Type.FunctionParamOrArg] &&
              !single.isInstanceOf[Type.ParamFunctionType] && !single.isInstanceOf[Type.PolyFunction] =>
            tree(single, indent)
          case _ => clause(parameters, parameters.values, "(", ")", indent)
        }
      case imported: Import => {
        val docs = imported.importers.iterator.map(spaced(_, indent)).toVector
        Doc.text("import") + Doc.intercalate(Doc.comma, docs)
      }
      case importer: Importer => {
        val qualifier = tree(importer.ref, indent)
        val selections = importer.importees match {
          case (single: Importee.Name) :: Nil => tree(single, indent)
          case (single: Importee.Wildcard) :: Nil => tree(single, indent)
          case _ => clause(importer, importer.importees, "{", "}", indent)
        }
        qualifier + Doc.char('.') + selections
      }
      case selection: Importee.Name => tree(selection.name, indent)
      case _: Importee.Wildcard => Doc.char('*')
      case rename: Importee.Rename => {
        val name = tree(rename.name, indent)
        val renamed = spaced(rename.rename, indent)
        name + Doc.text(" as") + renamed
      }
      case unimport: Importee.Unimport => tree(unimport.name, indent) + Doc.text(" as _")
      case _: Importee.GivenAll => Doc.text("given")
      case imported: Importee.Given => Doc.text("given") + spaced(imported.tpe, indent)
      case _: Mod.Final => Doc.text("final")
      case _: Mod.Lazy => Doc.text("lazy")
      case _: Mod.Override => Doc.text("override")
      case _: Mod.Inline => Doc.text("inline")
      case _: Mod.Transparent => Doc.text("transparent")
      case _: Mod.Implicit => Doc.text("implicit")
      case _: Mod.Using => Doc.text("using")
      case _: Mod.Covariant => Doc.char('+')
      case _: Mod.Contravariant => Doc.char('-')
      case modifier: Mod.Private => {
        val within = tree(modifier.within, indent)
        Doc.text("private") + (if (within.isEmpty) Doc.empty else Doc.char('[') + within + Doc.char(']'))
      }
      case modifier: Mod.Protected => {
        val within = tree(modifier.within, indent)
        Doc.text("protected") + (if (within.isEmpty) Doc.empty else Doc.char('[') + within + Doc.char(']'))
      }
      case definition: (Defn.Class | Defn.Trait | Defn.Enum) => {
        val mods = modifiers(definition.mods, indent)
        val name = spaced(definition.name, indent)
        val parameters = tree(definition.tparamClause, indent)
        val constructor = tree(definition.ctor, indent)
        val template = templateSuffix(definition.templ, indent)
        val keyword = definition match {
          case _: Defn.Class => "class"
          case _: Defn.Trait => "trait"
          case _: Defn.Enum => "enum"
        }
        mods + Doc.text(keyword) + name + parameters + constructor + template
      }
      case definition: Defn.EnumCase => {
        val mods = modifiers(definition.mods, indent)
        val name = spaced(definition.name, indent)
        val parameters = tree(definition.tparamClause, indent)
        val constructor = tree(definition.ctor, indent)
        val parents = if (definition.inits.isEmpty) Doc.empty else arrowTail(2) {
          val values = definition.inits.iterator.map(spaced(_, indent + 2)).toVector
          Doc.text("extends") + Doc.intercalate(Doc.text(" with"), values)
        }
        mods + Doc.text("case") + name + parameters + constructor + parents
      }
      case definition: Defn.RepeatedEnumCase => {
        val mods = modifiers(definition.mods, indent)
        val names = definition.cases.iterator.map(spaced(_, indent)).toVector
        mods + Doc.text("case") + Doc.intercalate(Doc.comma, names)
      }
      case definition: Defn.Given => {
        givenSignature(definition.mods, definition.name, definition.paramClauseGroups, definition.templ, indent)
      }
      case definition: Defn.GivenAlias => {
        val signature = givenSignature(definition.mods, definition.name, definition.paramClauseGroups,
          definition.decltpe, indent)
        signature + assignmentBody(definition.body, indent, header = Some(signature))
      }
      case definition: Defn.ExtensionGroup => {
        val parameters = definition.paramClauseGroup.map(tree(_, indent))
        val body = definition.body match {
          case block: Term.Block => tree(block, indent)
          case block: Stat.Block => tree(block, indent)
          case stat => braces(statements(definition, Vector(stat), indent + 2))
        }
        Doc.text("extension ") + parameters.getOrElse(Doc.empty) + Doc.space + body
      }
      case definition: Defn.Type => {
        val mods = modifiers(definition.mods, indent)
        val name = spaced(definition.name, indent)
        val parameters = tree(definition.tparamClause, indent)
        val bounds = tree(definition.bounds, indent)
        val body = assignmentBody(definition.body, indent)
        mods + Doc.text("type") + name + parameters + bounds + body
      }
      case definition: Defn.Var => {
        val mods = modifiers(definition.mods, indent)
        val patterns = definition.pats.iterator.map(spaced(_, indent)).toVector
        val result = definition.decltpe.map(value => Doc.char(':') + spaced(value, indent))
        val header = mods + Doc.text("var") + Doc.intercalate(Doc.comma, patterns) + result.getOrElse(Doc.empty)
        header + assignmentBody(definition.body, indent, header = Some(header))
      }
      case declaration: (Decl.Val | Decl.Var) => {
        val mods = modifiers(declaration.mods, indent)
        val patterns = declaration.pats.iterator.map(spaced(_, indent)).toVector
        val result = spaced(declaration.decltpe, indent)
        val keyword = declaration match {
          case _: Decl.Val => "val"
          case _: Decl.Var => "var"
        }
        mods + Doc.text(keyword) + Doc.intercalate(Doc.comma, patterns) +
          Doc.char(':') + result
      }
      case declaration: Decl.Def => {
        val mods = modifiers(declaration.mods, indent)
        val signature = methodSignature(declaration.name, declaration.paramClauseGroups, Some(declaration.decltpe), indent)
        mods + Doc.text("def") + signature
      }
      case declaration: Decl.Type => {
        val mods = modifiers(declaration.mods, indent)
        val name = spaced(declaration.name, indent)
        val parameters = tree(declaration.tparamClause, indent)
        val bounds = tree(declaration.bounds, indent)
        mods + Doc.text("type") + name + parameters + bounds
      }
      case declaration: Decl.GivenLike => {
        givenSignature(declaration.mods, declaration.name, declaration.paramClauseGroups, declaration.decltpe, indent)
      }
      case constructor: Ctor.Primary => {
        val mods = Doc.cat(constructor.mods.iterator.map(spaced(_, indent)).toVector)
        val parameters = constructor.paramClauses.iterator.map(tree(_, indent)).toVector
        mods + (if (mods.isEmpty || parameters.isEmpty) Doc.empty else Doc.space) + Doc.cat(parameters)
      }
      case constructor: Ctor.Secondary => {
        val mods = modifiers(constructor.mods, indent)
        val parameters = constructor.paramClauses.iterator.map(tree(_, indent)).toVector
        val body = assignmentBody(constructor.body, indent)
        mods + Doc.text("def this") + Doc.cat(parameters) + body
      }
      case body: Ctor.Block => braces(statements(body, body.init +: body.stats.toVector, indent + 2))
      case init: Init => {
        val tpe = init.tpe match {
          case singleton: Type.Singleton if singleton.ref.isInstanceOf[Term.This] =>
            tree(singleton.ref, indent)
          case tpe => atom(tpe, indent)
        }
        val arguments = init.argClauses.iterator.map(tree(_, indent)).toVector
        tpe + Doc.cat(arguments)
      }
      case self: Self => {
        val name = tree(self.name, indent)
        val tpe = self.decltpe.map(value => Doc.char(':') + spaced(value, indent))
        val alias = if (name.isEmpty && tpe.nonEmpty) Doc.char('_') else name
        alias + tpe.getOrElse(Doc.empty) + arrowSuffix
      }
      case template: Template => {
        val header = templateHeader(template, indent)
        val body = tree(template.body, indent)
        Doc.spread(Vector(header, body).filter(_.nonEmpty))
      }
      case body: Template.Body => {
        val self = body.selfOpt.map(tree(_, indent + 2))
        val stats = statements(body, body.stats, indent + 2)
        braces(Doc.intercalate(hardLine, (self.toVector :+ stats).filter(_.nonEmpty)))
      }
      case block: Stat.Block => braces(statements(block, block.stats, indent + 2))
      case obj: Pkg.Object => {
        val mods = modifiers(obj.mods, indent)
        val name = spaced(obj.name, indent)
        val template = templateSuffix(obj.templ, indent)
        Doc.text("package ") + mods + Doc.text("object") + name + template
      }
      case annotation: Mod.Annot => Doc.char('@') + tree(annotation.init, indent)
      case _: Mod.Sealed => Doc.text("sealed")
      case _: Mod.Open => Doc.text("open")
      case _: Mod.Case => Doc.text("case")
      case _: Mod.Abstract => Doc.text("abstract")
      case _: Mod.ValParam => Doc.text("val")
      case _: Mod.VarParam => Doc.text("var")
      case _: Mod.Infix => Doc.text("infix")
      case _: Mod.Opaque => Doc.text("opaque")
      case _: Mod.Erased => Doc.text("erased")
      case _: Mod.Tracked => Doc.text("tracked")
      case _: Mod.Into => Doc.text("into")

      case exported: Export => {
        val docs = exported.importers.iterator.map(spaced(_, indent)).toVector
        Doc.text("export") + Doc.intercalate(Doc.comma, docs)
      }
      case arguments: Pat.ArgClause => clause(arguments, arguments.values, "(", ")", indent)
      case enumerators: Term.EnumeratorsBlock =>
        braces(statements(enumerators, enumerators.enums, indent + 2))
      case generator: Enumerator.Generator => {
        val pattern = tree(generator.pat, indent)
        val rhs = spaced(generator.rhs, indent)
        pattern + Doc.text(" <-") + rhs
      }
      case generator: Enumerator.CaseGenerator => {
        val pattern = spaced(generator.pat, indent)
        val rhs = spaced(generator.rhs, indent)
        Doc.text("case") + pattern + Doc.text(" <-") + rhs
      }
      case binding: Enumerator.Val => {
        val pattern = tree(binding.pat, indent)
        val rhs = assignmentBody(binding.rhs, indent)
        pattern + rhs
      }
      case guard: Enumerator.Guard => {
        val condition = parenthesized(tree(guard.cond, indent + 2, lineStart = true))
        Doc.text("if ") + condition
      }
      case attempted: Term.TryClause => {
        val body = requiredBlock(attempted.expr, indent)
        val handler = attempted.catchClause.map {
          case value: Term => requiredBlock(value, indent)
          case value => tree(value, indent)
        }
        val finalizer = attempted.finallyp.map(requiredBlock(_, indent))
        Doc.text("try ") + body + handler.fold(Doc.empty)(Doc.text(" catch ") + _) +
          finalizer.fold(Doc.empty)(Doc.text(" finally ") + _)
      }
      case cases: Type.CasesBlock => braces(statements(cases, cases.cases, indent + 2))
      case branch: TypeCase => {
        val pattern = tree(branch.pat, indent)
        val body = tree(branch.body, indent)
        casePrefix + pattern + functionArrow + body
      }
      case _: Type.CapturesAny => Doc.empty
      case captures: Type.CapturesSet => clause(captures, captures.values, "{", "}", indent)
      case comment: Tree.Comment => verbatim(comment.parts.iterator.map(_.value).mkString, indent)
      case comments: Tree.Comments => Doc.stack(comments.values.iterator.map(tree(_, indent)).toVector)
      case _: Defn.Macro | _: Type.Existential | _: Type.Bounds |
          _: MultiSource | _: scala.meta.internal.trees.Quasi =>
        throw FormatError.Unsupported(value.productPrefix)
      // Tree is open; an unknown dependency node must fail rather than lose source content.
      case unexpected =>
        throw FormatError.Unsupported(s"unexpected Scalameta node: ${unexpected.productPrefix}")
    }

  }
}
