package csf

import munit.FunSuite
import scala.meta.*

class Scala3SyntaxSuite extends FunSuite {
  private given Dialect = dialects.Scala3

  private def structure(source: Source): Any = {
    def normalize(value: Any): Any = value match {
      case Term.Block(List(term: Term)) => normalize(term)
      case tree: Tree => (tree.productPrefix, tree.productIterator.map(normalize).toList)
      case values: List[?] => values.map(normalize)
      case value: Option[?] => value.map(normalize)
      case other => other
    }
    normalize(source)
  }

  private def comments(source: String): List[String] = {
    source.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList
  }

  private val programs = List(
    "type lambda" -> "object X { type F = [A] =>> List[A] }",
    "polymorphic function type" -> "object X { type F = [A] => A => A }",
    "dependent and erased function arguments" ->
      "object X { type F = (x: Input) => x.Result; type G = (erased Input) => Output }",
    "match type" ->
      "object X { type Elem[A] = A match {\ncase String => Char\ncase List[t] => t\n} }",
    "structural refinement and projection" ->
      "object X { type F = Base { type Out <: Product; val size: Int }; type G = F#Out }",
    "context function type and term" ->
      "object X { type F = Context ?=> Result; val f = (context: Context) ?=> context.result }",
    "polymorphic term lambda" -> "object X { val identity = [A] => (value: A) => value }",
    "quoted expressions and splices" ->
      "object X { def quoted = '{ val value = ${expression}; value }; def quotedType = '[List[Int]] }",
    "quoted expression and type patterns" ->
      "object X { val result = value match { case '{ $expression: Int } => expression; case '[List[t]] => 1 } }",
    "interpolation patterns and literal dollars" ->
      "object X { val result = value match { case s\"$$price${amount}EUR\" => amount } }",
    "multiline interpolation patterns" ->
      "object X { val result = value match { case s\"\"\"first\n  ${name}\nlast\"\"\" => name } }",
    "XML expressions and patterns" ->
      "object X { val xml = <node attr=\"value\">{name}</node>; val result = xml match { case <node>{child}</node> => child } }",
    "extractor sequence patterns" ->
      "object X { val result = xs match { case Seq(first, rest*) => rest; case Seq(_*) => xs } }",
    "for case generators and bindings" ->
      "object X { val result = for { case Some(value) <- values; next = value + 1; if next > 0 } yield next }",
    "for statements" -> "object X { def run = for (value <- values) use(value) }",
    "partial function and placeholders" ->
      "object X { val f = { case Some(value) if value > 0 => value; case _ => 0 }; val g = values.map(_.size) }",
    "try catch finally" ->
      "object X { def run = try action() catch { case error: Error => recover(error) } finally cleanup() }",
    "try handler expression" -> "object X { def run = try action() catch handler }",
    "while and assignment" -> "object X { def run = { while (ready) count = count + 1; () } }",
    "return throw and type ascription" ->
      "object X { def run: Int = { if (ready) return (value: Int); throw failure } }",
    "anonymous instances and parent references" ->
      "class X extends Parent { val instance = new Service { def run = this.result }; def run = super.run; def self = X.this }",
    "inline conditional and match" ->
      "object X { inline def select(inline value: Int) = inline if (value > 0) value else 0; inline def kind(inline value: Any) = inline value match { case _: String => 1; case _ => 0 } }",
    "match selector" -> "object X { val result = value.match { case Some(x) => x; case _ => 0 } }",
    "comments in comprehensions and cases" ->
      "object X { val result = for { /* generator */ value <- values; /* guard */ if value > 0 } yield { /* result */ value }; val f = { /* branch */ case x => /* body */ x } }",
    "comments before application arguments" -> "object X { val result = call /* arguments */ (1) }",
    "comments before type arguments" -> "object X { val result = call /* types */ [Int](1) }",
    "comments before constructor arguments" -> "class X extends Parent /* arguments */ (1)",
    "given nullary function type" -> "object X { given (() => Int) = () => 1 }",
    "given unary function type" -> "object X { given (Int => Int) = (value: Int) => value }",
  )

  programs.foreach { case (name, source) =>
    test(s"$name preserves AST, comments, and canonical braces") {
      val result = Formatter.format(source).fold(error => fail(error.getMessage), identity)
      val input = source.parse[Source].get
      val output = {
        given Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
        result.parse[Source] match {
          case Parsed.Success(tree) => tree
          case error: Parsed.Error => fail(s"Invalid brace syntax: $error\n$result")
        }
      }

      assertEquals(structure(output), structure(input), result)
      assertEquals(comments(result), comments(source), result)
      assertEquals(Formatter.format(result), Right(result))
    }
  }
}
