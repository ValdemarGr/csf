package csf

import munit.FunSuite
import scala.meta.*

class ParenthesesSuite extends FunSuite {
  private given Dialect = dialects.Scala3

  private def structure(value: Any): Any = value match {
    case Term.Block(List(term: Term)) => structure(term)
    case tree: Tree => (tree.productPrefix, tree.productIterator.map(structure).toList)
    case values: List[?] => values.map(structure)
    case value: Option[?] => value.map(structure)
    case other => other
  }

  private def format(source: String): String = {
    val result = Formatter.format(source).fold(error => fail(error.getMessage), identity)
    val output = {
      given Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
      result.parse[Source].get
    }
    assertEquals(structure(output), structure(source.parse[Source].get), result)
    assertEquals(Formatter.format(result), Right(result))
    assertEquals(
      result.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
      source.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
    )
    result
  }

  test("parenthesized lambda arguments retain their braced bodies") {
    val result = format("object X { val result = values.map((value: Int) => { val next = value + 1; next }) }")
    assert(result.contains("val result = values.map(\n    (value: Int) =>\n      {\n"), result)
  }

  test("untyped lambda parameters omit redundant parentheses") {
    val result = format("object X { val result = values.map((value) => value + 1) }")
    assert(result.contains("values.map(value => value + 1)"), result)
  }

  test("short multiple parameter clauses stay on the header line") {
    val result = format("object X { val result = combine((left: Int, right: Int) => { use(left); use(right) }) }")
    assert(result.contains("combine("), result)
    assert(result.contains("(left: Int, right: Int) =>\n      {\n"), result)
  }

  test("oversized lambda parameter clauses can wrap") {
    val parameters = (1 to 6).map(index => s"parameterNumber$index: LongParameterType").mkString(", ")
    val result = format(s"object X { val result = combine(($parameters) => parameterNumber1) }")
    assert(result.contains("combine("), result)
    assert(result.linesIterator.forall(_.length <= 100), result)
  }

  test("single block and partial function arguments retain parentheses around braces") {
    val result = format("object X { val a = run({ prepare(); value }); val b = values.collect({ case Some(value) => value }) }")
    assert(result.contains("run("), result)
    assert(result.contains("values.collect("), result)
    assertEquals(result.count(_ == '{'), 3, result)
  }

  test("multiple arguments retain required delimiters") {
    format("object X { val result = combine({ prepare(); value }, (left: Int, right: Int) => left + right) }")
  }

  test("constructor block arguments retain constructor argument parentheses") {
    val result = format("object X { val result = new Service({ prepare(); value }); val f = new Service((value: Int) => value) }")
    assert(result.contains("new Service("), result)
  }

  test("infix single-expression lambda arguments retain parentheses") {
    val result = format("object X { val result = values map ((value: Int) => value + 1) }")
    assert(result.contains("values map ((value: Int) => value + 1)"), result)
  }

  test("single-expression polymorphic lambda arguments stay parenthesized on repeated formatting") {
    val result = format("object X { val result = run([A] => (value: A) => value) }")
    assert(result.contains("run([A] => (value: A) => value)"), result)
  }

  test("lambda footer line comments remain inside argument braces before enclosing delimiters") {
    val result = format("object X { val result = build(values.map { value => transform(value) // footer\n}) }")
    assert(result.contains("// footer\n"), result)
  }

  test("expression operator precedence retains exactly necessary grouping") {
    val result = format("object X { val a = -x + y * z; val b = (x + y) * z; val c = x - (y - z); val d = a :: b :: c; val e = (-1).abs }")
    assert(result.contains("-x + y * z"), result)
    assert(result.contains("(x + y) * z"), result)
    assert(result.contains("x - (y - z)"), result)
    assert(result.contains("a :: b :: c"), result)
    assert(result.contains("(-1).abs"), result)
  }

  test("type and pattern precedence avoids blanket parentheses") {
    val result = format("object X { type F = A | B & C; type G = (A | B) & C; type H = Int => String; val result = xs match { case Head(x) :: tail => x; case _ => 0 } }")
    assert(result.contains("type F = A | B & C"), result)
    assert(result.contains("type G = (A | B) & C"), result)
    assert(result.contains("type H = Int => String"), result)
    assert(result.contains("case Head(x) :: tail =>"), result)
  }

  test("block qualifiers and lambdas preserve comments") {
    val result = format("object X { val x = { prepare(); value }.toString; val result = run { value => /* before */ val next = value + 1; /* after */ next } }")
    assert(!result.contains("({"), result)
    assert(result.contains("run{ value =>"), result)
  }

  test("lambda parameter comments survive inside parenthesized arguments") {
    val result = format("object X { val result = values.map((/* before */ value // after\n) => value + 1) }")
    assert(result.contains("values.map("), result)
  }
}
