package csf

import munit.FunSuite
import scala.meta.*

class InfixChainLayoutSuite extends FunSuite {
  private given Dialect = dialects.Scala3

  private def structure(value: Any): Any = value match {
    case Term.Block(List(term: Term)) => structure(term)
    case tree: Tree => (tree.productPrefix, tree.productIterator.map(structure).toList)
    case values: List[?] => values.map(structure)
    case value: Option[?] => value.map(structure)
    case other => other
  }

  private def format(source: String, columns: Int = 100): String = {
    val result = Formatter.format(source, columns).fold(error => fail(error.getMessage), identity)
    val original = source.parse[Source].get
    assertEquals(structure(result.parse[Source].get), structure(original), result)
    val reparsed = {
      given Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
      result.parse[Source].get
    }
    assertEquals(structure(reparsed), structure(original), result)
    assertEquals(
      result.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
      source.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
    )
    assertEquals(Formatter.format(result, columns), Right(result))
    result
  }

  test("short expression chains stay inline and collapse existing continuation breaks") {
    val expected = "val codec = first *: second *: last\n"

    assertEquals(format("val codec=first *: second *: last"), expected)
    assertEquals(format("val codec=first *:\n second *:\n last"), expected)
  }

  test("a complete chain fitting exactly 100 columns stays after equals") {
    val receiver = "r" * (100 - "val codec =  *: second *: last".length)
    val expected = s"val codec = $receiver *: second *: last\n"

    assertEquals(expected.linesIterator.next().length, 100)
    assertEquals(format(s"val codec=$receiver *: second *: last"), expected)
  }

  test("a 101-column assignment moves its still-fitting chain below equals before splitting operators") {
    val receiver = "r" * (101 - "val codec =  *: second *: last".length)

    assertEquals(s"val codec = $receiver *: second *: last".length, 101)
    assertEquals(format(s"val codec=$receiver *: second *: last"),
      s"val codec =\n  $receiver *: second *: last\n")
  }

  test("a chain exceeding 100 columns on its own line splits every RHS at one shared indent") {
    val receiver = "r" * (101 - "   *: second *: last".length)
    val result = format(s"val codec=$receiver *: second *: last")

    assertEquals(s"  $receiver *: second *: last".length, 101)
    assertEquals(result, s"val codec =\n  $receiver *:\n    second *:\n    last\n")
    assert(result.linesIterator.forall(_.length <= 100), result)
  }

  test("the codec chain wraps all RHS operands without progressively increasing indentation") {
    val source = "val codec=UID.skunkCodec[Contract] *: date *: date.opt *: InvoiceType.skunkCodec.opt *: CostId.skunkCodec"

    assertEquals(format(source, 60),
      "val codec =\n  UID.skunkCodec[Contract] *:\n    date *:\n    date.opt *:\n" +
        "    InvoiceType.skunkCodec.opt *:\n    CostId.skunkCodec\n")
  }

  test("a 256-level chain preserves its operand tree with a constant continuation indent") {
    val operands = (0 to 256).map(index => s"value$index")
    val expected = "val codec =\n  " + operands.head + " *:\n" +
      operands.tail.mkString("    ", " *:\n    ", "\n")

    assertEquals(format("val codec=" + operands.mkString(" *: ")), expected)
  }

  test("parentheses and mixed precedence preserve right-associative operand grouping") {
    assertEquals(format("val codec=(first *: second) *: last"),
      "val codec = (first *: second) *: last\n")
    assertEquals(format("val codec=first *: (second+third) *: last"),
      "val codec = first *: (second + third) *: last\n")
    assertEquals(format("val codec=({{first *: second}}) *: last"),
      format("val codec=(first *: second) *: last"))
    format("val codec=first *: second + third *: last", 30)
  }

  test("mandatory multiline comments and operand docs retain comment order and chain grouping") {
    assertEquals(format("val codec=first *: /* keep\n exact */ second *: last"),
      "val codec =\n  first *:\n    /* keep\n exact */\n    second *:\n    last\n")
    assertEquals(format("val codec=first *: // next operand\n second *: last"),
      "val codec =\n  first *:\n    // next operand\n    second *:\n    last\n")
    assertEquals(format("val codec=first *: build({prepare();value}) *: last"),
      "val codec =\n  first *:\n    build(\n      {\n        prepare()\n        value\n      }\n    ) *:\n    last\n")
  }
}
