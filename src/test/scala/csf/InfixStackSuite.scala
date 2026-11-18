package csf

import munit.FunSuite
import scala.meta.*

class InfixStackSuite extends FunSuite {
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
    val reparsed = {
      given Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
      result.parse[Source].get
    }
    assertEquals(structure(reparsed), structure(source.parse[Source].get), result)
    assertEquals(
      result.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
      source.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
    )
    assertEquals(Formatter.format(result), Right(result))
    result
  }

  test("a 256-level left-associative infix expression preserves its complete operand tree") {
    val operands = (0 to 256).map(index => s"value$index")

    format("val result=" + operands.mkString(" + "))
  }

  test("a 256-level right-associative infix expression preserves its complete operand tree") {
    val operands = (0 to 256).map(index => s"value$index")

    format("val result=" + operands.mkString(" :: "))
  }

  test("mixed precedence and original operand wrappers retain necessary grouping") {
    val left = (0 to 128).map(index => s"left$index").mkString(" + ")
    val right = (0 to 128).map(index => s"right$index").mkString(" - ")

    assertEquals(format(s"val result=({{$left}})*(base-{{$right}})"),
      format(s"val result=($left)*(base-($right))"))
    assertEquals(format("val result=(left+right)*value-(other-last)"),
      "val result = (left + right) * value - (other - last)\n")
    assertEquals(format("val result=(head::tail)::rest"), "val result = (head :: tail) :: rest\n")
  }

  test("deep infix operands and operators preserve block line and footer comments in lexical order") {
    val prefix = (0 until 256).map { index =>
      val following = if (index % 64 == 0) s"// after operator $index\n  " else ""
      s"value$index /* before operator $index */ + $following"
    }.mkString

    format("val result=/* leading */" + prefix + "last // footer\n")
  }
}
