package csf

import munit.FunSuite
import scala.meta.*

class CaseClassFieldLayoutSuite extends FunSuite {
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
    assert(result.linesIterator.forall(line => line == line.stripTrailing()), result)
    result
  }

  test("two or more first-clause case-class fields are vertical even when the complete class fits") {
    assertEquals(format("case class Pair(left:Int,right:String)"),
      "case class Pair(\n  left: Int,\n  right: String\n)\n")
    assertEquals(format("case class Triple(first:Int,second:String,third:Boolean)"),
      "case class Triple(\n  first: Int,\n  second: String,\n  third: Boolean\n)\n")
  }

  test("a single case-class field stays inline when it fits") {
    assertEquals(format("case class Box[A,B](value:A)"), "case class Box[A, B](value: A)\n")
  }

  test("nested final generic case classes keep type parameters inline and preserve member indentation") {
    assertEquals(format("object Outer{final case class Entry[A,B](first:A,second:B){val key=first}}"),
      "object Outer {\n  final case class Entry[A, B](\n    first: A,\n    second: B\n" +
        "  ) {\n    val key = first\n  }\n}\n")
  }

  test("field defaults and leading type and footer comments retain their contents and nesting") {
    assertEquals(format("case class Entry(first:Int={prepare();1},second:String=\"two\")"),
      "case class Entry(\n  first: Int = {\n    prepare()\n    1\n  },\n" +
        "  second: String = \"two\"\n)\n")
    assertEquals(format("case class Entry(/* leading */first:Int,second:/* type */String // footer\n)"),
      "case class Entry(\n  /* leading */\n  first: Int,\n  second:\n  /* type */\n  String\n  // footer\n)\n")
  }

  test("a one-field first clause does not force a later two-parameter clause vertical") {
    assertEquals(format("case class Entry(first:Int)(second:Int,third:Int)"),
      "case class Entry(first: Int)(second: Int, third: Int)\n")
  }

  test("a vertical first field clause leaves a fitting later contextual clause inline") {
    assertEquals(format("case class Entry(first:Int,second:String)(using ctx:Context,other:Other)"),
      "case class Entry(\n  first: Int,\n  second: String\n)(using ctx: Context, other: Other)\n")
  }

  test("ordinary classes and method definitions and declarations retain their width-based layout") {
    assertEquals(format("class Entry(first:Int,second:String){}"),
      "class Entry(first: Int, second: String) {}\n")
    assertEquals(format("def f(first:Int,second:String):Int=first"),
      "def f(first: Int, second: String): Int = first\n")
    assertEquals(format("trait Api{def f(first:Int,second:String):Int}"),
      "trait Api {\n  def f(first: Int, second: String): Int\n}\n")
  }
}
