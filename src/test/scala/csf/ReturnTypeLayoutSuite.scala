package csf

import munit.FunSuite
import scala.meta.*

class ReturnTypeLayoutSuite extends FunSuite {
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

  test("getUniqueHxl expands its first parameter clause before moving its return type") {
    val source = "object Queries{def getUniqueHxl[K:TypeId,A:Select:Unique[*,K]:TypeId]" +
      "(o:Org,k:K)(using Ses):Hxl[IO,A]=getUniqueHxlOpt[K,A](o,k).map(_.get)}"
    val result = format(source)

    assertEquals(result,
      "object Queries {\n" +
        "  def getUniqueHxl[K: TypeId, A: Select: Unique[*, K]: TypeId](\n" +
        "    o: Org,\n    k: K\n" +
        "  )(using Ses): Hxl[IO, A] = getUniqueHxlOpt[K, A](o, k).map{ _.get }\n" +
        "}\n")
    assert(result.linesIterator.forall(_.length <= 100), result)
  }

  test("short method definitions and declarations keep return types inline") {
    assertEquals(format("def value:Int=next"), "def value: Int = next\n")
    assertEquals(format("trait Api{def value:Result[Int]}"),
      "trait Api {\n  def value: Result[Int]\n}\n")
  }

  test("an abstract method return type fitting exactly 100 columns stays inline") {
    val name = "m" * (100 - "  def : Result".length)
    val line = s"  def $name: Result"

    assertEquals(line.length, 100)
    assertEquals(format(s"trait Api{def $name:Result}"), s"trait Api {\n$line\n}\n")
  }

  test("a 101-column abstract method return type breaks after its colon") {
    val name = "m" * (101 - "  def : Result".length)

    assertEquals(s"  def $name: Result".length, 101)
    assertEquals(format(s"trait Api{def $name:Result}"),
      s"trait Api {\n  def $name:\n    Result\n}\n")
  }

  test("return type arguments wrap only when the type remains too wide on its own line") {
    val result = format("def result:Either[FirstLongArgument,SecondLongArgument]=value", 40)

    assertEquals(result,
      "def result:\n  Either[\n    FirstLongArgument,\n    SecondLongArgument\n  ] = value\n")
    assert(result.linesIterator.forall(_.length <= 40), result)
  }

  test("nested method return types nest two spaces without shifting surrounding closing braces") {
    assertEquals(format("object Outer{trait Inner{def longMethodName:Result[LongValue]}}", 40),
      "object Outer {\n  trait Inner {\n    def longMethodName:\n" +
        "      Result[LongValue]\n  }\n}\n")
  }

  test("return type comments retain their token contents and ownership across definitions and declarations") {
    assertEquals(format("def value:/* result */Result=next"),
      "def value:\n  /* result */\n  Result = next\n")
    assertEquals(format("def value:// result\n Result=next"),
      "def value:\n  // result\n  Result = next\n")
    assertEquals(format("trait Api{def value:/* first */// result\n Result}"),
      "trait Api {\n  def value:\n    /* first */\n    // result\n    Result\n}\n")
  }
}
