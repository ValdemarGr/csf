package csf

import munit.FunSuite
import scala.meta.*

class MethodSignatureLayoutSuite extends FunSuite {
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

  test("short method signatures stay inline across ordinary and contextual clauses") {
    assertEquals(format("def f(value:Int)(using ctx:Context):Result=value"),
      "def f(value: Int)(using ctx: Context): Result = value\n")
    assertEquals(format("trait Api{def f(value:Int)(using Context):Result}"),
      "trait Api {\n  def f(value: Int)(using Context): Result\n}\n")
  }

  test("insert breaks its first parameter clause before contextual parameters or return type") {
    val source = "object Repository{def insert(header:CommandHeader,token:M2MToken)" +
      "(using ses:Ses,xa:Transaction[IO]):IO[Unit]=DB.insert(NonEmptyChain.one(header->token))}"

    assertEquals(format(source),
      "object Repository {\n  def insert(\n    header: CommandHeader,\n    token: M2MToken\n" +
        "  )(using ses: Ses, xa: Transaction[IO]): IO[Unit] = " +
        "DB.insert(NonEmptyChain.one(header -> token))\n}\n")
  }

  test("a signature fitting exactly 100 columns keeps its first clause inline") {
    val name = "m" * (100 - "  def (value: Int): Result".length)
    val line = s"  def $name(value: Int): Result"

    assertEquals(line.length, 100)
    assertEquals(format(s"trait Api{def $name(value:Int):Result}"),
      s"trait Api {\n$line\n}\n")
  }

  test("a 101-column signature breaks inside its first clause while keeping the opening attached") {
    val name = "m" * (101 - "  def (value: Int): Result".length)

    assertEquals(s"  def $name(value: Int): Result".length, 101)
    assertEquals(format(s"trait Api{def $name(value:Int):Result}"),
      s"trait Api {\n  def $name(\n    value: Int\n  ): Result\n}\n")
  }

  test("later parameter clauses break left to right before a still-fitting return type") {
    val source = "def method(first:First)(second:Second)(using third:Third):Result=value"

    assertEquals(format(source, 40),
      "def method(\n  first: First\n)(\n  second: Second\n" +
        ")(using third: Third): Result = value\n")
    assertEquals(format("def method(first:First)(second:Second)(using third:ThirdArgumentType):Result=value", 40),
      "def method(\n  first: First\n)(\n  second: Second\n)(\n" +
        "  using third: ThirdArgumentType\n): Result = value\n")
  }

  test("return types break only after all earlier parameter clauses have been expanded") {
    val source = "trait Api{def lookup(first:First)(second:Second):Either[VeryLongFirstResult,VeryLongSecondResult]}"

    assertEquals(format(source, 40),
      "trait Api {\n  def lookup(\n    first: First\n  )(\n    second: Second\n  ):\n" +
        "    Either[\n      VeryLongFirstResult,\n      VeryLongSecondResult\n    ]\n}\n")
  }

  test("nested abstract signatures retain member indentation and lexical comment ownership") {
    val source = "object Outer{trait Api{def lookup(first:First)(second:Second):Result}}"
    val result = format(source, 40)

    assertEquals(result,
      "object Outer {\n  trait Api {\n    def lookup(\n      first: First\n" +
        "    )(second: Second): Result\n  }\n}\n")
    assertEquals(format("object Outer{trait Api{def lookup(/* first */first:First)(// second\n second:Second):/* result */Result}}", 40),
      "object Outer {\n  trait Api {\n    def lookup(\n      /* first */\n      first: First\n" +
        "    )(\n      // second\n      second: Second\n    ):\n      /* result */\n      Result\n  }\n}\n")
  }
}
