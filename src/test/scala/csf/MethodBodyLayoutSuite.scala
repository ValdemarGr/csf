package csf

import munit.FunSuite
import scala.meta.*

class MethodBodyLayoutSuite extends FunSuite {
  private given Dialect = dialects.Scala3

  private def structure(value: Any): Any = value match {
    case Term.Block(List(term: Term)) => structure(term)
    case tree: Tree => (tree.productPrefix, tree.productIterator.map(structure).toList)
    case values: List[?] => values.map(structure)
    case value: Option[?] => value.map(structure)
    case other => other
  }

  private def check(source: String, expected: String): Unit = {
    val result = Formatter.format(source).fold(error => fail(error.getMessage), identity)
    val reparsed = {
      given Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
      result.parse[Source].get
    }
    assertEquals(result, expected)
    assertEquals(structure(reparsed), structure(source.parse[Source].get), result)
    assertEquals(
      result.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
      source.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
    )
    assertEquals(Formatter.format(result), Right(result))
    assert(result.linesIterator.forall(line => line == line.stripTrailing()), result)
  }

  test("short inline and generic method bodies remain after equals when they fit") {
    check("def answer:Int=42", "def answer: Int = 42\n")
    check("object Example{inline def identity[A](value:A):A=value}",
      "object Example {\n  inline def identity[A](value: A): A = value\n}\n")
  }

  test("block bodies and nested method definitions retain their braces and scope") {
    check("def run()={prepare();finish()}",
      "def run() = {\n  prepare()\n  finish()\n}\n")
    check("def idle()={}", "def idle() = {\n}\n")
    check("def outer={def inner[A](value:A)=value;inner(1)}",
      "def outer = {\n  def inner[A](value: A) = value\n\n  inner(1)\n}\n")
  }

  test("typed partial-function methods attach their brace to equals and preserve blank case gaps") {
    val source = "object dsl { def grpcStatus: PartialFunction[Throwable, io.grpc.Status] =\n {\n" +
      "case e: io.grpc.StatusException => e.getStatus()\n\n" +
      "case e: io.grpc.StatusRuntimeException => e.getStatus()\n} }"
    val expected = "object dsl {\n  def grpcStatus: PartialFunction[Throwable, io.grpc.Status] = {\n" +
      "    case e: io.grpc.StatusException => e.getStatus()\n\n" +
      "    case e: io.grpc.StatusRuntimeException => e.getStatus()\n  }\n}\n"

    check(source, expected)
  }

  test("partial-function method wrappers normalize while leading and footer comments stay inside braces") {
    check("def recover={{{\n  // source\n  case value=>use(value)\n  // footer\n}}}",
      "def recover = {\n  // source\n  case value =>\n    use(value)\n    // footer\n}\n")
  }

  test("given and extension member methods use the same body layout") {
    check("given Service with{def run(value:Int):Int=value}",
      "given Service with {\n  def run(value: Int): Int = value\n}\n")
    check("extension(value:Int){def doubled:Int=value*2}",
      "extension (value: Int) {\n  def doubled: Int = value * 2\n}\n")
  }

  test("method body comments and multiline literal indentation remain intact") {
    check("def answer=/* body */42", "def answer =\n  /* body */\n  42\n")
    check("def text:String=\"\"\"first\n  second\nlast\"\"\"",
      "def text: String =\n  \"\"\"first\n  second\nlast\"\"\"\n")
    check("object Example{def first=1// footer\n// next method\ndef second=2}",
      "object Example {\n  def first = 1\n\n  // footer\n  // next method\n  def second = 2\n}\n")
  }

  test("collapseStatuses starts below equals while its short condition stays inline") {
    val source =
      """def collapseStatuses(statuses: List[AggregateStatus]): AggregateStatus = if (
        |    statuses.exists(_ == Failed)
        |  ) {
        |    Failed
        |  } else {
        |    if (statuses.isEmpty || statuses.exists(_ == Progress)) {
        |      Progress
        |    } else {
        |      Completed
        |    }
        |  }""".stripMargin
    val expected =
      """def collapseStatuses(statuses: List[AggregateStatus]): AggregateStatus =
        |  if (statuses.exists{ _ == Failed }) {
        |    Failed
        |  } else if (statuses.isEmpty || statuses.exists{ _ == Progress }) {
        |    Progress
        |  } else {
        |    Completed
        |  }
        |""".stripMargin

    check(source, expected)
  }

  test("method declarations without bodies retain their signature layout") {
    check("trait Service{def run(value:Int):Int}",
      "trait Service {\n  def run(value: Int): Int\n}\n")
  }
}
