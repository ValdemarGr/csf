package csf

import munit.FunSuite
import scala.meta.*

class ValBodyLayoutSuite extends FunSuite {
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
    val reparsed = {
      given Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
      result.parse[Source].get
    }
    assertEquals(structure(reparsed), structure(source.parse[Source].get), result)
    assertEquals(
      result.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
      source.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
    )
    assertEquals(Formatter.format(result, columns), Right(result))
    result
  }

  test("short typed lazy and destructuring vals keep their RHS inline") {
    assertEquals(format("val answer=42"), "val answer = 42\n")
    assertEquals(format("val answer:Int=42"), "val answer: Int = 42\n")
    assertEquals(format("lazy val answer:Int=42"), "lazy val answer: Int = 42\n")
    assertEquals(format("val(left,right)=(first,second)"), "val (left, right) = (first, second)\n")
  }

  test("a RHS fitting exactly 100 columns remains after equals") {
    val text = "x" * (100 - "val result = \"\"".length)
    val expected = s"val result = \"$text\"\n"

    assertEquals(expected.linesIterator.next().length, 100)
    assertEquals(format(s"val result=\"$text\""), expected)
  }

  test("a RHS exceeding the configured width moves two spaces below equals") {
    List(40, 100).foreach { columns =>
      val text = "x" * (columns + 1 - "val result = \"\"".length)

      assertEquals(format(s"val result=\"$text\"", columns), s"val result =\n  \"$text\"\n")
    }
  }

  test("call arguments wrap before the opening line moves below equals") {
    val source = "val x = MyDatatype(first, second, third)"
    val columns = "val x = MyDatatype(".length

    assertEquals(format(source, columns),
      "val x = MyDatatype(\n  first,\n  second,\n  third\n)\n")
    assertEquals(format(source, columns - 1),
      "val x =\n  MyDatatype(\n    first,\n    second,\n    third\n  )\n")
    assertEquals(format("val x = /* call */ MyDatatype(first, second, third)"),
      "val x =\n  /* call */\n  MyDatatype(first, second, third)\n")
  }

  test("mandatory multiline conditional and match RHS expressions start below equals") {
    assertEquals(format("val result=if(ready)use(value)else fallback"),
      "val result =\n  if ready then {\n    use(value)\n  } else {\n    fallback\n  }\n")
    assertEquals(format("val result=input match{case Some(value)=>use(value);case _=>fallback}"),
      "val result =\n  input match {\n    case Some(value) => use(value)\n    case _ => fallback\n  }\n")
  }

  test("short lambda RHS expressions stay inline while block lambdas own their braces") {
    assertEquals(format("val f=(value:Int)=>use(value)"), "val f = (value: Int) => use(value)\n")
    assertEquals(format("val f=value=>{prepare(value);use(value)}"),
      "val f = { value =>\n  prepare(value)\n  use(value)\n}\n")
  }

  test("retained multistatement empty and declaration-only blocks start beside equals") {
    assertEquals(format("val result={val next=prepare();use(next)}"),
      "val result = {\n  val next = prepare()\n  use(next)\n}\n")
    assertEquals(format("val result={}"), "val result = {\n}\n")
    assertEquals(format("val result={val next=prepare()}"),
      "val result = {\n  val next = prepare()\n}\n")
  }

  test("nested singleton source blocks share the direct RHS layout") {
    assertEquals(format("val answer={{{42}}}"), format("val answer=42"))
    assertEquals(format("val result={{{use(value)}}}"), format("val result=use(value)"))
  }

  test("typed partial-function vals attach their brace to equals and preserve blank case gaps") {
    val source = "object dsl { val grpcStatus: PartialFunction[Throwable, io.grpc.Status] =\n {\n" +
      "case e: io.grpc.StatusException => e.getStatus()\n\n" +
      "case e: io.grpc.StatusRuntimeException => e.getStatus()\n} }"
    val expected = "object dsl {\n  val grpcStatus: PartialFunction[Throwable, io.grpc.Status] = {\n" +
      "    case e: io.grpc.StatusException => e.getStatus()\n\n" +
      "    case e: io.grpc.StatusRuntimeException => e.getStatus()\n  }\n}\n"

    assertEquals(format(source), expected)
  }

  test("partial-function val wrappers normalize while leading and footer comments stay inside braces") {
    val source = "val recover={{{\n  // source\n  case value=>use(value)\n  // footer\n}}}"

    assertEquals(format(source), "val recover = {\n  // source\n  case value =>\n    use(value)\n    // footer\n}\n")
  }

  test("leading line and footer comments retain their contents and RHS placement") {
    assertEquals(format("val answer=/* body */42"), "val answer =\n  /* body */\n  42\n")
    assertEquals(format("val answer=// note\n  42"), "val answer =\n  // note\n  42\n")
    assertEquals(format("val answer={42 // footer\n}"), "val answer = {\n  42\n  // footer\n}\n")
  }

  test("nested vals retain their indentation while triple-quoted literal lines remain data") {
    assertEquals(format("object Example{val text:String=\"\"\"first\n  second\nlast\"\"\"}"),
      "object Example {\n  val text: String =\n    \"\"\"first\n  second\nlast\"\"\"\n}\n")
    assertEquals(format("object Example{val outer={lazy val inner:Int=if(ready)one else other;val(left,right)=(first,second);use(inner,left,right)}}"),
      "object Example {\n  val outer = {\n    lazy val inner: Int =\n      if ready then {\n        one\n      } else {\n        other\n      }\n    val (left, right) = (first, second)\n    use(inner, left, right)\n  }\n}\n")
  }
}
