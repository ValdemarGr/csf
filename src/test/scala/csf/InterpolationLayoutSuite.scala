package csf

import munit.FunSuite
import scala.meta.*

class InterpolationLayoutSuite extends FunSuite {
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

  test("short names arithmetic and method calls keep compact interpolation braces") {
    assertEquals(format("val result=s\"hello $name\""), "val result = s\"hello ${name}\"\n")
    assertEquals(format("val result=s\"sum ${left+right}\""), "val result = s\"sum ${left + right}\"\n")
    assertEquals(format("val result=s\"value ${service.lookup(id)}\""),
      "val result = s\"value ${service.lookup(id)}\"\n")
  }

  test("short interpolation expressions collapse existing newlines") {
    assertEquals(format("val result=s\"before ${\n  value\n} after\""),
      "val result = s\"before ${value} after\"\n")
  }

  test("an interpolation fitting exactly 100 columns includes its surrounding string in the width") {
    val name = "x" * (100 - "val result = s\"before ${} after\"".length)
    val expected = "val result = s\"before ${" + name + "} after\"\n"

    assertEquals(expected.linesIterator.next().length, 100)
    assertEquals(format("val result=s\"before ${" + name + "} after\""), expected)
  }

  test("an overflowing val moves its interpolation below equals before breaking its braces") {
    val name = "x" * (101 - "val result = s\"before ${} after\"".length)
    val source = "val result=s\"before ${" + name + "} after\""

    assertEquals(format(source), "val result =\n  s\"before ${" + name + "} after\"\n")
  }

  test("an interpolation exceeding 100 columns on its own line breaks its braces") {
    val name = "x" * (101 - "  s\"before ${} after\"".length)
    val source = "val result=s\"before ${" + name + "} after\""

    assertEquals(format(source), "val result =\n  s\"before ${\n    " + name + "\n  } after\"\n")
  }

  test("multistatement interpolation bodies retain one pair of braces") {
    assertEquals(format("val result=s\"value ${val next=prepare();finish(next)} done\""),
      "val result =\n  s\"value ${\n    val next = prepare()\n    finish(next)\n  } done\"\n")
  }

  test("block arguments inside interpolation retain their required braces without wrapping another block") {
    val result = format("val result=s\"${service.run({prepare();value})}\"")

    assert(result.startsWith("val result =\n  s\"${\n"), result)
    assert(result.contains("service.run {\n"), result)
    assert(!result.contains("${\n    {"), result)
  }

  test("all comment kinds force standalone lines inside interpolation") {
    assertEquals(format("val result=s\"${ // note\nvalue}\""),
      "val result =\n  s\"${\n    // note\n    value\n  }\"\n")
    assertEquals(format("val result=s\"${/* note */value}\""),
      "val result =\n  s\"${\n    /* note */\n    value\n  }\"\n")
    assertEquals(format("val result=s\"${ // note\n}\""),
      "val result =\n  s\"${\n    // note\n  }\"\n")
    assertEquals(format("val result=s\"${value // footer\n}\""),
      "val result =\n  s\"${\n    value\n    // footer\n  }\"\n")
    assertEquals(format("val result=s\"${}\""), "val result = s\"${}\"\n")
  }

  test("literal dollars escaped quotes and raw backslashes retain their contents") {
    assertEquals(format("val result=s\"$$price: ${amount} EUR\""),
      "val result = s\"$$price: ${amount} EUR\"\n")
    assertEquals(format("val result=s\"say \\\"hi\\\" to ${name}\""),
      "val result = s\"say \\\"hi\\\" to ${name}\"\n")
    assertEquals(format("val result=raw\"C:\\tmp\\${name}\""),
      "val result = raw\"C:\\tmp\\${name}\"\n")
  }

  test("triple quoted literal newlines and indentation remain data inside nested definitions") {
    val source = "object Example{val result=s\"\"\"first\n  ${name}\nlast\"\"\"}"
    val expected = "object Example {\n  val result =\n    s\"\"\"first\n  ${name}\nlast\"\"\"\n}\n"

    assertEquals(format(source), expected)
  }

  test("interpolation pattern braces stay compact") {
    assertEquals(format("val result=input match{case s\"value ${name}\"=>name}"),
      "val result =\n  input match {\n    case s\"value ${name}\" => name\n  }\n")
  }

  test("XML splices retain their existing multiline brace layout") {
    assertEquals(format("val result = <node>{name}</node>"),
      "val result =\n  <node>{\n    name\n  }</node>\n")
  }
}
