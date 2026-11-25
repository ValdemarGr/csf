package csf

import munit.FunSuite
import scala.meta.*

class ParenthesizedLayoutSuite extends FunSuite {
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
    assertEquals(structure(reparsed), structure(source.parse[Source].get))
    assertEquals(
      result.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
      source.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
    )
    assertEquals(Formatter.format(result), Right(result))
  }

  test("short argument and parameter clauses collapse existing newlines") {
    check("val result=call(\nfirst,\nsecond\n)", "val result = call(first, second)\n")
    check("def combine(\na: Int,\nb: Int\n)=a", "def combine(a: Int, b: Int) = a\n")
  }

  test("a parenthesized call fitting exactly 100 columns stays inline") {
    val text = "x" * (100 - "val result = call(\"\")".length)
    val expected = s"val result = call(\"$text\")\n"

    assertEquals(expected.linesIterator.next().length, 100)
    check(s"val result=call(\"$text\")", expected)
  }

  test("a parenthesized call one column over keeps its opener inline") {
    val text = "x" * (101 - "val result = call(\"\")".length)

    check(s"val result=call(\"$text\")", s"val result = call(\n  \"$text\"\n)\n")
  }

  test("a longer argument wraps under the inline call opener") {
    val text = "x" * (101 - "  call(\"\")".length)

    assertEquals(s"  call(\"$text\")".length, 101)
    check(s"val result=call(\"$text\")", s"val result = call(\n  \"$text\"\n)\n")
  }

  test("a multiline chain forces broken argument delimiters and one item per line") {
    val receiver = "receiver" + "x" * 86
    check(s"val result=combine($receiver.map(f).filter(p),other)",
      s"val result = combine(\n  $receiver\n    .map(f).filter(p),\n  other\n)\n")
  }

  test("long expression pattern and type tuples use one item per line") {
    val first = "first" + "Identifier" * 6
    val second = "second" + "Identifier" * 6
    val firstType = "First" + "TypeName" * 7
    val secondType = "Second" + "TypeName" * 7

    check(s"val result=($first,$second)", s"val result =\n  (\n    $first,\n    $second\n  )\n")
    check(s"val ($first,$second)=input", s"val (\n  $first,\n  $second\n) = input\n")
    check(s"type Result=($firstType,$secondType)", s"type Result =\n  (\n    $firstType,\n    $secondType\n  )\n")
  }

  test("long method parameter clauses align their closing delimiter with the definition") {
    val tpe = "VeryLongParameterTypeForThisExample"
    check(s"def combine(firstArgument:$tpe,secondArgument:$tpe)=firstArgument",
      s"def combine(\n  firstArgument: $tpe,\n  secondArgument: $tpe\n) = firstArgument\n")
  }

  test("long lambda parameter clauses break inside argument parentheses") {
    val tpe = "VeryLongParameterTypeForThisExample"
    check(s"val result=combine((firstArgument:$tpe,secondArgument:$tpe)=>firstArgument)",
      s"val result = combine(\n  (\n    firstArgument: $tpe,\n    secondArgument: $tpe\n  ) => firstArgument\n)\n")
  }

  test("a required grouped multiline expression breaks its parentheses") {
    check("val result=(if(flag)first else second).value",
      "val result =\n  (\n    if flag then {\n      first\n    } else {\n      second\n    }\n  ).value\n")
  }

  test("a line comment forces broken grouping while preserving required parentheses") {
    check("val result=(// grouped\nleft+right).value",
      "val result =\n  (\n    // grouped\n    left + right\n  ).value\n")
  }

  test("a multiline block argument keeps both required braces and broken outer parentheses") {
    check("val result=combine({prepare();value},other)",
      "val result = combine(\n  {\n    prepare()\n    value\n  },\n  other\n)\n")
  }
}
