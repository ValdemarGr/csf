package csf

import munit.FunSuite
import scala.meta.*

class StripMarginSuite extends FunSuite {
  private given Dialect = dialects.Scala3

  private def structure(value: Any): Any = value match {
    case Term.Block(List(term: Term)) => structure(term)
    case Term.Select(literal: Lit.String, Term.Name("stripMargin")) =>
      ("stripMargin", literal.value.stripMargin)
    case Term.Select(interpolation: Term.Interpolate, Term.Name("stripMargin")) =>
      ("stripMargin", structure(interpolation.prefix),
        interpolation.parts.map(_.value.toString.stripMargin), structure(interpolation.args))
    case tree: Tree => (tree.productPrefix, tree.productIterator.map(structure).toList)
    case values: List[?] => values.map(structure)
    case value: Option[?] => value.map(structure)
    case other => other
  }

  private def format(source: String): String = {
    val result = Formatter.format(source).fold(error => fail(error.getMessage), identity)
    assertEquals(structure(result.parse[Source].get), structure(source.parse[Source].get), result)
    assertEquals(
      result.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
      source.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
    )
    assertEquals(Formatter.format(result), Right(result))
    result
  }

  test("plain string margins align with their first pipe") {
    assertEquals(format("val text = \"\"\"|first\n |second\n         |third\"\"\".stripMargin"),
      "val text =\n  \"\"\"|first\n     |second\n     |third\"\"\".stripMargin\n")
  }

  test("missing margins preserve every original space") {
    assertEquals(format("val text = \"\"\"|first\n    keep these exact spaces\n     |third\"\"\".stripMargin"),
      "val text =\n  \"\"\"|first\n     |    keep these exact spaces\n     |third\"\"\".stripMargin\n")
  }

  test("without an opening pipe margins align with the last opening quote") {
    assertEquals(format("val text = \"\"\"first\n |second\n |\"\"\".stripMargin"),
      "val text =\n  \"\"\"first\n    |second\n    |\"\"\".stripMargin\n")
  }

  test("blank lines trailing spaces tabs and unmarked closing lines retain their values") {
    val result = format("val text = \"\"\"|first\n\n  \n\tkeep  \n\t |last\n  \"\"\".stripMargin")
    assertEquals(result,
      "val text =\n  \"\"\"|first\n     |\n     |  \n     |\tkeep  \n     |last\n     |  \"\"\".stripMargin\n")
  }

  test("interpolated margins align after the prefix and normalize missing margins") {
    assertEquals(format("val text = s\"\"\"|first\n |$name\n    plain $other\n |\"\"\".stripMargin"),
      "val text =\n  s\"\"\"|first\n      |${name}\n      |    plain ${other}\n      |\"\"\".stripMargin\n")
    assertEquals(format("val text = raw\"\"\"first\n |$name\n |\"\"\".stripMargin"),
      "val text =\n  raw\"\"\"first\n       |${name}\n       |\"\"\".stripMargin\n")
  }

  test("nested strings and trailing chains retain aligned margins") {
    val result = format("object Example { val text = \"\"\"|first\n |second\"\"\".stripMargin.trim }")
    assert(result.contains("    \"\"\"|first\n       |second\"\"\""), result)
    val argument = format("val text = consume(\"\"\"|first\n |second\"\"\".stripMargin)")
    assertEquals(argument,
      "val text =\n  consume(\n    \"\"\"|first\n       |second\"\"\".stripMargin\n  )\n")
  }

  test("interpolation expression lines do not gain margin characters") {
    val result = format("val text = s\"\"\"|${val next = prepare(); finish(next)}\n |done\"\"\".stripMargin")
    assert(result.contains("val next = prepare()"), result)
    assert(!result.contains("|val next"), result)
    assert(result.contains("\n      |done\"\"\".stripMargin"), result)
  }

  test("multiline literals inside interpolation expressions preserve their contents") {
    format("val text = s\"\"\"|${\"\"\"first\n  second\"\"\"}\n |done\"\"\".stripMargin")
    format("val text = s\"\"\"|${s\"\"\"first\n  $name\"\"\"}\n |done\"\"\".stripMargin")
    format("val text = s\"\"\"|${<node>first\n  second</node>}\n |done\"\"\".stripMargin")
    format("val text = s\"\"\"|${/* first\n  second */ name}\n |done\"\"\".stripMargin")
  }

  test("unmarked lines beginning with splices retain their original margins") {
    List("", "    ").foreach { spaces =>
      val result = format("val text = s\"\"\"|first\n" + spaces + "${\"|second\"}\n |done\"\"\".stripMargin")
      assertEquals(result,
        "val text =\n  s\"\"\"|first\n" + spaces + "${\"|second\"}\n |done\"\"\".stripMargin\n")
    }
  }

  test("other multiline strings and explicit margin arguments retain their contents") {
    List("", ".trim", ".stripMargin('#')", ".stripMargin('|')", ".stripMargin()").foreach { suffix =>
      val literal = "\"\"\"|first\n |second\n    unmarked\"\"\""
      val result = format("val text = " + literal + suffix)
      assert(result.contains(literal), result)
    }
    assertEquals(format("val text = \"first\\n |second\".stripMargin"),
      "val text = \"first\\n |second\".stripMargin\n")
  }
}
