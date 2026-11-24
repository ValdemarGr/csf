package csf

import munit.FunSuite
import scala.meta.*

class NewlineSuite extends FunSuite {
  private given Dialect = dialects.Scala3

  private def structure(value: Any): Any = value match {
    case Term.Block(List(term: Term)) => structure(term)
    case tree: Tree => (tree.productPrefix, tree.productIterator.map(structure).toList)
    case values: List[?] => values.map(structure)
    case value: Option[?] => value.map(structure)
    case other => other
  }

  private def comments(source: String): List[String] = {
    source.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList
  }

  private def check(source: String, expected: String): Unit = {
    val result = Formatter.format(source).fold(error => fail(error.getMessage), identity)
    val reparsed = {
      given Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
      result.parse[Source].get
    }

    assertEquals(result, expected)
    assertEquals(structure(reparsed), structure(source.parse[Source].get))
    assertEquals(comments(result), comments(source))
    assertEquals(Formatter.format(result), Right(result))
  }

  List(0, 3).foreach { blanks =>
    val gap = "\n" * (blanks + 1)
    val boundedGap = "\n" * (blanks.min(1) + 1)

    test(s"top-level imports and definitions retain at most one of $blanks source blank lines") {
      val source = List("import a.A", "import b.B", "def first=1", "def second=2").mkString(gap)
      val expected = List("import a.A", "import b.B", "def first = 1", "def second = 2").mkString(boundedGap) + "\n"

      check(source, expected)
    }

    test(s"mixed object members retain at most one of $blanks source blank lines around methods") {
      val source = "object Example{" + List("val first=1", "def second=2", "val third=3").mkString(gap) + "}"
      val expected = "object Example {\n" +
        List("  val first = 1", "  def second = 2", "  val third = 3").mkString(boundedGap) + "\n}\n"

      check(source, expected)
    }

    test(s"block expressions retain at most one of $blanks source blank lines") {
      val source = "object Example{def run()={" + List("first()", "second()", "third()").mkString(gap) + "}}"
      val expected = "object Example {\n  def run() = {\n" +
        List("    first()", "    second()", "    third()").mkString(boundedGap) + "\n  }\n}\n"

      check(source, expected)
    }

    test(s"match cases retain at most one of $blanks source blank lines") {
      val source = "object Example{val result=value match{" +
        List("case 0=>zero", "case 1=>one", "case _=>other").mkString(gap) + "}}"
      val expected = "object Example {\n  val result =\n    value match {\n" +
        List("      case 0 => zero", "      case 1 => one", "      case _ => other").mkString(boundedGap) + "\n    }\n}\n"

      check(source, expected)
    }

    test(s"for enumerators retain at most one of $blanks source blank lines") {
      val source = "object Example{val result=for{" +
        List("a<-source", "b=convert(a)", "c<-next(b)").mkString(gap) + "}yield c}"
      val expected = "object Example {\n  val result =\n    for {\n" +
        List("      a <- source", "      b = convert(a)", "      c <- next(b)").mkString(boundedGap) +
        "\n    } yield c\n}\n"

      check(source, expected)
    }

    List(
      ("class", "class Item", "class Item"),
      ("case class", "case class Item(value:Int)", "case class Item(value: Int)"),
      ("trait", "trait Item", "trait Item"),
      ("object", "object Item", "object Item"),
      ("enum", "enum Item{case Value}", "enum Item {\n  case Value\n}"),
      ("type alias", "type Item=Int", "type Item = Int"),
    ).foreach { case (kind, declaration, formatted) =>
      test(s"$kind declarations retain at most one of $blanks source blank lines on both sides") {
        val source = "object Example{val before=1" + gap + declaration + gap + "val after=2}"
        val expected = "object Example {\n  val before = 1" + boundedGap +
          formatted.linesIterator.map("  " + _).mkString("\n") + boundedGap + "  val after = 2\n}\n"

        check(source, expected)
      }
    }

    test(s"abstract methods and types retain at most one of $blanks source blank lines") {
      val source = "trait Example{" +
        List("val before:Int", "def value:Int", "type Item", "val after:Item").mkString(gap) + "}"
      val expected = "trait Example {\n" +
        List("  val before: Int", "  def value: Int", "  type Item", "  val after: Item").mkString(boundedGap) + "\n}\n"

      check(source, expected)
    }

    test(s"method docs stay attached with $blanks source blank lines before the comment batch") {
      val source = "object Example{val before=1" + gap +
        "/** Method docs. */\n\n// More detail.\n\n@deprecated def value=2" + gap + "val after=3}"
      val expected = "object Example {\n  val before = 1" + boundedGap + "  /** Method docs. */\n" +
        "  // More detail.\n  @deprecated def value = 2" + boundedGap + "  val after = 3\n}\n"

      check(source, expected)
    }
  }

  test("same-line sibling declarations acquire a newline without adding blank lines") {
    check("object Example{val first=1;val second=2}",
      "object Example {\n  val first = 1\n  val second = 2\n}\n")
  }

  test("same-line methods move to consecutive lines") {
    check("object Example{def first=1;def second=2}",
      "object Example {\n  def first = 1\n  def second = 2\n}\n")
  }

  test("same-line secondary constructors move to consecutive lines with other members") {
    check("class Example(seed:Int){val before=1;def this()=this(0);val after=2}",
      "class Example(seed: Int) {\n  val before = 1\n  def this() = {\n" +
        "    this(0)\n  }\n  val after = 2\n}\n")
  }

  test("same-line local methods and types move to consecutive lines with ordinary statements") {
    val source = "def run()={val first=1;def next()=first;val second=next();type Result=Int;" +
      "val result:Result=second;result}"
    val expected = "def run() = {\n  val first = 1\n  def next() = first\n  val second = next()\n" +
      "  type Result = Int\n  val result: Result = second\n  result\n}\n"

    check(source, expected)
  }

  test("leading and trailing scope padding disappears independently of interior grouping") {
    val source = "object Example{\n\n\ndef run()={\n\n\nfirst()\n\n\nsecond()\n\n\n}\n\n\n}"
    val expected = "object Example {\n  def run() = {\n    first()\n\n    second()\n  }\n}\n"

    check(source, expected)
  }

  test("multiline comment bytes remain exact while declaration gaps become canonical") {
    val source = "def first=1\n\n\n/* first\n exact indentation */\n\n\ndef second=2"
    val expected = "def first = 1\n\n/* first\n exact indentation */\ndef second = 2\n"

    check(source, expected)
  }

  test("a tuple's attached block comment follows the grouping gap") {
    check("object Example{call();\n\n\n/* Next expression */\n(first,second)}",
      "object Example {\n  call()\n\n  /* Next expression */\n  (first, second)\n}\n")
  }

  test("a method and a following tuple comment are separated by one blank line") {
    List(0, 1, 3).foreach { blanks =>
      val gap = "\n" * (blanks + 1)
      check("object Example{def first=1;" + gap + "/* Next expression */\n(first,second)}",
        "object Example {\n  def first = 1\n\n  /* Next expression */\n  (first, second)\n}\n")
    }
  }

  test("a unary expression's attached line comment follows the grouping gap") {
    check("object Example{call();\n\n\n// Next expression\n-value}",
      "object Example {\n  call()\n\n  // Next expression\n  -value\n}\n")
  }

  test("a unit result after a unit-valued binding needs no separator") {
    val expected = """val d = env.context.commands.addHandler[Unit](
      |      for {
      |        _ <- Setup.unit
      |      } yield { (req, res) =>
      |        val _ = ()
      |        ()
      |      }
      |    )
      |""".stripMargin

    check(expected, expected)
    check(expected.replace("val _ = ()\n        ()", "val _ = { () }; { () }"), expected)
  }

  test("a standalone unit expression follows a completed call with the ordinary gap") {
    check("def run = { call(); () }", "def run = {\n  call()\n  ()\n}\n")
    check("def run = { call{ x => x }\n\n// next\n() }",
      "def run = {\n  call{ x =>\n    x\n  }\n\n  // next\n  ()\n}\n")
  }

  test("a blank line separates unit from a preceding postfix expression") {
    check("def run = { f postfix; () }", "def run = {\n  f postfix\n\n  ()\n}\n")
    check("def run = { val x = f postfix; () }", "def run = {\n  val x = f postfix\n\n  ()\n}\n")
  }

  test("standalone comments retain bounded grouping before the attached statement") {
    check("val first=1\n\n// explanation\n\n\nval second=2",
      "val first = 1\n\n// explanation\nval second = 2\n")
  }

  test("comments following an unbraced package use one newline on each side") {
    check("package example\n\n// explanation\n\nobject Example {}",
      "package example\n// explanation\nobject Example\n")
  }

  test("comment batches retain one leading blank line but no internal blank lines") {
    check("val first=1\n\n// first\n\n\n// second\n\nval second=2",
      "val first = 1\n\n// first\n// second\nval second = 2\n")
  }

  test("multiline comment contents survive canonical surrounding gaps") {
    check("val first=1\n\n/* first\nsecond */\n\n\nval second=2",
      "val first = 1\n\n/* first\nsecond */\nval second = 2\n")
  }

  List(0, 1, 3).foreach { blanks =>
    test(s"trailing comments normalize $blanks source blank lines to canonical gaps") {
      val source = "object Example {\nval first = 1 // trailing" + ("\n" * (blanks + 1)) +
        "val second = 2\n}"
      val result = Formatter.format(source).fold(error => fail(error.getMessage), identity)
      val firstEnd = result.indexOf("val first = 1") + "val first = 1".length
      val commentStart = result.indexOf("// trailing")
      val commentEnd = commentStart + "// trailing".length
      val secondStart = result.indexOf("val second = 2")
      val reparsed = {
        given Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
        result.parse[Source].get
      }

      assertEquals(result.substring(firstEnd, commentStart).count(_ == '\n'), 1)
      assertEquals(result.substring(commentEnd, secondStart).count(_ == '\n'), 1)
      assertEquals(structure(reparsed), structure(source.parse[Source].get))
      assertEquals(comments(result), comments(source))
      assertEquals(Formatter.format(result), Right(result))
    }
  }

  test("CRLF input normalizes declaration gaps while output uses LF") {
    check("object Example{\r\nval first=1\r\n\r\ndef second=2\r\n\r\n\r\nval third=3\r\n}",
      "object Example {\n  val first = 1\n\n  def second = 2\n\n  val third = 3\n}\n")
  }

  test("spaces and tabs do not prevent blank-line runs collapsing to one") {
    check("object Example{val first=1\n \t\n\t \nval second=2}",
      "object Example {\n  val first = 1\n\n  val second = 2\n}\n")
  }

  test("nested container gaps remain independent and do not accumulate") {
    val source =
      """object Example{
        |val first=1
        |
        |
        |def run()={
        |first()
        |
        |second()
        |}
        |
        |val last=3
        |}""".stripMargin
    val expected =
      """object Example {
        |  val first = 1
        |
        |  def run() = {
        |    first()
        |
        |    second()
        |  }
        |
        |  val last = 3
        |}
        |""".stripMargin

    check(source, expected)
  }
}
