package csf

import munit.FunSuite
import scala.meta.*

class ElseIfLayoutSuite extends FunSuite {
  private given Dialect = dialects.Scala3

  private def structure(value: Any): Any = value match {
    case Term.Block(List(term: Term)) => structure(term)
    case tree: Tree => (tree.productPrefix, tree.productIterator.map(structure).toList)
    case values: List[?] => values.map(structure)
    case value: Option[?] => value.map(structure)
    case other => other
  }

  private def format(source: String, options: FormatOptions = FormatOptions()): String = {
    val result = Formatter.format(source, options).fold(error => fail(error.getMessage), identity)
    val reparsed = {
      given Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
      result.parse[Source].get
    }
    assertEquals(structure(reparsed), structure(source.parse[Source].get), result)
    assertEquals(
      result.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
      source.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
    )
    assertEquals(Formatter.format(result, options), Right(result))
    result
  }

  test("new control syntax handles nested conditionals and comments in conditions") {
    val result = format(
      """def run =
        |  if (/* first check */ if ready then true else false) /* then */
        |    while ready do
        |      use()
        |      finish()
        |  else if other // before then
        |  then while (// keep checking
        |    ready) /* do */ wait()
        |""".stripMargin)

    assertEquals(" then \\{".r.findAllIn(result).length, 3, result)
    assertEquals(" do \\{".r.findAllIn(result).length, 2, result)
    assert(result.contains("if ready then {"), result)
    assert(result.contains("while ready do {"), result)
  }

  test("each mode canonicalizes both input styles to the same if and while output") {
    val old = FormatOptions(controlSyntax = ControlSyntax.Old)
    val legacy = "def run = if (ready && allowed) while (running) step() else stop()"
    val modern = "def run = if ready && allowed then while running do step() else stop()"
    val newOutput =
      "def run =\n  if ready && allowed then {\n    while running do {\n      step()\n    }\n  } else {\n    stop()\n  }\n"
    val oldOutput =
      "def run =\n  if (ready && allowed) {\n    while (running) {\n      step()\n    }\n  } else {\n    stop()\n  }\n"

    List(legacy, modern).foreach { source =>
      assertEquals(format(source), newOutput)
      assertEquals(format(source, old), oldOutput)
    }
  }

  test("new control syntax retains required grouping and braced condition blocks") {
    val source = """def run =
      |  if { val x = ready; x } then
      |    while (ready || allowed) && running do step()
      |  else stop()
      |""".stripMargin
    val result = format(source, FormatOptions(columns = 32))

    assert(result.contains("if {\n"), result)
    assert(result.contains("} then {"), result)
    assert(result.contains("while (ready || allowed) &&"), result)
    assert(result.contains("running do {"), result)
  }

  test("direct and comment-free blocked else-if branches share their compact layout") {
    val expected = "val result =\n  if first then {\n    one\n  } else if second then {\n    two\n  } else {\n    other\n  }\n"

    assertEquals(format("val result=if(first)one else if(second)two else other"), expected)
    assertEquals(format("val result=if(first)one else{if(second)two else other}"), expected)
  }

  test("long mixed direct and blocked else-if chains do not accumulate nesting") {
    assertEquals(format("val result=if(first)one else{if(second)two else if(third)three else{if(fourth)four else other}}"),
      "val result =\n  if first then {\n    one\n  } else if second then {\n    two\n  } else if third then {\n    three\n  } else if fourth then {\n    four\n  } else {\n    other\n  }\n")
  }

  test("a nested then-branch conditional cannot take ownership of its outer else") {
    assertEquals(format("val result=if(outer){if(inner)left}else{if(next)right else fallback}"),
      "val result =\n  if outer then {\n    if inner then {\n      left\n    }\n  } else if next then {\n    right\n  } else {\n    fallback\n  }\n")
  }

  test("comments before a direct else-if stay between else and the nested conditional") {
    val blockComment = format("val result=if(first)one else /* before */ if(second)two else other")
    assert(blockComment.contains("else\n  /* before */\n  if second then"), blockComment)
    format("val result=if(first)one else // before\nif(second)two else other")
  }

  test("leading footer and nested comments survive removing a redundant else block") {
    List(
      "val result=if(first)one else{/* inside */if(second)two else other}",
      "val result=if(first)one else{if(second)two else other // footer\n}",
      "val result=if(first)one else{if(second){/* nested */two}else other}",
    ).foreach { source =>
      val result = format(source)
      val conditional = result.parse[Source].get.collect { case conditional: Term.If => conditional }.head
      assert(conditional.elsep.isInstanceOf[Term.If], result)
    }
  }

  test("multistatement else blocks retain the scope of their local declaration") {
    assertEquals(format("val result=if(first)one else{val next=prepare();if(second)use(next)else fallback}"),
      "val result =\n  if first then {\n    one\n  } else {\n    val next = prepare()\n    if second then {\n      use(next)\n    } else {\n      fallback\n    }\n  }\n")
  }

  test("else-if chains retain explicit unit else branches and omit absent branches") {
    assertEquals(format("val result=if(first)one else if(second)two else()"),
      "val result =\n  if first then {\n    one\n  } else if second then {\n    two\n  } else {\n    ()\n  }\n")
    assertEquals(format("val result=if(first)one else if(second)two"),
      "val result =\n  if first then {\n    one\n  } else if second then {\n    two\n  }\n")
  }
}
