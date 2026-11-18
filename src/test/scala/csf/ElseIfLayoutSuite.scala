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

  test("direct and comment-free blocked else-if branches share their compact layout") {
    val expected = "val result =\n  if (first) {\n    one\n  } else if (second) {\n    two\n  } else {\n    other\n  }\n"

    assertEquals(format("val result=if(first)one else if(second)two else other"), expected)
    assertEquals(format("val result=if(first)one else{if(second)two else other}"), expected)
  }

  test("long mixed direct and blocked else-if chains do not accumulate nesting") {
    assertEquals(format("val result=if(first)one else{if(second)two else if(third)three else{if(fourth)four else other}}"),
      "val result =\n  if (first) {\n    one\n  } else if (second) {\n    two\n  } else if (third) {\n    three\n  } else if (fourth) {\n    four\n  } else {\n    other\n  }\n")
  }

  test("a nested then-branch conditional cannot take ownership of its outer else") {
    assertEquals(format("val result=if(outer){if(inner)left}else{if(next)right else fallback}"),
      "val result =\n  if (outer) {\n    if (inner) {\n      left\n    }\n  } else if (next) {\n    right\n  } else {\n    fallback\n  }\n")
  }

  test("comments before a direct else-if stay between else and the nested conditional") {
    val blockComment = format("val result=if(first)one else /* before */ if(second)two else other")
    assert(blockComment.contains("else\n  /* before */\n  if (second)"), blockComment)
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
      "val result =\n  if (first) {\n    one\n  } else {\n    val next = prepare()\n    if (second) {\n      use(next)\n    } else {\n      fallback\n    }\n  }\n")
  }

  test("else-if chains retain explicit unit else branches and omit absent branches") {
    assertEquals(format("val result=if(first)one else if(second)two else()"),
      "val result =\n  if (first) {\n    one\n  } else if (second) {\n    two\n  } else {\n    ()\n  }\n")
    assertEquals(format("val result=if(first)one else if(second)two"),
      "val result =\n  if (first) {\n    one\n  } else if (second) {\n    two\n  }\n")
  }
}
