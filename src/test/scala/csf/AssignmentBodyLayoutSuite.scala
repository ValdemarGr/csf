package csf

import munit.FunSuite
import scala.meta.*

class AssignmentBodyLayoutSuite extends FunSuite {
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

  test("short mutable regular named and tuple assignments keep their RHS inline") {
    assertEquals(format("var value=1"), "var value = 1\n")
    assertEquals(format("object X{var value=0;value=next}"),
      "object X {\n  var value = 0\n  value = next\n}\n")
    assertEquals(format("val result=call(value=next,other=2)"),
      "val result = call(value = next, other = 2)\n")
    assertEquals(format("val result=(name=value,count=1)"),
      "val result = (name = value, count = 1)\n")
  }

  test("mutable and regular assignment RHS expressions exceeding 40 columns move below equals") {
    val mutableText = "x" * (41 - "var value = \"\"".length)
    val assignedText = "x" * (41 - "  value = \"\"".length)

    assertEquals(s"var value = \"$mutableText\"".length, 41)
    assertEquals(format(s"var value=\"$mutableText\"", 40), s"var value =\n  \"$mutableText\"\n")
    assertEquals(s"  value = \"$assignedText\"".length, 41)
    assertEquals(format(s"object X{value=\"$assignedText\"}", 40),
      s"object X {\n  value =\n    \"$assignedText\"\n}\n")
  }

  test("parameter defaults share short overflowing and retained-block RHS layout") {
    val text = "x" * (41 - "  value: String = \"\"".length)

    assertEquals(format("def f(value:Int=1)=value"), "def f(value: Int = 1) = value\n")
    assertEquals(s"  value: String = \"$text\"".length, 41)
    assertEquals(format(s"def f(value:String=\"$text\")=value", 40),
      s"def f(\n  value: String =\n    \"$text\"\n) = value\n")
    assertEquals(format("def f(value:Int={prepare();1})=value"),
      "def f(\n  value: Int = {\n    prepare()\n    1\n  }\n) = value\n")
  }

  test("named call and tuple arguments retain block braces before their separating comma") {
    assertEquals(format("val result=call(value={prepare();next},other=2)"),
      "val result =\n  call(\n    value = {\n      prepare()\n      next\n    },\n    other = 2\n  )\n")
    assertEquals(format("val result=(name={prepare();value},count=1)"),
      "val result =\n  (\n    name = {\n      prepare()\n      value\n    },\n    count = 1\n  )\n")
  }

  test("mutable and regular partial-function assignments keep their brace beside equals") {
    assertEquals(format("var handler:PartialFunction[Int,Int]={case value=>value}"),
      "var handler: PartialFunction[Int, Int] = {\n  case value => value\n}\n")
    assertEquals(format("object X{handler={case error:Error=>recover(error)}}"),
      "object X {\n  handler = {\n    case error: Error => recover(error)\n  }\n}\n")
    assertEquals(format("var state={}"), "var state = {\n}\n")
  }

  test("named argument and default footer comments cannot swallow their following comma") {
    assertEquals(format("val result=call(value=/* before */next,other=after)"),
      "val result =\n  call(\n    value =\n      /* before */\n      next,\n    other = after\n  )\n")
    val namedExpected = "val result =\n  call(\n    value = {\n      next\n      // footer\n    },\n    other = after\n  )\n"
    List(
      "val result=call(value={next // footer\n},other=after)",
      "val result=call(value=({next // footer\n}),other=after)",
      "val result=call(value=(({next // footer\n})),other=after)",
    ).foreach { source =>
      assertEquals(format(source), namedExpected)
    }
    val defaultExpected = "def f(\n  first: Int = {\n    value\n    // footer\n  },\n  second: Int = 2\n) = first\n"
    List(
      "def f(first:Int={value // footer\n},second:Int=2)=first",
      "def f(first:Int=({value // footer\n}),second:Int=2)=first",
      "def f(first:Int=(({value // footer\n})),second:Int=2)=first",
    ).foreach { source =>
      assertEquals(format(source), defaultExpected)
    }
    val withoutBlock = "val result =\n  call(\n    value =\n      next\n      // footer\n  )\n"
    List(
      "val result=call(value={next // footer\n})",
      "val result=call(value={next // footer\n},\n)",
    ).foreach { source =>
      assertEquals(format(source), withoutBlock)
    }
  }

  test("for-value RHS layout preserves subsequent bindings and footer comments") {
    assertEquals(format("val result=for{value<-values;next=if(ready)use(value)else fallback;last=after(next)}yield last"),
      "val result =\n  for {\n    value <- values\n    next =\n      if ready then {\n        use(value)\n      } else {\n        fallback\n      }\n    last = after(next)\n  } yield last\n")
    format("val result=for{a<-as;b={next(a)// footer\n};c=after(b)}yield c")
  }

  test("singleton RHS wrappers normalize across every assignment context") {
    List(
      "var value={{{next}}}" -> "var value=next",
      "object X{value={{{next}}}}" -> "object X{value=next}",
      "val result=call(value={{{next}}})" -> "val result=call(value=next)",
      "val result=(name={{{value}}},count=1)" -> "val result=(name=value,count=1)",
      "def f(value:Int={{{1}}})=value" -> "def f(value:Int=1)=value",
      "val result=for{a<-as;b={{{next(a)}}}}yield b" -> "val result=for{a<-as;b=next(a)}yield b",
    ).foreach { case (blocked, direct) =>
      assertEquals(format(blocked), format(direct))
    }
  }
}
