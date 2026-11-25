package csf

import munit.FunSuite
import scala.meta.*

class ArrowLayoutSuite extends FunSuite {
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

  test("short lambda bodies remain after the arrow inside argument parentheses") {
    check("val f=x=>f(x)", "val f = x => f(x)\n")
    check("val result=run(x=>f(x))", "val result = run(x => f(x))\n")
  }

  test("short context lambda bodies remain after the context arrow") {
    check("val f=(ctx:Context)?=>use(ctx)", "val f = (ctx: Context) ?=> use(ctx)\n")
    check("val result=run((ctx:Context)?=>use(ctx))", "val result = run((ctx: Context) ?=> use(ctx))\n")
  }

  test("short polymorphic lambda bodies remain after their arrows") {
    check("val f=[A]=>(x:A)=>x", "val f = [A] => (x: A) => x\n")
    check("val result=run([A]=>(x:A)=>x)", "val result = run([A] => (x: A) => x)\n")
  }

  test("lambda and case bodies fitting exactly 100 columns remain inline") {
    val lambdaText = "x" * (100 - "val f = x => \"\"".length)
    val lambdaExpected = s"val f = x => \"$lambdaText\"\n"
    val caseText = "x" * (100 - "    case _ => \"\"".length)

    assertEquals(lambdaExpected.linesIterator.next().length, 100)
    assertEquals(s"    case _ => \"$caseText\"".length, 100)
    check(s"val f=x=>\"$lambdaText\"", lambdaExpected)
    check(s"val result=value match{case _=>\"$caseText\"}",
      s"val result =\n  value match {\n    case _ => \"$caseText\"\n  }\n")
  }

  test("lambda and case bodies exceeding 100 columns move below the arrow") {
    val lambdaText = "x" * (101 - "  x => \"\"".length)
    val caseText = "x" * (101 - "    case _ => \"\"".length)

    assertEquals(s"  x => \"$lambdaText\"".length, 101)
    assertEquals(s"    case _ => \"$caseText\"".length, 101)
    check(s"val f=x=>\"$lambdaText\"", s"val f =\n  x =>\n    \"$lambdaText\"\n")
    check(s"val result=value match{case _=>\"$caseText\"}",
      s"val result =\n  value match {\n    case _ =>\n      \"$caseText\"\n  }\n")
  }

  test("multiline chains move below normal and context lambda arrows") {
    val receiver = "receiver" + "x" * 86
    check(s"val result=run(x=>$receiver.foo.bar)",
      s"val result = run(\n  x =>\n    $receiver\n      .foo\n      .bar\n)\n")
    check(s"val f=(ctx:Context)?=>$receiver.first.second",
      s"val f =\n  (ctx: Context) ?=>\n    $receiver\n      .first\n      .second\n")
  }

  test("call bodies start below the arrow whenever the whole call cannot fit") {
    val receiver = "receiver" + "x" * 86
    check(s"val f=x=>combine($receiver.foo.bar,other)",
      s"val f =\n  x =>\n    combine(\n      $receiver\n        .foo\n        .bar,\n      other\n    )\n")
    val first = "first" + "Arg" * 10
    val second = "second" + "Arg" * 10
    check(s"val descriptive=value=>combine($first,$second)",
      s"val descriptive =\n  value => combine($first, $second)\n")
  }

  test("multistatement lambda bodies keep braces after the arrow") {
    check("val result=run(x=>{prepare(x);value(x)})",
      "val result = run(\n  x =>\n    {\n      prepare(x)\n      value(x)\n    }\n)\n")
  }

  test("match cases and partial functions choose body layout independently per case") {
    val receiver = "receiver" + "x" * 86
    check(s"val result=value match{case One=>f(one);case Two=>$receiver.compile.drain}",
      s"val result =\n  value match {\n    case One => f(one)\n    case Two =>\n      $receiver\n        .compile\n        .drain\n  }\n")
    check(s"val f={case Some(x)=>f(x);case _=>$receiver.compile.drain}",
      s"val f = {\n  case Some(x) => f(x)\n  case _ =>\n    $receiver\n      .compile\n      .drain\n}\n")
  }

  test("body comments always occupy standalone lines") {
    check("val result=run(x=> /* note */ f(x))", "val result = run(\n  x =>\n    /* note */\n    f(x)\n)\n")
    check("val result=run(x=> // note\nf(x))", "val result = run(\n  x =>\n    // note\n    f(x)\n)\n")
  }

  test("empty lambdas preserve empty parameters bodies and comment-only closing braces") {
    check("val result=run(()=>())", "val result = run(() => ())\n")
    check("val result=run(x=>{})", "val result = run(x => {})\n")
    check("val result=run(x=>{ // note\n})", "val result = run(\n  x =>\n    {\n      // note\n    }\n)\n")
  }
}
