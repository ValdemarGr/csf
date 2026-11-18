package csf

import munit.FunSuite
import scala.meta.*

class LambdaDelimiterSuite extends FunSuite {
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
    assertEquals(structure(result.parse[Source].get), structure(source.parse[Source].get), result)
    assertEquals(structure(reparsed), structure(source.parse[Source].get), result)
    assertEquals(
      result.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
      source.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
    )
    assertEquals(Formatter.format(result), Right(result))
    assert(result.linesIterator.forall(line => line == line.stripTrailing()), result)
    result
  }

  test("sole lambda argument forms converge to attached braces from either source delimiter") {
    List(
      "value=>use(value)",
      "(value:Int)=>use(value)",
      "(left:Int,right:Int)=>combine(left,right)",
      "(context:Context)?=>use(context)",
      "[A]=>(value:A)=>value",
      "_.value",
    ).foreach { lambda =>
      val result = format(s"val result=values.map($lambda)")
      assertEquals(result, format(s"val result=values.map{$lambda}"))
      assert(result.contains("values.map{"), result)
    }
    assertEquals(format("val result=values.map(value=>use(value))"),
      "val result = values.map{ value => use(value) }\n")
    assertEquals(format("val result=values.map(_.id)"), "val result = values.map{ _.id }\n")
    assertEquals(format("val result=values.map{_.id}"), "val result = values.map{ _.id }\n")
    val partial = format("val result=values.collect({case Some(value)=>use(value)})")
    assertEquals(partial, format("val result=values.collect{case Some(value)=>use(value)}"))
    assert(partial.contains("values.collect{"), partial)
  }

  test("single-expression lambda comments remain inside attached argument braces") {
    List(
      "val result=values.map(/* before */_.value)" -> "val result=values.map{/* before */_.value}",
      "val result=values.map(// before\n_.value)" -> "val result=values.map{// before\n_.value}",
      "val result=values.map(// before\n(value:Int)=>use(value))" ->
        "val result=values.map{// before\n(value:Int)=>use(value)}",
      "val result=values.map(_.value // footer\n)" -> "val result=values.map{_.value // footer\n}",
      "val result=values.map(/* before */(value:Int)=>use(value))" ->
        "val result=values.map{/* before */(value:Int)=>use(value)}",
      "val result=values.map((value:Int)=>use(value) // footer\n)" ->
        "val result=values.map{(value:Int)=>use(value) // footer\n}",
    ).foreach { case (parenthesized, braced) =>
      val result = format(parenthesized)
      assertEquals(result, format(braced))
      val application = result.parse[Source].get.stats.head.asInstanceOf[Defn.Val].rhs.asInstanceOf[Term.Apply]
      val argumentText = application.argClause.pos.text
      assert(argumentText.startsWith("{"), result)
      assert(argumentText.contains("//") || argumentText.contains("/*"), result)
    }
  }

  test("long single-expression placeholder bodies keep attached braces while wrapping") {
    val first = "first" + "Argument" * 5
    val second = "second" + "Argument" * 5
    val result = format(s"val result=values.map(_.combine($first,$second))")

    assertEquals(result, format(s"val result=values.map{_.combine($first,$second)}"))
    assert(result.contains("values.map{"), result)
    assert(result.linesIterator.forall(_.length <= 100), result)
  }

  test("multistatement empty and declaration-only lambda bodies use argument braces") {
    List(
      "val result=values.map(value=>{prepare(value);use(value)})" ->
        "val result=values.map{value=>prepare(value);use(value)}",
      "val result=values.map(value=>{})" -> "val result=values.map{value=>}",
      "val result=values.map(value=>{val next=value})" -> "val result=values.map{value=>val next=value}",
    ).foreach { case (parenthesized, braced) =>
      val result = format(parenthesized)
      assertEquals(result, format(braced))
      assert(result.contains("values.map{"), result)
    }
  }

  test("infix curried and wrapped lambda arguments use the same canonical delimiters") {
    List(
      "val result=values map (_.value)" -> "val result=values map {_.value}",
      "val result=select(context)(_.value)" -> "val result=select(context){_.value}",
      "val result=values.map({{{_.value}}})" -> "val result=values.map{_.value}",
      "val result=values map ({(value:Int)=>use(value) // footer\n})" ->
        "val result=values map{(value:Int)=>use(value) // footer\n}",
    ).foreach { case (parenthesized, braced) =>
      assertEquals(format(parenthesized), format(braced))
    }
    assertEquals(format("val result=values map (_.id)"), "val result = values map{ _.id }\n")
  }

  test("multiple arguments using clauses and constructors retain parentheses around braced lambdas") {
    val multiple = format("val result=combine((value:Int)=>use(value),other)")
    val usingClause = format("val result=call(using (value:Context)=>use(value))")
    val constructor = format("val result=new Service(_.value)")

    assert(multiple.contains("combine({ (value: Int) => use(value) }, other)"), multiple)
    assert(usingClause.contains("call(using { (value: Context) => use(value) })"), usingClause)
    assert(constructor.contains("new Service({ _.value })"), constructor)
  }

  test("standalone named and placeholder lambdas retain their expression layout") {
    assertEquals(format("val f=value=>use(value)"), "val f = value => use(value)\n")
    assertEquals(format("val f=_.value"), "val f = _.value\n")
  }

  test("standalone multistatement lambdas converge to one outer brace pair") {
    List("val f", "def f").foreach { declaration =>
      val expected = s"$declaration = { x =>\n  prepare(x)\n  value(x)\n}\n"
      List(
        "x=>{prepare(x);value(x)}",
        "{x=>prepare(x);value(x)}",
        "{{{x=>{{prepare(x);value(x)}}}}}",
      ).foreach { lambda =>
        assertEquals(format(s"$declaration=$lambda"), expected)
      }
    }
  }

  test("typed and context block lambdas share standalone brace ownership") {
    assertEquals(format("val f=(value:Int)=>{prepare(value);use(value)}"),
      "val f = { (value: Int) =>\n  prepare(value)\n  use(value)\n}\n")
    assertEquals(format("val f=(context:Context)?=>{prepare(context);use(context)}"),
      "val f = { (context: Context) ?=>\n  prepare(context)\n  use(context)\n}\n")
  }

  test("polymorphic lambda declarations retain their grammar-required body block") {
    val result = format("val f=[A]=>{val cached=prepare();(value:A)=>cached}")
    val lambda = result.parse[Source].get.collect { case value: Term.PolyFunction => value }.head

    assert(lambda.body.isInstanceOf[Term.Block], result)
    assertEquals(result, format("val f={[A]=>{val cached=prepare();(value:A)=>cached}}"))
  }

  test("empty and declaration-only standalone lambda blocks retain one outer brace pair") {
    assertEquals(format("val f=value=>{}"), "val f = { value => }\n")
    assertEquals(format("val f=value=>{val next=value}"),
      "val f = { value => val next = value }\n")
    assertEquals(format("val f=value=>{// body\n}"),
      "val f = { value =>\n  // body\n}\n")
  }

  test("a polymorphic lambda retains its nested value lambda and block body") {
    val result = format("val f=[A]=>(value:A)=>{prepare(value);value}")

    assert(result.contains("[A] =>"), result)
    assert(result.contains("{ (value: A) =>\n"), result)
    assertEquals(result, format("val f=[A]=>{(value:A)=>prepare(value);value}"))
  }

  test("standalone lambda bodies retain bounded statement grouping without extra indentation") {
    val expected = "val f = { x =>\n  prepare(x)\n\n  value(x)\n}\n"

    assertEquals(format("val f={x=>prepare(x)\n\n\nvalue(x)}"), expected)
    assertEquals(format("val f=x=>{prepare(x)\n\nvalue(x)}"), expected)
  }

  test("nested standalone lambda and local block scopes remain distinct") {
    val source = "val f=x=>{val first={val local=prepare(x);local};" +
      "val g=y=>{val second=prepare(y);combine(first,second)};finish(g)}"
    val expected = "val f = { x =>\n  val first = {\n    val local = prepare(x)\n    local\n  }\n" +
      "  val g = { y =>\n    val second = prepare(y)\n    combine(first, second)\n  }\n  finish(g)\n}\n"

    assertEquals(format(source), expected)
  }

  test("standalone lambda comments stay inside their brace pair without capturing sibling comments") {
    val source = "object Example{val f={/* header */x=>{prepare(x);value(x)// body footer\n}" +
      "/* wrapper footer */};\n// sibling\nval after=1}"
    val result = format(source)

    assert(result.contains("val f = {"), result)
    assert(result.indexOf("/* header */") < result.indexOf("x =>"), result)
    assert(result.contains("/* wrapper footer */\n  }\n  // sibling\n  val after = 1"), result)
  }

  test("a single-expression conditional lambda uses attached braces around its multiline body") {
    val result = format("val result=values.map(value=>if(ready)use(value)else other)")

    assertEquals(result, format("val result=values.map{value=>if(ready){use(value)}else{other}}"))
    assert(result.contains("values.map{"), result)
    val application = result.parse[Source].get.stats.head.asInstanceOf[Defn.Val].rhs.asInstanceOf[Term.Apply]
    assert(application.argClause.pos.text.startsWith("{"), result)
    assert(application.argClause.pos.text.contains("\n"), result)
  }
}
