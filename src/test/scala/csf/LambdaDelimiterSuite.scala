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
    val original = structure(source.parse[Source].get)
    val reparsed = {
      given Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
      result.parse[Source].get
    }
    assertEquals(structure(result.parse[Source].get), original, result)
    assertEquals(structure(reparsed), original, result)
    assertEquals(
      result.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
      source.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
    )
    assertEquals(Formatter.format(result), Right(result))
    assert(result.linesIterator.forall(line => line == line.stripTrailing()), result)
    result
  }

  test("sole lambda arguments retain their call delimiters") {
    List(
      ("value=>use(value)", "value =>", "use(value)"),
      ("(value:Int)=>use(value)", "(value: Int) =>", "use(value)"),
      ("(left:Int,right:Int)=>combine(left,right)", "(left: Int, right: Int) =>", "combine(left, right)"),
      ("(context:Context)?=>use(context)", "(context: Context) ?=>", "use(context)"),
      ("[A]=>(value:A)=>value", "[A] =>", "(value: A) => value"),
      ("_.value", "", "_.value"),
    ).foreach { case (lambda, header, body) =>
      val inline = if (header.isEmpty) body else s"$header $body"
      val opening = if (header.isEmpty) "{" else s"{ $header"
      assertEquals(format(s"val result=values.map($lambda)"), s"val result = values.map($inline)\n")
      assertEquals(format(s"val result=values.map{$lambda}"), s"val result = values.map$opening\n    $body\n  }\n")
    }
    val partial = format("val result=values.collect({case Some(value)=>use(value)})")
    assert(partial.contains("values.collect("), partial)
    assert(partial.contains("{"), partial)
    assertEquals(format("val result=values.collect{case Some(value)=>use(value)}"),
      "val result = values.collect{ case Some(value) =>\n    use(value)\n  }\n")
  }

  test("single-expression lambda comments remain inside the source argument delimiters") {
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
      List(parenthesized -> "(", braced -> "{").foreach { case (source, opening) =>
        val result = format(source)
        val application = result.parse[Source].get.stats.head.asInstanceOf[Defn.Val].rhs.asInstanceOf[Term.Apply]
        val argumentText = application.argClause.pos.text
        assert(argumentText.startsWith(opening), result)
        assert(argumentText.contains("//") || argumentText.contains("/*"), result)
      }
    }

    val result = format("val result=values.map(/* before block */{/* inside block */value=>use(value)})")
    val application = result.parse[Source].get.stats.head.asInstanceOf[Defn.Val].rhs.asInstanceOf[Term.Apply]
    val block = application.argClause.values.head.pos.text
    assert(application.argClause.pos.text.contains("/* before block */"), result)
    assert(!block.contains("/* before block */"), result)
    assert(block.contains("/* inside block */"), result)
  }

  test("long single-expression placeholder bodies retain their call delimiters while wrapping") {
    val first = "first" + "Argument" * 5
    val second = "second" + "Argument" * 5
    val parenthesized = format(s"val result=values.map(_.combine($first,$second))")
    val braced = format(s"val result=values.map{_.combine($first,$second)}")

    assert(parenthesized.contains("values.map("), parenthesized)
    assert(braced.contains("values.map{"), braced)
    assert(parenthesized.linesIterator.forall(_.length <= 100), parenthesized)
    assert(braced.linesIterator.forall(_.length <= 100), braced)
  }

  test("lambda body braces stay inside their argument delimiters") {
    List(
      "val result=values.map(value=>{prepare(value);use(value)})" ->
        "val result=values.map{value=>prepare(value);use(value)}",
      "val result=values.map(value=>{})" -> "val result=values.map{value=>}",
      "val result=values.map(value=>{val next=value})" -> "val result=values.map{value=>val next=value}",
    ).foreach { case (parenthesized, braced) =>
      val result = format(parenthesized)
      assert(result.contains("values.map("), result)
      assert("value =>\\s*\\{".r.findFirstIn(result).nonEmpty, result)
      val attached = format(braced)
      assert(attached.contains("values.map{"), attached)
    }

    val bracedBody = format("val result=values.map{value=>{prepare(value);use(value)}}")
    assert(bracedBody.contains("values.map{"), bracedBody)
    assert("value =>\\s*\\{".r.findFirstIn(bracedBody).nonEmpty, bracedBody)

    val nested = format("val result=values.map{value=>(inner=>{use(inner)});use(value)}")
    assert("inner =>\\s*\\{".r.findFirstIn(nested).nonEmpty, nested)
  }

  test("infix curried and wrapped lambda arguments retain their source delimiters") {
    assertEquals(format("val result=values map (_.id)"), "val result = values map (_.id)\n")
    assertEquals(format("val result=values map {_.value}"), "val result =\n  values map{\n    _.value\n  }\n")
    assertEquals(format("val result=select(context)(_.value)"), "val result = select(context)(_.value)\n")
    assertEquals(format("val result=select(context){_.value}"), "val result = select(context){\n    _.value\n  }\n")
    val wrapped = format("val result=values.map({{{_.value}}})")
    assert(wrapped.contains("values.map("), wrapped)
    assertEquals(wrapped.count(_ == '{'), 3, wrapped)
    val commented = format("val result=values map ({(value:Int)=>use(value) // footer\n})")
    assert("values map\\s*\\(".r.findFirstIn(commented).nonEmpty, commented)
    assertEquals(commented.count(_ == '{'), 1, commented)
    val braced = format("val result=values map{(value:Int)=>use(value) // footer\n}")
    assert(braced.contains("values map{"), braced)
  }

  test("multiple arguments and constructors retain unbraced lambdas in parentheses") {
    val multiple = format("val result=combine((value:Int)=>use(value),other)")
    val constructor = format("val result=new Service(_.value)")

    assert(multiple.contains("combine((value: Int) => use(value), other)"), multiple)
    assert(constructor.contains("new Service(_.value)"), constructor)
  }

  test("literal lambda forms keep explicit braces inside multiargument parentheses") {
    List(
      "value=>use(value)",
      "(value:Int)=>use(value)",
      "(left:Int,right:Int)=>combine(left,right)",
      "(context:Context)?=>use(context)",
      "[A]=>(value:A)=>value",
    ).foreach { lambda =>
      val naked = format(s"val result=call($lambda,42)")
      val braced = format(s"val result=call({$lambda},42)")

      assert(!naked.contains("{"), naked)
      assert(braced.contains("call("), braced)
      assertEquals(braced.count(_ == '{'), 1, braced)
    }
    val naked = format("val result=call(_.id,42)")
    val braced = format("val result=call({_.id},42)")
    assertEquals(naked, "val result = call(_.id, 42)\n")
    assertEquals(braced.count(_ == '{'), 1, braced)
  }

  test("constructor and using arguments keep their explicit lambda braces") {
    List(
      "val result=new Service(_.id,42)" -> "val result=new Service({_.id},42)",
      "val result=new Service((value:Int)=>use(value))" ->
        "val result=new Service({(value:Int)=>use(value)})",
      "val result=call(using (left:Context)=>use(left),(right:Context)=>use(right))" ->
        "val result=call(using {(left:Context)=>use(left)},{(right:Context)=>use(right)})",
    ).foreach { case (naked, braced) =>
      val plain = format(naked)
      val wrapped = format(braced)
      assert(!plain.contains("{"), plain)
      assertEquals(wrapped.count(_ == '{'), braced.count(_ == '{'), wrapped)
    }
    assertEquals(format("val result=new Service(_.id)"), "val result = new Service(_.id)\n")
    val constructor = format("val result=new Service({_.id})")
    assert(constructor.contains("new Service("), constructor)
    assertEquals(constructor.count(_ == '{'), 1, constructor)
    assertEquals(format("val result=call(using (value:Context)=>use(value))"),
      "val result = call(using (value: Context) => use(value))\n")
    val contextual = format("val result=call(using {(value:Context)=>use(value)})")
    assert(contextual.contains("using"), contextual)
    assertEquals(contextual.count(_ == '{'), 1, contextual)
  }

  test("named argument lambdas retain their enclosing argument clause and explicit braces") {
    List(
      "val result=call(callback=(value=>use(value)),other=42)" ->
        "val result=call(callback={value=>use(value)},other=42)",
      "val result=call(callback=(_.id),other=42)" -> "val result=call(callback={_.id},other=42)",
      "val result=call(callback=(value=>{prepare(value);use(value)}),other=42)" ->
        "val result=call(callback={value=>prepare(value);use(value)},other=42)",
      "val result=call(other=42,callback=({/* callback */value=>use(value)} // footer\n))" ->
        "val result=call(other=42,callback={/* callback */value=>use(value) // footer\n})",
      "val result=call(callback=({value=>use(value)} // callback footer\n),other=/* other argument */42)" ->
        "val result=call(callback={value=>use(value) // callback footer\n},other=/* other argument */42)",
    ).foreach { case (naked, braced) =>
      List(naked, braced).foreach { source =>
        val result = format(source)
        assert(result.contains("call("), result)
        assertEquals(result.count(_ == '{'), source.count(_ == '{'), result)
      }
    }
    assertEquals(format("val result=call(callback=(value=>use(value)),other=42)"),
      "val result = call(callback = value => use(value), other = 42)\n")
  }

  test("typed argument wrappers retain ascriptions and explicit lambda braces") {
    val naked = "val result=call((((value:Int)=>use(value)):Int=>Int),other)"
    val braced = "val result=call(({(value:Int)=>use(value)}:Int=>Int),other)"
    List(naked, braced).foreach { source =>
      val result = format(source)
      assert(result.contains(": Int => Int"), result)
      assertEquals(result.count(_ == '{'), source.count(_ == '{'), result)
    }
  }

  test("explicit lambda body braces remain inside multiargument calls") {
    assertEquals(format("val result=call(value=>use(value),42)"),
      "val result = call(value => use(value), 42)\n")
    val multiline = format("val result=call(value=>{prepare(value);use(value)},42)")
    val braced = format("val result=call({value=>prepare(value);use(value)},42)")

    assert(multiline.contains("value =>\n      {"), multiline)
    assert(braced.contains("{ value =>"), braced)
  }

  test("empty and declaration-only lambda bodies keep their explicit brace placement") {
    List(
      "val result=call(value=>{},42)" -> "val result=call({value=>},42)",
      "val result=call(value=>{val next=value},42)" ->
        "val result=call({value=>val next=value},42)",
    ).foreach { case (naked, braced) =>
      val body = format(naked)
      val argument = format(braced)
      assert("value =>\\s*\\{".r.findFirstIn(body).nonEmpty, body)
      assert(argument.contains("{ value =>"), argument)
    }
  }

  test("a lambda cannot consume comments belonging to its following argument") {
    val source = "val result=call(value=>use(value),// second argument\n other=>use(other))"
    val result = format(source)
    val application = result.parse[Source].get.stats.head.asInstanceOf[Defn.Val].rhs.asInstanceOf[Term.Apply]
    val arguments = application.argClause.values
    val secondComment = result.tokenize.get.collectFirst {
      case comment: Token.Comment if comment.text == "// second argument" => comment
    }.get

    assertEquals(arguments.size, 2)
    assert(secondComment.start >= arguments.head.pos.end, result)
    assert(!arguments.head.pos.text.contains("second argument"), result)
    val braced = format("val result=call({value=>use(value)},// second argument\n {other=>use(other)})")
    val bracedApplication = braced.parse[Source].get.stats.head.asInstanceOf[Defn.Val].rhs.asInstanceOf[Term.Apply]
    assert(!bracedApplication.argClause.values.head.pos.text.contains("second argument"), braced)
    val named = format("val result=call(callback=({value=>use(value)} // callback footer\n),other=/* other argument */42)")
    val namedApplication = named.parse[Source].get.stats.head.asInstanceOf[Defn.Val].rhs.asInstanceOf[Term.Apply]

    assert(namedApplication.argClause.values.head.pos.text.contains("// callback footer"), named)
    assert(!namedApplication.argClause.values.head.pos.text.contains("other argument"), named)
    assert(namedApplication.argClause.values(1).pos.text.contains("/* other argument */"), named)
  }

  test("a polymorphic lambda retains its nested function in a multiargument clause") {
    assertEquals(format("val result=call([A]=>(value:A)=>value,42)"),
      "val result = call([A] => (value: A) => value, 42)\n")
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

  test("conditional lambda arguments keep their source delimiters while wrapping") {
    List(
      "val result=values.map(value=>if(ready)use(value)else other)" -> "(",
      "val result=values.map{value=>if(ready){use(value)}else{other}}" -> "{",
    ).foreach { case (source, opening) =>
      val result = format(source)
      val application = result.parse[Source].get.stats.head.asInstanceOf[Defn.Val].rhs.asInstanceOf[Term.Apply]
      assert(application.argClause.pos.text.startsWith(opening), result)
      assert(application.argClause.pos.text.contains("\n"), result)
    }
  }
}
