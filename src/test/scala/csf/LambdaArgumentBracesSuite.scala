package csf

import munit.FunSuite
import scala.meta.*

class LambdaArgumentBracesSuite extends FunSuite {
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
    val original = source.parse[Source].get
    assertEquals(structure(result.parse[Source].get), structure(original), result)
    val reparsed = {
      given Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
      result.parse[Source].get
    }
    assertEquals(structure(reparsed), structure(original), result)
    assertEquals(
      result.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
      source.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
    )
    assertEquals(Formatter.format(result), Right(result))
    assert(result.linesIterator.forall(line => line == line.stripTrailing()), result)
    result
  }

  test("all literal lambda forms retain braces inside required multiargument parentheses") {
    List(
      "value=>use(value)",
      "(value:Int)=>use(value)",
      "(left:Int,right:Int)=>combine(left,right)",
      "(context:Context)?=>use(context)",
      "[A]=>(value:A)=>value",
      "_.id",
    ).foreach { lambda =>
      val result = format(s"val result=call($lambda,42)")

      assertEquals(result, format(s"val result=call({$lambda},42)"))
      assert(result.contains("call({"), result)
      assert(result.contains("}, 42)"), result)
    }
    assertEquals(format("val result=call(_.id,42)"), "val result = call({ _.id }, 42)\n")
  }

  test("constructor and using arguments keep both lambda braces and required parentheses") {
    List(
      "val result=new Service(_.id)" -> "val result=new Service({_.id})",
      "val result=new Service(_.id,42)" -> "val result=new Service({_.id},42)",
      "val result=new Service((value:Int)=>use(value))" ->
        "val result=new Service({(value:Int)=>use(value)})",
      "val result=call(using (value:Context)=>use(value))" ->
        "val result=call(using {(value:Context)=>use(value)})",
      "val result=call(using (left:Context)=>use(left),(right:Context)=>use(right))" ->
        "val result=call(using {(left:Context)=>use(left)},{(right:Context)=>use(right)})",
    ).foreach { case (naked, braced) =>
      assertEquals(format(naked), format(braced))
    }
    assertEquals(format("val result=new Service(_.id)"), "val result = new Service({ _.id })\n")
    assertEquals(format("val result=call(using (value:Context)=>use(value))"),
      "val result = call(using { (value: Context) => use(value) })\n")
  }

  test("named argument lambdas use braces without removing their enclosing argument clause") {
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
      assertEquals(format(naked), format(braced))
    }
    assertEquals(format("val result=call(callback=(value=>use(value)),other=42)"),
      "val result = call(callback = { value => use(value) }, other = 42)\n")
  }

  test("typed argument wrappers preserve their ascription around a braced lambda") {
    val naked = "val result=call((((value:Int)=>use(value)):Int=>Int),other)"
    val braced = "val result=call(({(value:Int)=>use(value)}:Int=>Int),other)"
    val result = format(naked)

    assertEquals(result, format(braced))
    assert(result.contains("{ (value: Int) => use(value) }"), result)
    assert(result.contains(": Int => Int"), result)
  }

  test("lambda argument braces remain when a body grows from one expression to multiple statements") {
    assertEquals(format("val result=call(value=>use(value),42)"),
      "val result = call({ value => use(value) }, 42)\n")
    val multiline = format("val result=call(value=>{prepare(value);use(value)},42)")

    assertEquals(multiline, format("val result=call({value=>prepare(value);use(value)},42)"))
    assertEquals(multiline,
      "val result =\n  call(\n    { value =>\n      prepare(value)\n      use(value)\n    },\n    42\n  )\n")
  }

  test("empty and declaration-only lambda bodies remain braced arguments in larger clauses") {
    List(
      "val result=call(value=>{},42)" -> "val result=call({value=>},42)",
      "val result=call(value=>{val next=value},42)" ->
        "val result=call({value=>val next=value},42)",
    ).foreach { case (naked, braced) =>
      assertEquals(format(naked), format(braced))
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
    assertEquals(result, format("val result=call({value=>use(value)},// second argument\n {other=>use(other)})"))
    val named = format("val result=call(callback=({value=>use(value)} // callback footer\n),other=/* other argument */42)")
    val namedApplication = named.parse[Source].get.stats.head.asInstanceOf[Defn.Val].rhs.asInstanceOf[Term.Apply]

    assert(namedApplication.argClause.values.head.pos.text.contains("// callback footer"), named)
    assert(!namedApplication.argClause.values.head.pos.text.contains("other argument"), named)
    assert(namedApplication.argClause.values(1).pos.text.contains("/* other argument */"), named)
  }

  test("standalone lambdas and a polymorphic lambda's directly nested function retain their scope") {
    assertEquals(format("val f=[A]=>(value:A)=>value"), "val f = [A] => (value: A) => value\n")
    assertEquals(format("val result=call([A]=>(value:A)=>value,42)"),
      "val result = call({ [A] => (value: A) => value }, 42)\n")
  }
}
