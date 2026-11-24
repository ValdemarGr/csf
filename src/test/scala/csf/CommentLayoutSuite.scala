package csf

import munit.FunSuite
import scala.meta.*

class CommentLayoutSuite extends FunSuite {
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
    val (original, reparsed) = {
      given Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
      (source.parse[Source].get, result.parse[Source].get)
    }
    assertEquals(structure(reparsed), structure(original), result)
    assertEquals(structure(result.parse[Source].get), structure(original), result)
    val comments = result.tokenize.get.collect { case comment: Token.Comment => comment }.toList
    assertEquals(
      comments.map(_.text),
      source.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
    )
    val lines = result.linesIterator.toVector
    comments.foreach { comment =>
      assert(lines(comment.pos.startLine).take(comment.pos.startColumn).forall(_.isWhitespace), result)
      assert(lines(comment.pos.endLine).drop(comment.pos.endColumn).forall(_.isWhitespace), result)
    }
    assertEquals(Formatter.format(result), Right(result))
    assert(lines.forall(line => line == line.stripTrailing()), result)
    result
  }

  test("a currency line comment stays between fields without a blank line before it") {
    val source = "case class Invoice(name:String,currency:Currency,// Currency\ncustomer:CustomerId)"
    val expected = "case class Invoice(\n  name: String,\n  currency: Currency,\n" +
      "  // Currency\n  customer: CustomerId\n)\n"

    assertEquals(format(source), expected)
    assertEquals(format("case class Invoice(\nname:String,\ncurrency:Currency,\n// Currency\ncustomer:CustomerId\n)"), expected)
  }

  test("inline and standalone block comments before fields converge") {
    assertEquals(
      format("case class Invoice(name:String,/* Currency */currency:Currency,customer:CustomerId)"),
      format("case class Invoice(name:String,\n/* Currency */\ncurrency:Currency,customer:CustomerId)"),
    )
  }

  test("parameter and argument block comments always occupy their own lines") {
    assertEquals(
      format("def use(/* parameter */value:Int)=call(/* argument */value)"),
      format("def use(\n/* parameter */\nvalue:Int)=call(\n/* argument */\nvalue)"),
    )
  }

  test("doc comments precede their declarations without sharing a line") {
    val expected = "/** Class docs. */\nclass Example {\n  /** Value docs. */\n  val value = 1\n}\n"

    assertEquals(format("/** Class docs. */class Example{/** Value docs. */val value=1}"), expected)
    assertEquals(format("/** Class docs. */\nclass Example{\n/** Value docs. */\nval value=1\n}"), expected)
    format("/** Class docs.\n * Exact indentation.\n */class Example{}")
  }

  test("multiline block comment contents and lexical comment order remain exact") {
    format("object Example{/* before */val value=/* keep\n exact */next;/* after */val other=value}")
    assertEquals(format("val value=/* first *//* second */next"),
      format("val value=\n/* first */\n/* second */\nnext"))
  }

  test("trailing and standalone line comments between declarations converge") {
    val expected = "object Example {\n  val value = 1\n  // footer\n  val other = 2\n}\n"

    assertEquals(format("object Example{val value=1 // footer\nval other=2}"), expected)
    assertEquals(format("object Example{val value=1\n// footer\nval other=2}"), expected)
  }

  test("semicolon selection and infix boundaries retain their grammar around standalone comments") {
    List(
      "object Example{first();/* boundary */(left,right);next()}" ->
        "object Example{first();\n/* boundary */\n(left,right);next()}",
      "val result=service /* access */ .load(id).result" ->
        "val result=service\n/* access */\n.load(id).result",
      "val result=left + /* operand */right" -> "val result=left +\n/* operand */\nright",
    ).foreach { case (inlineSource, standaloneSource) =>
      assertEquals(format(inlineSource), format(standaloneSource))
    }
  }

  test("named type RHS comments retain required parentheses and assignment siblings") {
    assertEquals(
      format("val result=box[Element=/* type */String,Other=Int]"),
      format("val result=box[Element=(\n/* type */\nString),Other=Int]"),
    )
  }

  test("named pattern RHS comments retain their value and following assignment") {
    assertEquals(
      format("val Pair(name=/* pattern */value,other=tail)=input"),
      format("val Pair(name=(\n/* pattern */\nvalue),other=tail)=input"),
    )
  }

  test("named repeated patterns keep grammar-valid unparenthesized RHS comments") {
    assertEquals(
      format("val Pair(other=tail,name=/* sequence */rest*)=input"),
      format("val Pair(other=tail,name=/* sequence */\nrest*)=input"),
    )
  }

  test("keyword type and delimiter seams preserve grammar around standalone comments") {
    val returned = format("def returned=return /* returned value */value")

    assert(returned.contains("return (\n"), returned)
    List(
      "def fail=throw /* thrown value */error",
      "val result=new /* constructed type */Service /* constructor arguments */(value)",
      "class /* declaration name */Example{def /* method name */value:Int=1}",
      "def value(input /* parameter colon */:Int) /* return colon */:String=\"value\"",
      "type Function=Int /* arrow */=>/* result type */String",
      "type Range[A /* lower bound */>:Nothing /* upper bound */<:Any]=A",
      "val result=for{value /* generator */<-values}yield value",
      "val result= - /* unary operand */value",
      "class Service(value:Int){def this() /* constructor body */=this /* constructor arguments */(0);" +
        "def call=service /* call arguments */(value);def typed=service /* type arguments */[Int](value)}",
    ).foreach { source =>
      format(source)
    }
  }

  test("given modifier parent and pattern seams preserve standalone comments and syntax") {
    List(
      "given /*c*/ named: Service = service",
      "given named: /*c*/ Service = service",
      "given /*c*/ Service = service",
      "private given /*c*/ named: Service = service",
      "given /*c*/ Service with {}",
      "private given /*c*/ Service with {}",
      "inline /*a*/ given /*b*/ named: Service = service",
      "given named[A](value:A): /*c*/ Service[A] = service(value)",
      "private /*c*/ final class Example",
      "@first /*c*/ @second class Example",
      "enum E { case A extends /*c*/ E }",
      "enum E { case A extends E /*c*/ with Tag }",
      "val result=values /*c*/ size",
      "val left /*c*/ :: right = input",
      "val left :: /*c*/ right = input",
    ).foreach { source =>
      format(source)
    }
  }
}
