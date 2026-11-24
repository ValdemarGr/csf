package csf

import munit.FunSuite
import scala.meta.*

class TypeAssignmentBodyLayoutSuite extends FunSuite {
  private given Dialect = dialects.Scala3

  private def structure(value: Any): Any = value match {
    case Term.Block(List(term: Term)) => structure(term)
    case tree: Tree => (tree.productPrefix, tree.productIterator.map(structure).toList)
    case values: List[?] => values.map(structure)
    case value: Option[?] => value.map(structure)
    case other => other
  }

  private def check(source: String, expected: String, columns: Int = 100): Unit = {
    val result = Formatter.format(source, columns).fold(error => fail(error.getMessage), identity)
    val original = structure(source.parse[Source].get)
    val reparsed = {
      given Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
      result.parse[Source].get
    }
    assertEquals(result, expected)
    assertEquals(structure(result.parse[Source].get), original, result)
    assertEquals(structure(reparsed), original, result)
    assertEquals(
      result.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
      source.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
    )
    assertEquals(Formatter.format(result, columns), Right(result))
    assert(result.linesIterator.forall(_.length <= columns), result)
  }

  test("type aliases keep 100-column RHS inline and move 101-column RHS below equals") {
    val tpe = "T" + "x" * (99 - "type Result = ".length)
    val inlineExpected = s"type Result = $tpe"

    assertEquals(inlineExpected.length, 100)
    check(s"type Result=$tpe", inlineExpected + "\n")
    check(s"type Result=${tpe}x", s"type Result =\n  ${tpe}x\n")
  }

  test("opaque aliases use the same RHS width boundary") {
    val tpe = "T" + "x" * (99 - "opaque type Token = ".length)
    val inlineExpected = s"opaque type Token = $tpe"

    assertEquals(inlineExpected.length, 100)
    check(s"opaque type Token=$tpe", inlineExpected + "\n")
    check(s"opaque type Token=${tpe}x", s"opaque type Token =\n  ${tpe}x\n")
  }

  test("type lambdas keep their arrow and type arguments") {
    check("type Result[A]=[B]=>>Either[A,B]",
      "type Result[A] = [B] =>> Either[A, B]\n")
  }

  test("match types move below equals and retain separate case bodies") {
    check("type Result[X]=X match{case String=>Int\ncase _=>Boolean}",
      "type Result[X] =\n  X match {\n    case String => Int\n    case _ => Boolean\n  }\n")
  }

  test("bare refinements keep their opening brace beside equals") {
    check("type Protocol={def send(value:String):Int;type Result=String}",
      "type Protocol = {\n  def send(value: String): Int\n  type Result = String\n}\n")
  }

  test("refinements with a base type move the whole RHS below equals") {
    check("type Protocol=Base{def send(value:String):Int;type Result=String}",
      "type Protocol =\n  Base {\n    def send(value: String): Int\n    type Result = String\n  }\n")
  }

  test("a line comment before an alias RHS keeps its order and indentation") {
    check("type Result=// before\nOption[String]",
      "type Result =\n  // before\n  Option[String]\n")
  }

  test("named type arguments preserve commas and their sibling assignments") {
    check("val result=box[Element=String,Other=Int]",
      "val result = box[Element = String, Other = Int]\n")
  }

  test("long named type arguments wrap inside their RHS without separating its first token") {
    check("val result=box[Element=List[FirstVeryLongElementType,SecondVeryLongElementType],Other=Int]",
      "val result =\n  box[\n    Element = List[\n      FirstVeryLongElementType,\n      SecondVeryLongElementType\n    ],\n    Other = Int\n  ]\n", columns = 40)
  }

  test("named tuple and extractor patterns preserve assignment siblings") {
    check("val (name=first,count=second)=input",
      "val (name = first, count = second) = input\n")
    check("val Pair(name=first,count=second)=input",
      "val Pair(name = first, count = second) = input\n")
  }

  test("long named patterns wrap inside their extractor RHS without separating its first token") {
    check("val (name=Extractor(firstVeryLongVariableName,secondVeryLongVariableName),other=second)=input",
      "val (\n  name = Extractor(\n    firstVeryLongVariableName,\n    secondVeryLongVariableName\n  ),\n  other = second\n) = input\n", columns = 40)
  }

  test("a leading line comment retains necessary grouping in named type arguments") {
    check("val result=box[Element=(// before\n String),Other=Int]",
      "val result =\n  box[\n    Element = (\n      // before\n      String\n    ),\n    Other = Int\n  ]\n")
  }

  test("a leading line comment retains necessary grouping in named patterns") {
    check("val Pair(name=(// before\n value),other=tail)=input",
      "val Pair(\n  name = (\n    // before\n    value\n  ),\n  other = tail\n) = input\n")
  }

  test("standalone block comments retain required RHS grouping") {
    check("val result=box[Element= /* before */ String,Other=Int]",
      "val result =\n  box[\n    Element = (\n      /* before */\n      String\n    ),\n    Other = Int\n  ]\n")
    check("val Pair(name= /* before */ value,other=tail)=input",
      "val Pair(\n  name = (\n    /* before */\n    value\n  ),\n  other = tail\n) = input\n")
  }

  test("footer line comments cannot consume named type or pattern siblings") {
    check("val result=box[Element=(String // footer\n),Other=Int]",
      "val result =\n  box[\n    Element = String,\n    // footer\n    Other = Int\n  ]\n")
    check("val Pair(name=(value // footer\n),other=tail)=input",
      "val Pair(\n  name = value,\n  // footer\n  other = tail\n) = input\n")
  }

  test("leading comments retain grouping through transparent patterns and reuse tuple parentheses") {
    List("value: Value", "bound @ Some(value)", "Some(value)", "First | Second").foreach { pattern =>
      check(s"val Pair(name=(// before\n $pattern),other=tail)=input",
        s"val Pair(\n  name = (\n    // before\n    $pattern\n  ),\n  other = tail\n) = input\n")
    }
    check("val Pair(name=(// before\n (first,second)),other=tail)=input",
      "val Pair(\n  name = (\n    // before\n    first,\n    second\n  ),\n  other = tail\n) = input\n")
  }

  test("mixed outer block and inner line comments share a stable RHS prefix") {
    check("val result=box[Element= /* outer */ (// inner\n String),Other=Int]",
      "val result =\n  box[\n    Element = (\n      /* outer */\n      // inner\n      String\n    ),\n    Other = Int\n  ]\n")
    check("val Pair(name= /* outer */ (// inner\n value),other=tail)=input",
      "val Pair(\n  name = (\n    /* outer */\n    // inner\n    value\n  ),\n  other = tail\n) = input\n")
  }

  test("comment-led sequence patterns retain direct RHS and align continuation with the name") {
    check("val Wrap(values= /* first */ /* second */ rest*)=input",
      "val Wrap(\n  values =\n  /* first */\n  /* second */\n  rest*\n) = input\n")
    check("val Wrap(\n values= // before\n rest*\n)=input",
      "val Wrap(\n  values =\n  // before\n  rest*\n) = input\n")
    check("val Wrap(\n values= /* before\n after */\n rest*\n)=input",
      "val Wrap(\n  values =\n  /* before\n after */\n  rest*\n) = input\n")
  }

  test("secondary constructors keep their required initial this call and braces") {
    check("class Example(seed:Int){def this()=this(0)}",
      "class Example(seed: Int) {\n  def this() = {\n    this(0)\n  }\n}\n")
    check("class Example(seed:Int){def this()={this(0);initialize()}}",
      "class Example(seed: Int) {\n  def this() = {\n    this(0)\n    initialize()\n  }\n}\n")
  }

  test("constructor comments remain between the initial call and following statements") {
    check("class Example(seed:Int){def this()={this(0);/* next */initialize()}}",
      "class Example(seed: Int) {\n  def this() = {\n    this(0)\n    /* next */\n    initialize()\n  }\n}\n")
  }
}
