package csf

import munit.FunSuite
import scala.meta.*

class FormatterSuite extends FunSuite {
  private given Dialect = dialects.Scala3

  private def formatted(source: String): String = {
    val result = Formatter.format(source).fold(error => fail(error.getMessage), identity)
    given Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    result.parse[Source] match {
      case Parsed.Success(_) => result
      case error: Parsed.Error => fail(s"Output requires braces or is invalid: $error\n$result")
    }
  }

  private def structure(source: String): Any = {
    def normalize(value: Any): Any = value match {
      case Term.Block(List(term: Term)) => normalize(term)
      case tree: Tree => (tree.productPrefix, tree.productIterator.map(normalize).toList)
      case values: List[?] => values.map(normalize)
      case value: Option[?] => value.map(normalize)
      case other => other
    }

    normalize(source.parse[Source].get)
  }

  private def comments(source: String): List[String] = {
    source.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList
  }

  test("method boundaries preserve a source blank line") {
    val compact = "object Example{val x=1;def echo(value:Int):Int=value}"
    val spaced = "object  Example  {\n\n val   x =  1\n\n def echo ( value : Int ) : Int = value\n}"
    val expected =
      """object Example {
        |  val x = 1
        |  def echo(value: Int): Int = value
        |}
        |""".stripMargin

    assertEquals(formatted(compact), expected)
    assertEquals(formatted(spaced), expected.replace("\n  def", "\n\n  def"))
  }

  test("significant indentation for an object body returns a parse error") {
    val source =
      """object Example:
        |  val x = 1
        |  def echo(value: Int): Int = value
        |""".stripMargin
    Formatter.format(source) match {
      case Left(FormatError.Parse(details)) => assert(details.nonEmpty)
      case other => fail(s"Expected parse error, got $other")
    }
  }

  test("empty source remains empty") {
    assertEquals(formatted(""), "")
    assertEquals(formatted("  \n\t\n"), "")
  }

  test("if without else does not acquire an else branch") {
    val source = "object Example { def run(): Unit = if (ready) action() }"
    val result = formatted(source)

    assert(!result.contains("else"), result)
    assertEquals(structure(result), structure(source))
    assertEquals(formatted(result), result)
  }

  test("explicit else unit remains present") {
    val source = "object Example { def run(): Unit = if (ready) action() else () }"
    val result = formatted(source)

    assert(result.contains("else"), result)
    assertEquals(structure(result), structure(source))
    assertEquals(formatted(result), result)
  }

  private val programs = List(
    "package example; import scala.collection.immutable.List; object Example { val xs: List[Int] = List(1, 2) }",
    "package first { object A }; package second { object B }",
    "object Example { def choose(flag: Boolean, left: Int, right: Int): Int = if (flag) left else right }",
    "object Example { def label(value: Int): String = value match { case 0 => \"zero\"; case _ => \"other\" } }",
    "object Example { val values = input.map((value: Int) => convert(value)) }",
    "object Example { def run(value: Int): Int = { val result = convert(value); result } }",
    "object Example { val result = (if (ready) left else right).apply(value) }",
    "object Example { val result = (a :: b) :: c }",
    "object Example { val result = a :: (b :: c) }",
    "object Example { val result = a op (()) }",
    "object Example { val result = a op ((one, two)) }",
    "object Example { def foo(using Int): Int = 1 }",
    "object Example { val magnitude = (-1).abs; val text = (-1.0).toString }",
    "object Example { val text = (-0.0).toString }",
    "object Example { val `type` = 1; val result = `type` }",
  )

  programs.zipWithIndex.foreach { case (source, index) =>
    test(s"supported syntax preserves structure and is idempotent: $index") {
      val result = formatted(source)

      assertEquals(structure(result), structure(source))
      assertEquals(formatted(result), result)
      assert(result.endsWith("\n"))
      assert(!result.endsWith("\n\n"))
    }
  }

  test("long argument lists use the configured line width") {
    val arguments = (1 to 15).map(index => f"argumentNumber$index%02d").mkString(",")
    val source = s"object Example { val result = function($arguments) }"
    val result = formatted(source)

    assert(result.linesIterator.forall(_.length <= 100), result)
    assertEquals(structure(result), structure(source))
    assertEquals(formatted(result), result)
  }

  test("literal contents survive formatting") {
    val source =
      "object Example { val café = \"  hello\\nworld  \"; " +
        "val text = \"\"\"first\n    second\n\nthird\"\"\"; " +
        "val escaped = '\\n'; val number = 0xFF; val decimal = 1.2500; " +
        "val mask = 0xFFFF_FFFF.toHexString; val signed = (-0xFFFF_FFFF).toHexString; val bits = 0B10_01L }"
    val result = formatted(source)

    assertEquals(structure(result), structure(source))
    assert(result.contains("\"  hello\\nworld  \""))
    assert(result.contains("\"\"\"first\n    second\n\nthird\"\"\""))
    assert(result.contains("val number = 0xFF"), result)
    assert(result.contains("val mask = 0xFFFF_FFFF.toHexString"), result)
    assert(result.contains("val signed = (-0xFFFF_FFFF).toHexString"), result)
    assert(result.contains("val bits = 0B10_01L"), result)
    assertEquals(formatted(result), result)
  }

  private val commentedPrograms = List(
    "// header\nobject Example {\n/** value docs */\nval x = 1 // trailing\n// dangling\n}\n// footer\n",
    "object Example { /* empty body */ }",
    "object Example { def run(): Unit = { /* empty block */ } }",
    "object Example { val x = function(/* first */ one, /* second */ two /* last */) }",
    "object Example { val x = function(/* empty arguments */) }",
    "object Example { def run(/* empty parameters */): Unit = () }",
    "object Example { val /* before pattern */ x /* before equals */ = /* before value */ 1 }",
    "/* file-only comment */\n// another comment\n",
    "trait Example\n// first\n\n// second\n",
    "object Example { val result = function(one,\n// first\n// second\ntwo) }",
    "object Example { val result = function(one,\n// first\n\n// second\ntwo) }",
    "object Example { val result = function(implicit value =>\n// first\n\n// second\nvalue) }",
  )

  commentedPrograms.zipWithIndex.foreach { case (source, index) =>
    test(s"comments survive exactly once and remain idempotent: $index") {
      val result = formatted(source)

      assertEquals(comments(result), comments(source))
      assertEquals(structure(result), structure(source))
      assertEquals(formatted(result), result)
    }
  }

  test("malformed source returns a parse error") {
    Formatter.format("object Example {") match {
      case Left(FormatError.Parse(details)) => assert(details.nonEmpty)
      case other => fail(s"Expected parse error, got $other")
    }
  }

  private val scala3Programs = List(
    "classes and traits" -> "trait Base[A] { def get: A }; final case class Box[+A](value: A) extends Serializable",
    "enums" -> "enum Result[+A] { case Empty, Missing; case Value(value: A) }",
    "given aliases" -> "given value: Int = 1",
    "given instances" -> "given [A](using ord: Ordering[A]): Ordering[List[A]] with { def compare(left: List[A], right: List[A]): Int = 0 }",
    "extensions" -> "extension [A](values: List[A]) { def first: Option[A] = values.headOption }",
    "type aliases" -> "opaque type Id = Int; type Pair[A] = (A, A)",
    "for comprehensions" -> "object Example { val result = for { a <- xs; b = convert(a) } yield b }",
    "try expressions" -> "object Example { val result = try run() catch { case error: Exception => throw error } finally cleanup() }",
    "extractor patterns" -> "object Example { val result = value match { case Some(result @ Left(_)) => result; case _ => fallback } }",
    "implicit lambdas" -> "object Example { val result = values.map(implicit value => value) }",
    "placeholder infix expressions" -> "object Example { val result = _ + \".\" + name }",
    "interpolation" -> "object Example { val result = s\"hello $value\" }",
    "XML splices" -> "object Example { val result = <root>{value}</root> }",
    "infix constructor types" -> "object Example { val result = new (F ~> G) { def apply[A](a: F[A]): G[A] = f(a) } }",
    "function parents" -> "trait Raise[-E1, +E2] extends (E1 => E2)",
    "typed infix patterns" -> "object Example { val result = value match { case _: (h *: t) => h } }",
    "double literal types" -> "object Example { val first = -4832.00; val second = 1337d }",
    "context function and parameter types" -> "trait Example { def run(input: => Int, values: String*): Context ?=> Int }",
  )

  scala3Programs.foreach { case (feature, source) =>
    test(s"Scala 3 $feature preserves structure, comments, and canonical output") {
      val result = formatted(source)

      assertEquals(structure(result), structure(source))
      assertEquals(comments(result), comments(source))
      assertEquals(formatted(result), result)
    }
  }
}
