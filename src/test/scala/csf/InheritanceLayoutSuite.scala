package csf

import munit.FunSuite
import scala.meta.*

class InheritanceLayoutSuite extends FunSuite {
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
    assertEquals(Formatter.format(result, columns), Right(result))
    assert(result.linesIterator.forall(line => line == line.stripTrailing()), result)
    result
  }

  test("InsertKey expands its fields and keeps a fitting parent after the closing constructor parenthesis") {
    val source = "final case class InsertKey[H,A](header:H,tableName:String,typeId:TypeId[A])extends DSKey[A,Unit]"

    assertEquals(format(source),
      "final case class InsertKey[H, A](\n  header: H,\n  tableName: String,\n  typeId: TypeId[A]\n" +
        ") extends DSKey[A, Unit]\n")
  }

  test("an inheritance header fitting exactly 100 columns remains inline") {
    val name = "C" * (100 - "case class (value: Int) extends Parent".length)
    val expected = s"case class $name(value: Int) extends Parent\n"

    assertEquals(expected.linesIterator.next().length, 100)
    assertEquals(format(s"case class $name(value:Int)extends Parent"), expected)
  }

  test("a 101-column inheritance header breaks before extends without breaking the constructor") {
    val name = "C" * (101 - "case class (value: Int) extends Parent".length)

    assertEquals(s"case class $name(value: Int) extends Parent".length, 101)
    assertEquals(format(s"case class $name(value:Int)extends Parent"),
      s"case class $name(value: Int)\n  extends Parent\n")
  }

  test("an already oversized constructor and a parent with oversized arguments can wrap independently") {
    val source = "case class Child(first:FirstArgument,second:SecondArgument)extends Parent[FirstArgument,SecondArgument]"
    val result = format(source, 40)

    assertEquals(result,
      "case class Child(\n  first: FirstArgument,\n  second: SecondArgument\n)\n" +
        "  extends Parent[\n    FirstArgument,\n    SecondArgument\n  ]\n")
    assert(result.linesIterator.forall(_.length <= 40), result)
  }

  test("a broken inheritance header does not add indentation to members or the closing body brace") {
    val source = "object Outer{final class InsertKey[H,A](header:H,tableName:String,typeId:TypeId[A])extends DSKey[A,Unit]{val value=1}}"

    assertEquals(format(source),
      "object Outer {\n" +
        "  final class InsertKey[H, A](header: H, tableName: String, typeId: TypeId[A])\n" +
        "    extends DSKey[A, Unit] {\n" +
        "    val value = 1\n" +
        "  }\n" +
        "}\n")
  }

  test("short class trait object enum and package-object inheritance stays inline") {
    List(
      "class Child extends Parent{}" -> "class Child extends Parent {}\n",
      "trait Child extends Parent{}" -> "trait Child extends Parent\n",
      "object Child extends Parent{}" -> "object Child extends Parent\n",
      "enum Child extends Parent{case Value}" -> "enum Child extends Parent {\n  case Value\n}\n",
      "enum Parent{case Child extends Parent}" -> "enum Parent {\n  case Child extends Parent\n}\n",
      "package object child extends Parent{}" -> "package object child extends Parent {}\n",
    ).foreach { case (source, expected) =>
      assertEquals(format(source), expected)
    }
  }

  test("long trait object enum case and package-object headers break before extends") {
    List(
      "trait LongerChildName[A] extends Parent[A]" ->
        "trait LongerChildName[A]\n  extends Parent[A]\n",
      "object LongerChildName extends Parent" ->
        "object LongerChildName\n  extends Parent\n",
      "enum LongerChildName extends Parent{case Value}" ->
        "enum LongerChildName\n  extends Parent {\n  case Value\n}\n",
      "enum Parent{case LongerChildName(value:Int)extends Parent}" ->
        "enum Parent {\n  case LongerChildName(value: Int)\n    extends Parent\n}\n",
      "package object longerChildName extends Parent{}" ->
        "package object longerChildName\n  extends Parent {}\n",
    ).foreach { case (source, expected) =>
      assertEquals(format(source, 34), expected)
    }
  }

  test("multiple parents derives and parent comments retain their order") {
    assertEquals(format("case class Child(value:Int)extends First with Second derives Codec"),
      "case class Child(value: Int) extends First with Second derives Codec\n")
    val source = "case class LongerChildName(value:Int)extends /* first */ First with /* second */ Second derives Codec"
    val result = format(source, 60)

    assertEquals(result,
      "case class LongerChildName(value: Int)\n" +
        "  extends\n  /* first */\n  First with\n  /* second */\n  Second derives Codec\n")
    assertEquals(format("trait Child extends // parent\n Parent {\n // body\n def value:Int\n}"),
      "trait Child\n  extends\n  // parent\n  Parent {\n  // body\n  def value: Int\n}\n")
  }
}
