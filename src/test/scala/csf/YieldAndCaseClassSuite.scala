package csf

import munit.FunSuite
import scala.meta.*

class YieldAndCaseClassSuite extends FunSuite {
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
    assert(result.linesIterator.forall(line => line == line.stripTrailing()), result)
    result
  }

  test("direct and explicitly braced single-expression yields share their direct layout") {
    val expected = "val result =\n  for {\n    x <- xs\n  } yield x\n"

    assertEquals(format("val result=for(x<-xs)yield x"), expected)
    assertEquals(format("val result=for(x<-xs)yield{x}"), expected)
    assertEquals(format("val result=for(x<-xs)yield{{x}}"), expected)
    assertEquals(format("val result=for(x<-xs)yield{x+1}"),
      "val result =\n  for {\n    x <- xs\n  } yield x + 1\n")
  }

  test("a wrapped single-expression call yield does not gain an outer block") {
    val first = "first" + "Argument" * 5
    val second = "second" + "Argument" * 5
    val result = format(s"val result=for(x<-xs)yield{combine($first,$second)}")

    assert(result.contains("} yield combine(\n"), result)
    assert(result.linesIterator.forall(_.length <= 100), result)
  }

  test("a wrapped single-expression chain yield does not gain an outer block") {
    val receiver = "r" * 80
    val result = format(s"val result=for(x<-xs)yield{$receiver.first().second()}")

    assert(result.contains(s"} yield $receiver\n"), result)
    assert(!result.contains("} yield {"), result)
    assert(result.linesIterator.forall(_.length <= 100), result)
  }

  test("yield drops a leading-only wrapper and retains blocks with footer comments") {
    val leading = format("val result=for(x<-xs)yield{/* before */x}")
    assert(!leading.contains("} yield {"), leading)
    List(
      "val result=for(x<-xs)yield{x // footer\n}",
      "val result=for(x<-xs)yield{/* before */x /* footer */}",
      "object Example{val result=for(x<-xs)yield{x // footer\n};val next=after}",
      "val result=(for(x<-xs)yield{x // footer\n}).map(f)",
      "val result=(for(x<-xs)yield{x // footer\n}) + other",
    ).foreach { source =>
      val result = format(source)
      assert(result.contains("} yield {"), result)
    }
  }

  test("multiple statements and declaration-only yield bodies keep their block") {
    assertEquals(format("val result=for(x<-xs)yield{prepare(x);x}"),
      "val result =\n  for {\n    x <- xs\n  } yield {\n    prepare(x)\n    x\n  }\n")
    assertEquals(format("val result=for(x<-xs)yield{val next=x}"),
      "val result =\n  for {\n    x <- xs\n  } yield {\n    val next = x\n  }\n")
  }

  test("nested conditional yields preserve outer else ownership") {
    val result = format(
      "val result=if(outer){for(x<-xs)yield{if(inner){first}}}else{fallback}",
    )

    assert(result.contains("yield if inner then"), result)
    assert(!result.contains("} yield {"), result)
  }

  test("empty plain final and generic case classes omit their body with or without source braces") {
    List(
      "case class Empty()" -> "case class Empty()\n",
      "final case class Point(value:Int)" -> "final case class Point(value: Int)\n",
      "case class Box[A](value:A)" -> "case class Box[A](value: A)\n",
    ).foreach { case (source, expected) =>
      assertEquals(format(source), expected)
      assertEquals(format(source + "{}"), expected)
    }
  }

  test("empty case classes retain extends and derives clauses without body braces") {
    List(
      "case class Point(value:Int)extends Parent(value)" ->
        "case class Point(value: Int) extends Parent(value)\n",
      "case class Box[A](value:A)derives Show,Eq" ->
        "case class Box[A](value: A) derives Show, Eq\n",
      "case class Box[A](value:A)extends Base[A] derives Show" ->
        "case class Box[A](value: A) extends Base[A] derives Show\n",
    ).foreach { case (source, expected) =>
      assertEquals(format(source), expected)
      assertEquals(format(source + "{}"), expected)
    }
  }

  test("comment-only case-class bodies keep the comment inside their braces") {
    assertEquals(format("case class Empty(){}\n// companion\n// details"),
      "case class Empty()\n// companion\n// details\n")
    assertEquals(format("case class Empty(){/* keep */}"),
      "case class Empty() {\n  /* keep */\n}\n")
    assertEquals(format("case class Empty(){// keep\n}"),
      "case class Empty() {\n  // keep\n}\n")
    format("case class C() /* keep */ {}")
    format("case class C() extends Parent /* keep */ {}")
  }

  test("case-class self aliases and self types keep their body braces") {
    assertEquals(format("case class Empty(){self=>}"),
      "case class Empty() {\n  self =>\n}\n")
    assertEquals(format("case class Empty(){self:Requirement=>}"),
      "case class Empty() {\n  self: Requirement =>\n}\n")
  }

  test("nonempty case-class bodies retain their declarations") {
    assertEquals(format("case class Box(value:Int){def get=value}"),
      "case class Box(value: Int) {\n  def get = value\n}\n")
  }

  test("empty regular classes and traits omit their body with or without source braces") {
    val privateConstructor = "final class UIDescription private (val setup: Setup[UIDescription.Api])"
    List(
      "class Empty" -> "class Empty\n",
      privateConstructor -> (privateConstructor + "\n"),
      "trait Base" -> "trait Base\n",
      "trait Container[A]" -> "trait Container[A]\n",
      "trait Container[A]extends Parent[A]" -> "trait Container[A] extends Parent[A]\n",
    ).foreach { case (source, expected) =>
      assertEquals(format(source), expected)
      assertEquals(format(source + "{}"), expected)
    }
  }

  test("empty objects and case objects omit bodies with or without source braces") {
    List(
      "object Empty",
      "case object Missing",
      "case object Ready extends Status",
    ).foreach { source =>
      assertEquals(format(source), source + "\n")
      assertEquals(format(source + "{}"), source + "\n")
    }
  }

  test("nested empty traits and objects remain separate members after body omission") {
    assertEquals(format("object Container{trait Marker{};object Empty{};case object Ready extends Status{}}"),
      "object Container {\n  trait Marker\n  object Empty\n  case object Ready extends Status\n}\n")
  }

  test("class trait and object comments self types and declarations retain their body braces") {
    List("class Empty", "trait Marker", "object Empty", "case object Ready").foreach { header =>
      assertEquals(format(header + "{/* keep */}"), header + " {\n  /* keep */\n}\n")
      assertEquals(format(header + "{self:Requirement=>}"), header + " {\n  self: Requirement =>\n}\n")
      assertEquals(format(header + "{val size=1}"), header + " {\n  val size = 1\n}\n")
      format(header + " /* before */ {}")
      format(header + " extends Parent /* before */ {}")
    }
  }

  test("trailing comments do not introduce an empty trait or object body") {
    List("trait Marker", "object Empty", "case object Ready").foreach { header =>
      val trailing = "\n// companion\n// details"
      assertEquals(format(header + "{}" + trailing), header + trailing + "\n")
      assertEquals(format(header + trailing), header + trailing + "\n")
    }
    assertEquals(format("trait Marker{}\n// next member\nobject Empty{}"),
      "trait Marker\n// next member\nobject Empty\n")
  }

  test("AggregateStatus omits empty bodies while preserving declaration gaps") {
    val source = "sealed trait AggregateStatus extends EnumEntry.Snakecase {}\n" +
      "object AggregateStatus extends Enum[AggregateStatus] {\n" +
      "  val values = findValues\n\n" +
      "  case object Progress extends AggregateStatus {}\n" +
      "  case object Completed extends AggregateStatus {}\n" +
      "  case object Failed extends AggregateStatus {}\n}"
    val expected = "sealed trait AggregateStatus extends EnumEntry.Snakecase\n" +
      "object AggregateStatus extends Enum[AggregateStatus] {\n" +
      "  val values = findValues\n\n" +
      "  case object Progress extends AggregateStatus\n" +
      "  case object Completed extends AggregateStatus\n" +
      "  case object Failed extends AggregateStatus\n}\n"

    assertEquals(format(source), expected)
  }
}
