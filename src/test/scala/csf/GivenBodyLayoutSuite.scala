package csf

import munit.FunSuite
import scala.meta.*

class GivenBodyLayoutSuite extends FunSuite {
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
    assert(result.linesIterator.forall(line => line == line.stripTrailing()), result)
    result
  }

  test("context-bound given aliases wrap call arguments with the opener next to equals") {
    val source = "given [H: TypeId as tidH, A: TypeId as tidA]: TypeId[HeaderRow[H,A]]=TypeIdHeaderRowImpl(tidH,tidA)"

    assertEquals(format(source),
      "given [H: TypeId as tidH, A: TypeId as tidA]: TypeId[HeaderRow[H, A]] = TypeIdHeaderRowImpl(\n" +
        "  tidH,\n  tidA\n)\n")
  }

  test("short named anonymous and polymorphic given aliases remain inline") {
    assertEquals(format("given answer:Int=42"), "given answer: Int = 42\n")
    assertEquals(format("given Int=42"), "given Int = 42\n")
    assertEquals(format("given empty[A]:List[A]=Nil"), "given empty[A]: List[A] = Nil\n")
    assertEquals(format("given [A]:List[A]=Nil"), "given [A]: List[A] = Nil\n")
  }

  test("contextual parameters context-bound aliases and arrow signatures retain their structure") {
    assertEquals(format("given [A](using value:Value[A]):Output[A]=build(value)"),
      "given [A](using value: Value[A]): Output[A] = build(value)\n")
    assertEquals(format("given typed[A:TypeId as tidA]:TypeId[List[A]]=listTypeId(tidA)"),
      "given typed[A: TypeId as tidA]: TypeId[List[A]] = listTypeId(tidA)\n")
    assertEquals(format("given [A] => (value:Value[A]) => Output[A]=build(value)"),
      "given [A] => (value: Value[A]) => Output[A] = build(value)\n")
  }

  test("given RHS expressions break after equals only beyond the configured width") {
    List(40, 100).foreach { columns =>
      val text = "x" * (columns - "given value: String = \"\"".length)
      val expected = s"given value: String = \"$text\"\n"

      assertEquals(expected.linesIterator.next().length, columns)
      assertEquals(format(s"given value:String=\"$text\"", columns), expected)
      assertEquals(format(s"given value:String=\"${text}x\"", columns),
        s"given value: String =\n  \"${text}x\"\n")
    }
  }

  test("mandatory multiline given RHS expressions start below equals") {
    assertEquals(format("given answer:Int=if(ready)one else other"),
      "given answer: Int =\n  if ready then {\n    one\n  } else {\n    other\n  }\n")
    val receiver = "r" * 86
    assertEquals(format(s"given result:Result=$receiver.first().second()"),
      s"given result: Result =\n  $receiver\n    .first()\n    .second()\n")
  }

  test("retained multistatement empty and declaration-only given blocks keep their opening brace beside equals") {
    assertEquals(format("given service:Service={prepare();build()}"),
      "given service: Service = {\n  prepare()\n  build()\n}\n")
    assertEquals(format("given marker:Unit={}"), "given marker: Unit = {\n}\n")
    assertEquals(format("given service:Service={val next=prepare()}"),
      "given service: Service = {\n  val next = prepare()\n}\n")
    assertEquals(format("given answer:Int={{{42}}}"), "given answer: Int = 42\n")
  }

  test("Transformer given lambda owns its braces and keeps body statements at one indent") {
    val source = "given Transformer[CreditorFlag,creditro.CreditorFlag]={x=>" +
      "val f=x match{case Disabled()=>disabled;case CreateAndNotify()=>enabled}" +
      "\n\ncreditro.CreditorFlag.of(f)}"
    val expected = "given Transformer[CreditorFlag, creditro.CreditorFlag] = { x =>\n" +
      "  val f =\n    x match {\n      case Disabled() => disabled\n" +
      "      case CreateAndNotify() => enabled\n    }\n\n  creditro.CreditorFlag.of(f)\n}\n"

    assertEquals(format(source), expected)
    assertEquals(format("given Transformer[CreditorFlag,creditro.CreditorFlag]=x=>{" +
      "val f=x match{case Disabled()=>disabled;case CreateAndNotify()=>enabled}" +
      "\n\ncreditro.CreditorFlag.of(f)}"), expected)
  }

  test("nested given lambda body aligns with its header rather than adding a block indent") {
    val source = "object Example{given Transformer[Input,Output]=value=>{" +
      "val next=prepare(value);build(next)}}"
    val expected = "object Example {\n  given Transformer[Input, Output] = { value =>\n" +
      "    val next = prepare(value)\n    build(next)\n  }\n}\n"

    assertEquals(format(source), expected)
  }

  test("partial-function given aliases retain one brace pair beside equals and preserve comments") {
    val source = "given matcher:PartialFunction[Int,Int]={\n// cases\ncase value=>value\n// footer\n}"
    val expected = "given matcher: PartialFunction[Int, Int] = {\n  // cases\n  case value =>\n    value\n    // footer\n}\n"

    assertEquals(format(source), expected)
    assertEquals(format("given matcher:PartialFunction[Int,Int]={{{case value=>value}}}"),
      "given matcher: PartialFunction[Int, Int] = {\n  case value => value\n}\n")
  }

  test("given RHS comments retain standalone leading and footer placement") {
    assertEquals(format("given answer:Int=/* body */42"), "given answer: Int =\n  /* body */\n  42\n")
    assertEquals(format("given answer:Int=// note\n42"), "given answer: Int =\n  // note\n  42\n")
    assertEquals(format("given answer:Int={42 // footer\n}"), "given answer: Int = {\n  42\n  // footer\n}\n")
  }

  test("nested given aliases indent their RHS below their declaration") {
    assertEquals(format("object Example{given answer:Int=if(ready)one else other}"),
      "object Example {\n  given answer: Int =\n    if ready then {\n      one\n    } else {\n      other\n    }\n}\n")
  }

  test("function and polymorphic result-type parentheses remain in the given signature") {
    assertEquals(format("given (()=>Int)=()=>1"), "given (() => Int) = () => 1\n")
    assertEquals(format("given mapper:(Int=>Int)=(value:Int)=>value"),
      "given mapper: (Int => Int) = (value: Int) => value\n")
    assertEquals(format("given ([A]=>A=>A)=[A]=>(value:A)=>value"),
      "given ([A] => A => A) = [A] => (value: A) => value\n")
  }

  test("given templates retain their with-body layout") {
    assertEquals(format("given Service with{def run():Unit=()}"),
      "given Service with {\n  def run(): Unit = ()\n}\n")
  }
}
