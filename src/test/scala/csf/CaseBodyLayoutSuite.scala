package csf

import munit.FunSuite
import scala.meta.*

class CaseBodyLayoutSuite extends FunSuite {
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
    assertEquals(structure(reparsed), structure(source.parse[Source].get), result)
    assertEquals(
      result.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
      source.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
    )
    assertEquals(Formatter.format(result), Right(result))
  }

  test("direct and explicitly blocked single-expression match bodies share their short layout") {
    val expected = "val result =\n  input match {\n    case x => use(x)\n  }\n"

    check("val result=input match{case x=>use(x)}", expected)
    check("val result=input match{case x=>{use(x)}}", expected)
  }

  test("multistatement match bodies emit local declarations and expressions below their arrow") {
    check("val result=input match{case x=>{val next=prepare(x);log(next);use(next)}}",
      "val result =\n  input match {\n    case x =>\n      val next = prepare(x)\n      log(next)\n      use(next)\n  }\n")
  }

  test("multiple partial-function cases keep their bodies separate after removing body braces") {
    check("val f={case Some(x)=>{val next=prepare(x);use(next)};case _=>{fallback}}",
      "val f = {\n  case Some(x) =>\n    val next = prepare(x)\n    use(next)\n  case _ => fallback\n}\n")
  }

  test("a tuple partial-function argument keeps its case header inline above its statements") {
    check("val result=rows.map{case(name,values)=>{val attribute=Attribute(name,values);attribute}}",
      "val result = rows.map{ case (name, values) =>\n  val attribute = Attribute(name, values)\n  attribute\n}\n")
  }

  test("catch cases emit their statements directly without another pair of body braces") {
    check("val result=try action() catch{case error:Error=>{log(error);recover(error)}}",
      "val result =\n  try {\n    action()\n  } catch {\n    case error: Error =>\n      log(error)\n      recover(error)\n  }\n")
  }

  test("empty match and partial-function bodies retain empty case semantics without empty braces") {
    check("val result=input match{case First=>{};case _=>}",
      "val result =\n  input match {\n    case First =>\n    case _ =>\n  }\n")
    check("val result=values.map{case _=>{}}", "val result = values.map{ case _ =>\n}\n")
  }

  test("comment-only bodies and footer comments remain with their case before the next sibling") {
    check("val result=input match{case First=>{// body\n};case _=>other}",
      "val result =\n  input match {\n    case First =>\n      // body\n    case _ => other\n  }\n")
    check("val result=input match{case x=>{use(x)// footer\n};case _=>other}",
      "val result =\n  input match {\n    case x =>\n      use(x)\n      // footer\n    case _ => other\n  }\n")
    val expected = "val result =\n  input match {\n    case First =>\n      value\n      // footer\n      // next case\n    case _ => other\n  }\n"
    check(
      "val result=input match {\n  case First=>{\n    value\n    // footer\n  }\n\n  // next case\n  case _=>other\n}",
      expected,
    )
  }

  test("a nested partial function keeps its own braces so its cases cannot become outer siblings") {
    check("val result=input match{case x=>{{case y=>y}};case _=>other}",
      "val result =\n  input match {\n    case x =>\n      {\n        case y => y\n      }\n    case _ => other\n  }\n")
  }

  test("a nested match keeps its cases separate from following outer cases") {
    check("val result=input match{case First=>{value match{case Inner=>inside;case _=>fallback}};case _=>other}",
      "val result =\n  input match {\n    case First =>\n      value match {\n        case Inner => inside\n        case _ => fallback\n      }\n    case _ => other\n  }\n")
  }

  test("an independent nested block retains its scope inside an unwrapped case body") {
    check("val result=input match{case x=>{{val nested=x;consume(nested)};fallback};case _=>other}",
      "val result =\n  input match {\n    case x =>\n      {\n        val nested = x\n        consume(nested)\n      }\n      fallback\n    case _ => other\n  }\n")
  }
}
