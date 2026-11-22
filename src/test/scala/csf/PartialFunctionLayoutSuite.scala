package csf

import munit.FunSuite
import scala.meta.*

class PartialFunctionLayoutSuite extends FunSuite {
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
    assertEquals(structure(reparsed), structure(source.parse[Source].get))
    assertEquals(
      result.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
      source.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList,
    )
    assertEquals(Formatter.format(result), Right(result))
  }

  test("short single-case arguments keep the case header on the call line") {
    check("val result=values.map{case Some(x)=>f(x)}",
      "val result = values.map{ case Some(x) => f(x) }\n")
  }

  test("a use case argument nests its body two spaces and aligns its closing brace with the method") {
    check("val result=resource.acquire.use{case(send,outbox)=>{prepare(send);outbox.persist(send)}}",
      "val result =\n  resource\n    .acquire\n    .use{ case (send, outbox) =>\n" +
        "      prepare(send)\n      outbox.persist(send)\n    }\n")
  }

  test("tuple case headers stay inline above multiline constructor bodies") {
    val attribute = "attribute" + "x" * 85
    check(s"val result=rows.map{case(name,values)=>Attribute($attribute,values.map(render).mkString(\",\"))}",
      "val result =\n  rows.map{ case (name, values) =>\n" +
        s"    Attribute(\n      $attribute,\n      values.map(render).mkString(\",\")\n    )\n  }\n")
  }

  test("short guarded case headers remain on the call line") {
    check("val result=values.collect{case x if p(x)=>f(x)}",
      "val result = values.collect{ case x if p(x) => f(x) }\n")
  }

  test("wide tuple patterns wrap while their short body stays after the arrow") {
    val first = "first" + "Identifier" * 5
    val second = "second" + "Identifier" * 5
    check(s"val result=rows.map{case($first,$second)=>$first}",
      s"val result =\n  rows.map{ case (\n      $first,\n      $second\n    ) => $first\n  }\n")
  }

  test("block comments in a case body occupy standalone lines") {
    check("val result=values.map{case x=> /* body */ f(x)}",
      "val result =\n  values.map{ case x =>\n    /* body */\n    f(x)\n  }\n")
  }

  test("comments before the case retain the ordinary block layout") {
    check("val result=values.map{ // before case\ncase x=>f(x)}",
      "val result =\n  values.map{\n    // before case\n    case x => f(x)\n  }\n")
  }

  test("footer line comments remain inside their original argument delimiters") {
    check("val result=values.map{case x=>f(x) // footer\n}",
      "val result =\n  values.map{ case x =>\n    f(x)\n    // footer\n  }\n")
    check("val result=values.collect({/* leading */case x=>f(x)} // footer\n)",
      "val result = values.collect(\n  {\n    /* leading */\n    case x => f(x)\n  }\n  // footer\n)\n")
  }

  test("explicit multistatement case bodies omit their outer braces") {
    check("val result=values.map{case x=>{prepare(x);f(x)}}",
      "val result =\n  values.map{ case x =>\n    prepare(x)\n    f(x)\n  }\n")
  }

  test("nested and infix calls apply the same single-case argument layout") {
    check("val result=wrap(values.map{case Some(x)=>f(x)})",
      "val result = wrap(values.map{ case Some(x) => f(x) })\n")
    check("val result=values map {case Some(x)=>f(x)}",
      "val result = values map{ case Some(x) => f(x) }\n")
  }

  test("qualified Carleasing patterns remain attached to their call") {
    check("val previousToken=existing.collect{case model.FinancingPartner.ApiIntegration.Carleasing(_,value)=>value}",
      "val previousToken =\n  existing.collect{ case model.FinancingPartner.ApiIntegration.Carleasing(_, value) => value }\n")
    check(
      "object Example{def run=input match{case _=>{val previousToken=existing.collect{case model.FinancingPartner.ApiIntegration.Carleasing(_,value)=>value};previousToken}}}",
      "object Example {\n  def run =\n    input match {\n      case _ =>\n        val previousToken =\n          existing.collect{ case model.FinancingPartner.ApiIntegration.Carleasing(_, value) => value\n          }\n        previousToken\n    }\n}\n",
    )
  }

  test("a whole case fitting exactly 100 columns remains inline") {
    val name = "p" * (100 - "val result = values.map{ case Some() => v }".length)
    val expected = s"val result = values.map{ case Some($name) => v }\n"

    assertEquals(expected.linesIterator.next().length, 100)
    check(s"val result=values.map{case Some($name)=>v}", expected)
  }

  test("a 101-column whole case moves below equals with its header still attached") {
    val name = "p" * (101 - "val result = values.map{ case Some() => v }".length)

    assertEquals(s"val result = values.map{ case Some($name) => v }".length, 101)
    check(s"val result=values.map{case Some($name)=>v}",
      s"val result =\n  values.map{ case Some($name) => v }\n")
  }

  test("an attached extractor header fitting exactly 100 columns keeps its arguments inline") {
    val name = "p" * (100 - "  values.map{ case Qualified.Pair(, second) =>".length)
    val header = s"  values.map{ case Qualified.Pair($name, second) =>"

    assertEquals(header.length, 100)
    check(s"val result=values.map{case Qualified.Pair($name,second)=>value}",
      s"val result =\n$header\n    value\n  }\n")
  }

  test("a 101-column attached extractor header wraps arguments without moving case off the call line") {
    val name = "p" * (101 - "  values.map{ case Qualified.Pair(, second) =>".length)

    assertEquals(s"  values.map{ case Qualified.Pair($name, second) =>".length, 101)
    check(s"val result=values.map{case Qualified.Pair($name,second)=>value}",
      s"val result =\n  values.map{ case Qualified.Pair(\n      $name,\n      second\n    ) => value\n  }\n")
  }

  test("guards remain attached and their own argument clauses wrap when necessary") {
    check("val result=values.collect{case Some(value) if allowed(value)=>value}",
      "val result = values.collect{ case Some(value) if allowed(value) => value }\n")
    check("val carleasingGraphQLResult=valuesForCarleasingResultsWithMetadata.collect{case Qualified.Carleasing(_,value) if allowed(value)=>value}",
      "val carleasingGraphQLResult =\n  valuesForCarleasingResultsWithMetadata.collect{ case Qualified.Carleasing(_, value) if allowed(\n      value\n    ) => value\n  }\n")
  }

  test("multistatement bodies nest two spaces from their call independently of wrapped extractor arguments") {
    val name = "p" * (101 - "  values.map{ case Qualified.Pair(, second) =>".length)

    check("val result=values.map{case Some(value)=>{prepare(value);use(value)}}",
      "val result =\n  values.map{ case Some(value) =>\n    prepare(value)\n    use(value)\n  }\n")
    check(s"val result=values.map{case Qualified.Pair($name,second)=>{prepare(value);use(value)}}",
      s"val result =\n  values.map{ case Qualified.Pair(\n      $name,\n      second\n    ) =>\n    prepare(value)\n    use(value)\n  }\n")
  }

  test("leading and footer comments retain case and body indentation") {
    check("val result=values.collect{// before\ncase Qualified.Carleasing(_,value)=>{use(value)// footer\n}}",
      "val result =\n  values.collect{\n    // before\n    case Qualified.Carleasing(_, value) =>\n      use(value)\n      // footer\n  }\n")
  }

  test("multiple-case argument blocks retain separate case lines") {
    check("val result=values.collect{case Some(x)=>f(x);case _=>fallback}",
      "val result =\n  values.collect{\n    case Some(x) => f(x)\n    case _ => fallback\n  }\n")
  }

  test("standalone single-case partial functions retain their block layout") {
    check("val f={case Some(x)=>f(x)}", "val f = {\n  case Some(x) => f(x)\n}\n")
  }
}
