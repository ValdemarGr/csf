package csf

import munit.FunSuite
import scala.meta.*

class CaseArgumentWrappingSuite extends FunSuite {
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
    assert(result.linesIterator.forall(_.length <= 100), result)
  }

  test("qualified Carleasing patterns remain attached to their call") {
    check("val previousToken=existing.collect{case model.FinancingPartner.ApiIntegration.Carleasing(_,value)=>value}",
      "val previousToken =\n  existing.collect{ case model.FinancingPartner.ApiIntegration.Carleasing(_, value) => value }\n")
    check(
      "object Example{def run=input match{case _=>{val previousToken=existing.collect{case model.FinancingPartner.ApiIntegration.Carleasing(_,value)=>value};previousToken}}}",
      "object Example {\n  def run =\n    input match {\n      case _ =>\n        val previousToken =\n          existing.collect{ case model.FinancingPartner.ApiIntegration.Carleasing(_, value) => value\n          }\n        previousToken\n    }\n}\n",
    )
  }

  test("a short whole case remains inline") {
    check("val result=values.map{case Some(value)=>use(value)}",
      "val result = values.map{ case Some(value) => use(value) }\n")
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
}
