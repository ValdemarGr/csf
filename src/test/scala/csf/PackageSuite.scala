package csf

import munit.FunSuite
import scala.meta.*

class PackageSuite extends FunSuite {
  private given Dialect = dialects.Scala3

  private def structure(value: Any): Any = value match {
    case Term.Block(List(term: Term)) => structure(term)
    case tree: Tree => (tree.productPrefix, tree.productIterator.map(structure).toList)
    case values: List[?] => values.map(structure)
    case value: Option[?] => value.map(structure)
    case other => other
  }

  private def comments(source: String): List[String] = {
    source.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList
  }

  private def check(source: String, expected: String): Unit = {
    val result = Formatter.format(source).fold(error => fail(error.getMessage), identity)
    val reparsed = {
      given Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
      result.parse[Source].get
    }

    assertEquals(result, expected)
    assertEquals(structure(reparsed), structure(source.parse[Source].get))
    assertEquals(comments(result), comments(source))
    assertEquals(Formatter.format(result), Right(result))
  }

  test("ordinary package headers leave their body unindented") {
    check("package example\n\n\n\nimport a.A\nobject Example{val x=1}",
      "package example\n\nimport a.A\nobject Example {\n  val x = 1\n}\n")
  }

  test("redundant braces around a whole-file package chain are removed") {
    check("package first{package second{object Example{}}}",
      "package first\npackage second\nobject Example\n")
  }

  test("sibling packages retain their separate braced scopes") {
    check("package first{object A{}}\n\npackage second{object B{}}",
      "package first {\n  object A\n}\n\npackage second {\n  object B\n}\n")
  }

  test("package siblings of root declarations retain braces") {
    check("package nested{object Inside{}}\n\nobject Outside{}",
      "package nested {\n  object Inside\n}\n\nobject Outside\n")
  }

  test("packages nested inside required braced scopes retain braces") {
    check("package first{package nested{object A{}}}\npackage second{object B{}}",
      "package first {\n  package nested {\n    object A\n  }\n}\npackage second {\n  object B\n}\n")
  }

  test("nested package siblings keep scope beneath an ordinary outer header") {
    check("package outer{package inner{object Inside{}}\n\nobject Outside{}}",
      "package outer\npackage inner {\n  object Inside\n}\n\nobject Outside\n")
  }

  test("empty and comment-only whole-file packages need no braces") {
    check("package empty{}", "package empty\n")
    check("package example{/* body */}", "package example\n/* body */\n")
  }

  test("comments before and after package headers retain contents with canonical gaps") {
    check("// package docs\n\npackage example\n\n/* scope\ndetails */\n\n\nobject Example{}",
      "// package docs\npackage example\n/* scope\ndetails */\nobject Example\n")
  }

  test("inline package comments normalize to standalone lines") {
    check("package example // scope\n\nobject Example{}",
      "package example\n// scope\nobject Example\n")
  }

  test("member comments normalize without inflating the package header gap") {
    check("package example\nobject Example{val first=1\n\n// member docs\nval second=2}",
      "package example\nobject Example {\n  val first = 1\n\n  // member docs\n  val second = 2\n}\n")
  }
}
