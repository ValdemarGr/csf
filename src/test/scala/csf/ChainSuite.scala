package csf

import munit.FunSuite
import scala.meta.*

class ChainSuite extends FunSuite {
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
    result
  }

  test("short method parameterless and nested chains collapse to one line") {
    assertEquals(format("val result=xs.map(f).filter(p).toList"),
      "val result = xs.map(f).filter(p).toList\n")
    assertEquals(format("val result=stream\n  .compile\n  .drain"),
      "val result = stream.compile.drain\n")
    assertEquals(format("val result=consume(left.first().second()).third().fourth()"),
      "val result = consume(left.first().second()).third().fourth()\n")
    assertEquals(format("val result=value.map(f)"), "val result = value.map(f)\n")
  }

  test("a chain fitting exactly 100 columns including its line prefix stays inline") {
    val receiver = "r" * (100 - "val result = .first().second()".length)
    val expected = s"val result = $receiver.first().second()\n"

    assertEquals(expected.linesIterator.next().length, 100)
    assertEquals(format(s"val result=$receiver.first().second()"), expected)
  }

  test("chains move below equals before splitting their continuation dots") {
    val receiver = "r" * (101 - "val result = .first().second()".length)
    assertEquals(s"val result = $receiver.first().second()".length, 101)
    assertEquals(format(s"val result=$receiver.first().second()"),
      s"val result =\n  $receiver.first().second()\n")

    val overflowingReceiver = "r" * (101 - "  .first().second()".length)
    assertEquals(s"  $overflowingReceiver.first().second()".length, 101)
    assertEquals(format(s"val result=$overflowingReceiver.first().second()"),
      s"val result =\n  $overflowingReceiver\n    .first()\n    .second()\n")
  }

  test("a terminal braced lambda moves below equals before splitting the chain dots") {
    val source = "val x=a.b.d.e.f{y=>work(y);y}"
    val inline = "val x = a.b.d.e.f{ y =>\n  work(y)\n  y\n}\n"
    val belowEquals = "val x =\n  a.b.d.e.f{ y =>\n    work(y)\n    y\n  }\n"
    val dotted = "val x =\n  a\n    .b\n    .d\n    .e\n    .f{ y =>\n      work(y)\n      y\n    }\n"
    val inlineWidth = inline.linesIterator.next().length
    val belowEqualsWidth = belowEquals.linesIterator.drop(1).next().length

    assertEquals(format(source, columns = inlineWidth), inline)
    assertEquals(format(source, columns = belowEqualsWidth), belowEquals)
    assertEquals(format(source, columns = belowEqualsWidth - 1), dotted)
    assertEquals(format("val x=a.b[Int](z).d.e.f{y=>work(y);y}"),
      "val x = a.b[Int](z).d.e.f{ y =>\n  work(y)\n  y\n}\n")
  }

  test("overflowing parameterless chains use one shared continuation indent") {
    val receiver = "r" * 86
    assertEquals(format(s"val result=$receiver.compile.drain"),
      s"val result =\n  $receiver\n    .compile\n    .drain\n")
  }

  test("overflowing generic and curried chains keep arguments attached to their step") {
    val firstType = "LongType" * 5
    val secondType = "OtherType" * 4
    assertEquals(format(s"val result=service.load[$firstType](id)(ctx).map[$secondType](f).value"),
      s"val result =\n  service\n    .load[$firstType](id)(ctx)\n    .map[$secondType](f)\n    .value\n")
  }

  test("mandatory multiline lambda arguments split every dot even when their footer would fit 100 columns") {
    val first = "a" * 50
    val second = "b" * 39
    val expected = "val result =\n  values\n    .map(\n      x =>\n        {\n          prepare(x)\n          x\n        }\n" +
      s"    )\n    .$first()\n    .$second()\n"

    assertEquals(s"    ).$first().$second()".length, 100)
    assertEquals(format(s"val result=values.map(x=>{prepare(x);x}).$first().$second()"), expected)
  }

  test("mandatory multiline block arguments split every dot even when their footer would fit") {
    val first = "a" * 50
    val second = "b" * 39
    val expected = "val result =\n  service\n    .run(\n      {\n        prepare()\n        value\n      }\n    )\n" +
      s"    .$first()\n    .$second()\n"

    assertEquals(s"    ).$first().$second()".length, 100)
    assertEquals(format(s"val result=service.run({prepare();value}).$first().$second()"), expected)
  }

  test("lambda body calls wrapping at soft argument breaks split the enclosing chain dots") {
    val first = "first" + "Argument" * 5
    val second = "second" + "Argument" * 5
    val result = format(s"val result=values.map(x=>combine($first,$second)).filter(p)")

    assert(result.startsWith("val result =\n  values\n    .map(\n      x =>\n"), result)
    assert(result.contains("combine(\n"), result)
    assert(result.endsWith("\n    )\n    .filter(p)\n"), result)
    assert(result.linesIterator.forall(_.length <= 100), result)
  }

  test("an enclosing infix expression preserves width limits after multiline chain arguments") {
    val first = "a" * 50
    val second = "b" * 40
    val result = format(s"val result=left + service.run({prepare();value}).$first().$second()")

    assert(result.linesIterator.forall(_.length <= 100), result)
  }

  test("between-step comments retain their order and aligned continuation dots") {
    val result = format("val result=value.first() // first\n.second() /* second */ .third()")

    assert(result.contains("\n    .second()"), result)
    assert(result.contains("\n    .third()"), result)
  }

  test("long package import type and stable pattern paths remain unsplit") {
    val qualifier = "namespace" + "x" * 80
    val path = s"$qualifier.deep.inner"
    val source = s"package $path\nimport $path.Token\n" +
      s"object Example{val value:$path.Token=input;val result=input match{" +
      s"case $path.Token=>first;case Some($path.Token)=>second;case _=>other}}"
    val result = format(source)

    assert(result.startsWith(s"package $path\nimport $path.Token\n"), result)
    assert(result.contains(s"val value: $path.Token =\n    input"), result)
    assert(result.contains(s"case $path.Token =>"), result)
    val extracted = result.parse[Source].get.collect {
      case pattern: Pat.Extract => pattern.argClause.values.head.pos.text
    }
    assertEquals(extracted, List(s"$path.Token"))
  }

  test("short grouped receivers and expression qualifications remain inline") {
    assertEquals(format("val result=(left+right).normalize.result"),
      "val result = (left + right).normalize.result\n")
    assertEquals(format("val result=java.time.Instant.now().toString"),
      "val result = java.time.Instant.now().toString\n")
  }

  test("the Sales mail-content call remains entirely inline when it fits") {
    val source = "val result=TemplatingDB.getMailGenContentHxl(o,id).flatMapF(R.fromOption(UpsertError.MailContentNotFound(id))(_))"

    assertEquals(format(source, columns = 200),
      "val result = TemplatingDB.getMailGenContentHxl(o, id).flatMapF(R.fromOption(UpsertError.MailContentNotFound(id))(_))\n")
  }

  test("the wrapped Sales mail-content call aligns its closing parenthesis with the method step") {
    val expression = "TemplatingDB.getMailGenContentHxl(o,id).flatMapF(R.fromOption(UpsertError.MailContentNotFound(id))(_))"
    val source = "object A{" * 7 + expression + "}" * 7
    val opening = (0 until 7).map(depth => "  " * depth + "object A {").mkString("\n") + "\n"
    val closing = (0 until 7).reverse.map(depth => "  " * depth + "}").mkString("\n") + "\n"
    val expected = opening +
      "              TemplatingDB\n" +
      "                .getMailGenContentHxl(o, id)\n" +
      "                .flatMapF(\n" +
      "                  R.fromOption(UpsertError.MailContentNotFound(id))(_)\n" +
      "                )\n" + closing
    val result = format(source, columns = 78)

    assertEquals(result, expected)
    assert(result.linesIterator.forall(_.length <= 78), result)
  }
}
