package csf

import munit.FunSuite
import scala.meta.*

class NormalizationSuite extends FunSuite {
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

  test("nested singleton blocks and method bodies converge to their expression layout") {
    List(
      "val result={{{value}}}" -> "val result=value",
      "def identity[A](value:A):A={{{value}}}" -> "def identity[A](value:A):A=value",
    ).foreach { case (blocked, direct) =>
      assertEquals(format(blocked), format(direct))
    }
  }

  test("singleton lambda bodies converge while argument lambda delimiters stay required") {
    val blocked = "val result=values.map(value=>{{{use(value)}}})"
    val direct = "val result=values.map(value=>use(value))"

    assertEquals(format(blocked), format(direct))
  }

  test("yield and case bodies normalize nested singleton expression blocks") {
    List(
      "val result=for(value<-values)yield{{use(value)}}" ->
        "val result=for(value<-values)yield use(value)",
      "val result=input match{case Some(value)=>{{use(value)}};case _=>other}" ->
        "val result=input match{case Some(value)=>use(value);case _=>other}",
      "val result=values.map{case Some(value)=>{{use(value)}}}" ->
        "val result=values.map{case Some(value)=>use(value)}",
    ).foreach { case (blocked, direct) =>
      assertEquals(format(blocked), format(direct))
    }
  }

  test("call arguments normalize independently without changing their arity") {
    assertEquals(format("val result=combine({{{first}}},{{second}})"),
      format("val result=combine(first,second)"))
    assertEquals(format("val result=call({(first,second)})"),
      format("val result=call((first,second))"))
    assertEquals(format("val result=call({()})"), format("val result=call(())"))
  }

  test("singleton qualifier blocks and chained receivers share their direct expression layout") {
    List(
      "val result=({{value}}).name" -> "val result=value.name",
      "val result=({{service.first()}}).second().third()" ->
        "val result=service.first().second().third()",
    ).foreach { case (blocked, direct) =>
      assertEquals(format(blocked), format(direct))
    }
    val receiver = "receiver" + "x" * 65
    assertEquals(format(s"val result=({{$receiver.first()}}).second().third()"),
      format(s"val result=$receiver.first().second().third()"))
    List(
      "val result=({service.first() /* footer */}).second().third()",
      "val result=({service.first() // footer\n}).second().third()",
      "val result=({a.b // footer\n})(value)",
      "val result=({a.b // footer\n})[T](value)",
    ).foreach(format)
  }

  test("removing expression braces preserves precedence associativity and signed receivers") {
    List(
      "val result=({{left+right}})*value" -> "val result=(left+right)*value",
      "val result=left - {{right-last}}" -> "val result=left-(right-last)",
      "val result={{head::tail}}::rest" -> "val result=(head::tail)::rest",
      "val result=({{-1}}).abs" -> "val result=(-1).abs",
      "val result=({{-0.0}}).toString" -> "val result=(-0.0).toString",
      "val result=left op {(first,second)}" -> "val result=left op ((first,second))",
      "val result=left op {()}" -> "val result=left op (())",
    ).foreach { case (blocked, grouped) =>
      assertEquals(format(blocked), format(grouped))
    }
  }

  test("singleton control branches converge from braced and direct syntax") {
    List(
      "val result=if(ready){use(value)}else{fallback}" ->
        "val result=if(ready)use(value)else fallback",
      "val result=while(ready){advance()}" -> "val result=while(ready)advance()",
      "val result=for(value<-values){use(value)}" -> "val result=for(value<-values)use(value)",
      "val result=try{action()}catch{case error:Error=>{recover(error)}}finally{cleanup()}" ->
        "val result=try action()catch{case error:Error=>recover(error)}finally cleanup()",
    ).foreach { case (blocked, direct) =>
      assertEquals(format(blocked), format(direct))
    }
  }

  test("nested conditionals retain their outer else ownership through control bodies") {
    List(
      "val result=if(outer){if(inner)left}else right",
      "val result=if(outer){if(inner)left else if(next)right}else fallback",
      "val result=if(outer){while(ready){if(inner)left}}else right",
      "val result=if(outer){for(value<-values){if(inner)left}}else right",
    ).foreach(format)
  }

  test("nested try expressions retain ownership of their distinct catch clauses") {
    val blocked = "val result=try{try{action()}catch{case inner:Inner=>{recover(inner)}}}catch{case outer:Outer=>{recover(outer)}}"
    val direct = "val result=try{try action()catch{case inner:Inner=>recover(inner)}}catch{case outer:Outer=>recover(outer)}"

    assertEquals(format(blocked), format(direct))
    List(
      "val result=try{if(ready){try action()}}catch{case error=>recover(error)}",
      "val result=try action()catch{try handler}finally cleanup()",
      "val result=try{try action()catch handler}finally cleanup()",
    ).foreach(format)
  }

  test("leading and footer comments retain their order when singleton blocks are removed") {
    List(
      "def value={/* before */source}",
      "def value={source // footer\n}",
      "val result=call({/* before */value /* footer */})",
    ).foreach(format)
  }

  test("empty and multistatement method blocks remain blocks") {
    List("def idle={}", "def run={val value=prepare();use(value)}").foreach { source =>
      val result = format(source)
      val definition = result.parse[Source].get.stats.head.asInstanceOf[Defn.Def]
      assert(definition.body.isInstanceOf[Term.Block], result)
    }
    assertEquals(format("def idle={{{}}}"), format("def idle={}"))
    assertEquals(format("def run={{{val value=prepare();use(value)}}}"),
      format("def run={val value=prepare();use(value)}"))
  }

  test("a scoped standalone block cannot become an argument to its preceding statement") {
    val result = format("object Example{call();{val scoped=prepare();use(scoped)};next()}")

    assert(result.contains("call();\n"), result)
    assertEquals(format("object Example{call();{(first,second)};next()}"),
      format("object Example{call();(first,second);next()}"))
    assertEquals(format("object Example{call();{-value};next()}"),
      format("object Example{call();-value;next()}"))
    format("object Example{call();{value // footer\n};(first,second)}")
  }

  test("quoted expressions and splices retain their syntax-required delimiters") {
    val quote = format("val quoted='{value}")
    val splice = format("val quoted='{${value}}")

    assert(quote.contains("'{"), quote)
    assert(splice.contains("'{"), splice)
    assert(splice.contains("${"), splice)
    format("val quotedType='[List[Int]]")
    assertEquals(format("val result = <node>{ { {value} } }</node>"),
      format("val result = <node>{value}</node>"))
  }
}
