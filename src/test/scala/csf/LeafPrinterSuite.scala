package csf

import munit.FunSuite
import scala.meta.*

class LeafPrinterSuite extends FunSuite {
  private given Dialect = dialects.Scala3

  private def parsedLiteral(source: String): Lit = source.parse[Term].get match {
    case literal: Lit => literal
    case other => fail(s"Expected literal: ${other.productPrefix}")
  }

  test("parsed names preserve backticks and Unicode spelling") {
    List("plain", "`type`", "`café`", "`name with spaces`", "+").foreach { source =>
      val name = source.parse[Term].get.asInstanceOf[Term.Name]
      assertEquals(LeafPrinter.name(name), source)
    }
  }

  test("generated names use valid Scala identifiers and quote keywords") {
    List("plain", "café", "𝛂", "+", "value_+").foreach { value =>
      assertEquals(LeafPrinter.name(Term.Name(value)), value)
    }
    assertEquals(LeafPrinter.name(Term.Name("type")), "`type`")
    assertEquals(LeafPrinter.name(Term.Name("two words")), "`two words`")
    assertEquals(LeafPrinter.name(Name.Anonymous()), "")
    assertEquals(LeafPrinter.name(Name.This()), "this")
    assertEquals(LeafPrinter.name(Name.Placeholder()), "_")
    assertEquals(LeafPrinter.name(Term.Anonymous()), "")
    assertEquals(LeafPrinter.name(Term.CapSetName("caps")), "caps^")
    assertEquals(LeafPrinter.name(Type.CapSetName("caps")), "caps^")
  }

  test("projected splice names exclude the macro dollar prefix") {
    val quotation = "'{ $expression }".parse[Term].get
    val name = quotation.collect {
      case name: Term.Name if name.value == "expression" => name
    }.head
    assertEquals(LeafPrinter.name(name), "expression")
  }

  test("strings and chars use their literal token without surrounding parentheses") {
    val examples = List(
      "(\"hello\\nworld\")" -> "\"hello\\nworld\"",
      "(\"\"\"first\n  second\n\nthird\"\"\")" -> "\"\"\"first\n  second\n\nthird\"\"\"",
      "('\\n')" -> "'\\n'",
      "('λ')" -> "'λ'",
    )
    examples.foreach { case (source, expected) =>
      val original = parsedLiteral(source)
      val result = LeafPrinter.literal(original)
      assertEquals(result, expected)
      assertEquals(parsedLiteral(result).value, original.value)
    }
  }

  test("generated string and character literals use library quoting") {
    val text = "first\n  second\n"
    val rendered = LeafPrinter.literal(Lit.String(text))
    assertEquals(parsedLiteral(rendered).value, text)
    assertEquals(LeafPrinter.literal(Lit.Char('\n')), "'\\n'")
  }

  test("numeric emission retains established canonical spellings") {
    val examples = List(
      "0xFF" -> "255",
      "+1" -> "1",
      "-12" -> "-12",
      "1_000L" -> "1000L",
      "1.25F" -> "1.25f",
      "1.2500D" -> "1.25d",
      "1e3" -> "1E+3d",
      "true" -> "true",
      "null" -> "null",
      "()" -> "()",
    )
    examples.foreach { case (source, expected) =>
      assertEquals(LeafPrinter.literal(parsedLiteral(source)), expected)
    }
  }

  test("negative floating zero retains lexical sign and grouping requirement") {
    List("-0.0", "-0.0D", "-0.0F").foreach { source =>
      val literal = parsedLiteral(source)
      val result = LeafPrinter.literal(literal)
      assert(result.startsWith("-0.0"), result)
      assert(LeafPrinter.isNegative(literal), source)
      val parsedNumber = result.last match {
        case 'f' => java.lang.Float.floatToRawIntBits(java.lang.Float.parseFloat(result)).toLong
        case _ => java.lang.Double.doubleToRawLongBits(java.lang.Double.parseDouble(result))
      }
      assert(parsedNumber != 0L, result)
    }
    assert(!LeafPrinter.isNegative(parsedLiteral("0.0")))
    assert(!LeafPrinter.isNegative(parsedLiteral("\"-text\"")))
    assert(LeafPrinter.isNegative(Lit.Double(-0.0d)))
    assert(LeafPrinter.isNegative(Lit.Float(-0.0f)))
  }

  test("formatting retains negative floating zero through leaf emission") {
    val result = Formatter.format("object X { val value = -0.0D; val text = (-0.0F).toString }")
      .fold(error => fail(error.getMessage), identity)
    assert(result.contains("val value = -0.0d"), result)
    assert(result.contains("(-0.0f).toString"), result)
    assertEquals(Formatter.format(result), Right(result))
  }

  test("generic numeric literals and unary wrappers emit without composite printing") {
    val integer = Lit.IntXL(BigInt("123456789012345678901234567890"))
    assertEquals(LeafPrinter.literal(integer), "123456789012345678901234567890")
    val decimal = Lit.FloatXL("12345", "-10")
    assertEquals(LeafPrinter.literal(decimal), "12345e-10")
    assertEquals(LeafPrinter.literal(Lit.WithUnary(Term.Name("-"), integer)), "-123456789012345678901234567890")
    assert(LeafPrinter.isNegative(Lit.WithUnary(Term.Name("-"), integer)))
  }
}
