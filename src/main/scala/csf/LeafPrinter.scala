package csf

import scala.meta.*
import scala.meta.internal.prettyprinters.{DoubleQuotes, SingleQuotes}
import scala.meta.trees.Origin

private[csf] object LeafPrinter {
  private given Dialect = dialects.Scala3
  private val keywords = scala.meta.internal.tokenizers.keywords(dialects.Scala3)

  def name(value: Name): String = value match {
    case _: Name.Anonymous | _: Term.Anonymous => ""
    case _: Name.This => "this"
    case _: Name.Placeholder => "_"
    case _ =>
      val suffix = value match {
        case _: Term.CapSetName | _: Type.CapSetName => "^"
        case _ => ""
      }
      value.origin match {
        case parsed: Origin.ParsedPartial =>
          val tokens = parsed.allInputTokens()
          var index = parsed.begTokenIdx
          while (index < parsed.endTokenIdx) {
            tokens(index) match {
              case token: Token.Ident =>
                val spelling = if (token.value == value.value) token.text
                else if (token.text.startsWith("`")) "`" + value.value + "`"
                else generatedName(value.value)
                return spelling + suffix
              case token: Token.Interpolation.Id => return token.text + suffix
              case _ => ()
            }
            index += 1
          }
        case _ => ()
      }
      generatedName(value.value) + suffix
  }

  private def generatedName(value: String): String = {
    val plain = !keywords(value) && (value.tokenize match {
      case Tokenized.Success(tokens) =>
        tokens.length == 3 && tokens(1).isInstanceOf[Token.Ident] && tokens(1).text == value
      case _: Tokenized.Error => false
    })
    if (plain) value else "`" + value + "`"
  }

  private def quoted(value: Lit, fallback: => String): String = {
    value.origin match {
      case parsed: Origin.ParsedPartial =>
        val tokens = parsed.allInputTokens()
        var index = parsed.begTokenIdx
        while (index < parsed.endTokenIdx) {
          tokens(index) match {
            case token: Token.Constant.String => return token.text
            case token: Token.Constant.Char => return token.text
            case _ => ()
          }
          index += 1
        }
      case _ => ()
    }
    fallback
  }

  private def radixLiteral(value: Lit): Option[String] = {
    value.origin match {
      case parsed: Origin.ParsedPartial =>
        val tokens = parsed.allInputTokens()
        var index = parsed.begTokenIdx
        while (index < parsed.endTokenIdx) {
          tokens(index) match {
            case token: Token.NumericConstant[?] =>
              val text = token.text
              return Option.when(text.length >= 2 && text(0) == '0' && "xXbB".contains(text(1))) {
                if (parsedMinus(value)) "-" + text else text
              }
            case _ => ()
          }
          index += 1
        }
      case _ => ()
    }
    None
  }

  def literal(value: Lit): String = value match {
    case literal: Lit.Int => radixLiteral(literal).getOrElse(literal.value.toString)
    case literal: Lit.Long => radixLiteral(literal).getOrElse(literal.value.toString + "L")
    case literal: Lit.Float =>
      val format = if (isNegative(literal) && !literal.format.startsWith("-")) "-" + literal.format
      else literal.format
      if (format.last.toLower == 'f') format else format + "f"
    case literal: Lit.Double =>
      (if (isNegative(literal) && !literal.format.startsWith("-")) "-" else "") + literal.format + "d"
    case literal: Lit.IntXL => radixLiteral(literal).getOrElse(literal.value.toString)
    case literal: Lit.FloatXL =>
      literal.digits + (if (literal.exponent.isEmpty) "" else "e" + literal.exponent)
    case literal: Lit.WithUnary => name(literal.op) + this.literal(literal.arg)
    case literal: Lit.Byte =>
      "ByteLiterals." + (if (literal.value == 0) "Zero"
                        else if (literal.value > 0) "Plus" + literal.value
                        else "Minus" + literal.value)
    case literal: Lit.Short =>
      "ShortLiterals." + (if (literal.value == 0) "Zero"
                         else if (literal.value > 0) "Plus" + literal.value
                         else "Minus" + literal.value)
    case literal: Lit.Boolean => literal.value.toString
    case literal: Lit.String => quoted(literal, DoubleQuotes.orTriple(literal.value))
    case literal: Lit.Char => quoted(literal, SingleQuotes(literal.value))
    case literal: Lit.Symbol => "'" + literal.value.name
    case _: Lit.Null => "null"
    case _: Lit.Unit => "()"
  }

  def isNegative(value: Lit): Boolean = value match {
    case literal: Lit.Int => radixLiteral(literal).fold(literal.value < 0)(_.startsWith("-"))
    case literal: Lit.Long => radixLiteral(literal).fold(literal.value < 0L)(_.startsWith("-"))
    case literal: Lit.Float => literal.format.startsWith("-") ||
      literal.value == 0.0f && parsedMinus(literal)
    case literal: Lit.Double => literal.format.startsWith("-") ||
      literal.value == 0.0d && parsedMinus(literal)
    case literal: Lit.IntXL => radixLiteral(literal).fold(literal.value.toString.startsWith("-"))(_.startsWith("-"))
    case literal: Lit.FloatXL => literal.digits.startsWith("-")
    case literal: Lit.WithUnary => literal.op.value.startsWith("-")
    case _ => false
  }

  private def parsedMinus(value: Lit): Boolean = {
    value.origin match {
      case parsed: Origin.ParsedPartial =>
        val tokens = parsed.allInputTokens()
        var index = parsed.begTokenIdx
        while (index < parsed.endTokenIdx) {
          tokens(index) match {
            case token: Token.Ident => return token.value == "-"
            case _: Token.NumericConstant[?] => return false
            case _ => ()
          }
          index += 1
        }
      case _ => ()
    }
    false
  }
}
