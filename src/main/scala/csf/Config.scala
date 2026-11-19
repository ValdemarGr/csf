package csf

import cats.effect.IO
import java.io.IOException
import java.nio.file.{NoSuchFileException, Path}
import org.tomlj.{Toml, TomlParseResult}
import scala.jdk.CollectionConverters.*

private[csf] object Config {
  private val path = Path.of("csf.toml")

  def load: IO[FormatOptions] =
    IO.blocking(Toml.parse(path)).flatMap { result =>
      IO.fromEither(decode(result).left.map(message => new IllegalArgumentException(message)))
    }.handleErrorWith {
      case _: NoSuchFileException => IO.pure(FormatOptions())
      case error => IO.raiseError(new IOException(s"$path: ${error.getMessage}", error))
    }

  private def decode(result: TomlParseResult): Either[String, FormatOptions] = {
    val defaults = FormatOptions()
    val errors = result.errors().asScala
    val unknown = result.keySet().asScala.iterator.filterNot(Set("columns", "control-syntax")).toList.sorted
    for {
      _ <- Either.cond(errors.isEmpty, (), errors.mkString("\n"))
      _ <- Either.cond(unknown.isEmpty, (), s"Unknown options: ${unknown.mkString(", ")}")
      columns <- result.get("columns") match {
        case null => Right(defaults.columns)
        case width: java.lang.Long if width.longValue() > 0 && width.longValue() <= Int.MaxValue =>
          Right(width.intValue())
        case _ => Left(s"columns must be an integer between 1 and ${Int.MaxValue}.")
      }
      controlSyntax <- result.get("control-syntax") match {
        case null => Right(defaults.controlSyntax)
        case "preserve" => Right(ControlSyntax.Preserve)
        case "new" => Right(ControlSyntax.New)
        case _ => Left("control-syntax must be \"preserve\" or \"new\".")
      }
    } yield FormatOptions(columns, controlSyntax)
  }
}
