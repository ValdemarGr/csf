package csf

import cats.effect.IO
import java.io.IOException
import java.nio.file.{FileAlreadyExistsException, Files, NoSuchFileException, Path, StandardOpenOption}
import org.tomlj.{Toml, TomlParseResult}
import scala.jdk.CollectionConverters.*

private[csf] object Config {
  private val path = Path.of("csf.toml")
  private val defaults = FormatOptions()
  private val syntaxNames = Map(ControlSyntax.New -> "new", ControlSyntax.Old -> "old")

  def init: IO[Unit] = IO.blocking {
    val contents = s"columns = ${defaults.columns}\ncontrol-syntax = \"${syntaxNames(defaults.controlSyntax)}\"\n"
    Files.writeString(path, contents, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
  }.void.adaptError {
    case error: FileAlreadyExistsException => new IOException(s"$path already exists.", error)
  }

  def load: IO[FormatOptions] =
    IO.blocking(Toml.parse(path)).flatMap { result =>
      IO.fromEither(decode(result).left.map(message => new IllegalArgumentException(message)))
    }.handleErrorWith {
      case _: NoSuchFileException => IO.pure(defaults)
      case error => IO.raiseError(new IOException(s"$path: ${error.getMessage}", error))
    }

  private def decode(result: TomlParseResult): Either[String, FormatOptions] = {
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
        case value => syntaxNames.collectFirst { case (syntax, name) if name == value => syntax }
          .toRight("control-syntax must be \"new\" or \"old\".")
      }
    } yield FormatOptions(columns, controlSyntax)
  }
}
