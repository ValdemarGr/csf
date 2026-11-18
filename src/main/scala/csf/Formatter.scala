package csf

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.{CopyFlag, CopyFlags, Files, Path}
import scala.meta.*
import scala.meta.parsers.{Parse, Parsed, ParserOptions}

enum FormatError(message: String) extends RuntimeException(message) {
  case Parse(details: String) extends FormatError(details)
  case Unsupported(node: String) extends FormatError(s"Unsupported syntax: $node")
}

object Formatter {
  private given Dialect = dialects.Scala3
  private given ParserOptions = ParserOptions.default.withCaptureComments(false)

  def format(source: String, columns: Int = 100): Either[FormatError, String] = {
    Either.catchNonFatal(Parse.parseSource(Input.String(source)))
      .leftMap(error => FormatError.Parse(error.toString)).flatMap {
      case Parsed.Success(tree) =>
        Printer.print(tree).map { doc =>
          val rendered = doc.render(columns)
          if (rendered.isEmpty) "" else rendered.replaceAll("\n+$", "") + "\n"
        }
      case error: Parsed.Error => Left(FormatError.Parse(error.toString))
    }
  }

  def formatFile(path: Path, columns: Int = 100): IO[Unit] = {
    processFile(path, write = true, columns = columns).void
  }

  def checkFile(path: Path, columns: Int = 100): IO[Boolean] = {
    processFile(path, write = false, columns = columns)
  }

  private def processFile(path: Path, write: Boolean, columns: Int): IO[Boolean] = {
    val files = Files[IO]
    for {
      target <- files.realPath(path)
      source <- files.readUtf8(target).compile.string
      formatted <- IO.delay(format(source, columns)).flatMap(IO.fromEither)
      unchanged = formatted == source
      _ <- if (unchanged || !write) IO.unit
      else {
        for {
          patched <- IO.delay(TextDiff(source, formatted))
          permissions <- files.getPosixPermissions(target)
          _ <- files.tempFile(target.parent, ".csf-", ".tmp", None).use { temporary =>
            Stream.emit(patched).through(files.writeUtf8(temporary)).compile.drain *>
              files.setPosixPermissions(temporary, permissions) *>
              files.move(temporary, target, CopyFlags(CopyFlag.AtomicMove, CopyFlag.ReplaceExisting))
          }
        } yield ()
      }
    } yield unchanged
  }
}
