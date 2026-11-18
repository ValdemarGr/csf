package csf

import cats.effect.{ExitCode, IO, Ref}
import cats.effect.std.Console
import cats.syntax.all.*
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import fs2.Stream
import fs2.io.file.Path

object Main extends CommandIOApp(
  name = "csf",
  header = "Normalize Scala 3 source files.",
) {
  private val write = (
    Opts.flag("check", "Check formatting without changing files.").as(false) orElse
      Opts.flag("write", "Apply formatting changes to files.").as(true)
  ).withDefault(false)

  private val columns = Opts.option[Int](
    "columns",
    "Set output line width (default: 100).",
    metavar = "N",
  ).validate("Columns must be greater than zero.")(_ > 0).withDefault(100)

  private val paths = Opts.arguments[java.nio.file.Path]("path").map(_.map(Path.fromNioPath))

  def main: Opts[IO[ExitCode]] = (write, columns, paths).mapN { (write, columns, paths) =>
    for {
      discovered <- Stream.emits(paths.toList).flatMap(FileDiscovery.files).compile.toVector
      (errors, files) = discovered.partitionMap(identity)
      _ <- errors.traverse_(error => Console[IO].errorln(error.toString))
      interactive <- IO(System.console() != null)
      parallelism <- IO(Runtime.getRuntime.availableProcessors().min(8).max(1))
      _ <- if (interactive && files.nonEmpty) Console[IO].print("\n\n\u001b[2A") else IO.unit
      results <- Stream.eval(Ref.of[IO, Int](0)).flatMap { index =>
        val nextPath = index.getAndUpdate(_ + 1).map(files.lift)
        val worker = Stream.repeatEval(nextPath).unNoneTerminate.evalMap { path =>
          process(path, write, columns).attempt.map(path -> _)
        }
        Stream.emits(List.fill(parallelism)(worker)).covary[IO].parJoin(parallelism)
      }.zipWithIndex.evalMap { case (result @ (path, _), index) =>
        val update = if (interactive) progress(index.toInt + 1, files.size, path) else IO.unit
        update.as(result)
      }.compile.toVector.guarantee(if (interactive && files.nonEmpty) Console[IO].println("") else IO.unit)
      _ <- if (interactive && files.nonEmpty) IO.unit
      else Console[IO].println(s"(${files.size}/${files.size})") *>
        results.lastOption.traverse_(result => Console[IO].println(result._1))
      _ <- results.traverse_ {
        case (path, Left(error)) => Console[IO].errorln(s"$path: ${error.getMessage}")
        case _ => IO.unit
      }
    } yield ExitCode(results.foldLeft(if (errors.isEmpty) 0 else 2) {
      case (status, (_, Right(code))) => status.max(code)
      case (_, (_, Left(_))) => 2
    })
  }

  private def process(path: Path, write: Boolean, columns: Int): IO[Int] = {
    if (write) Formatter.formatFile(path, columns).as(0)
    else Formatter.checkFile(path, columns).map(unchanged => if (unchanged) 0 else 1)
  }

  private def progress(current: Int, total: Int, path: Path): IO[Unit] = {
    val width = sys.env.get("COLUMNS").flatMap(_.toIntOption).getOrElse(80).max(16) - 1
    val name = path.toString.replace("\r", "\\r").replace("\n", "\\n")
    val visible = if (name.length <= width) name else "..." + name.takeRight(width - 3)
    val previous = if (current == 1) "" else "\u001b[1A"
    Console[IO].print(s"$previous\r\u001b[2K($current/$total)\n\r\u001b[2K$visible")
  }
}
