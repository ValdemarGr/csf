package csf

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.{Files, Path}
import fs2.io.process.{ProcessBuilder, Processes}
import java.io.IOException
import java.nio.file.NoSuchFileException

object FileDiscovery {
  def files(root: Path): Stream[IO, Either[Throwable, Path]] = {
    Stream.eval(Files[IO].isRegularFile(root)).flatMap { regular =>
      if (regular) Stream.emit(Right(root))
      else {
        Stream.eval(Files[IO].getBasicFileAttributes(root)).flatMap { attributes =>
          if (!attributes.isDirectory || attributes.isSymbolicLink) Stream.empty
          else {
            Stream.eval(IO(root.absolute.normalize)).flatMap { directory =>
              if (directory.endsWith(".git")) Stream.empty
              else Stream.eval(inWorkTree(directory)).flatMap { git =>
                if (git) trackedAndUntracked(directory) else walk(directory)
              }
            }
          }
        }
      }
    }.handleErrorWith(error => Stream.emit(Left(error)))
  }

  private def hasGitParent(directory: Path): IO[Boolean] = {
    Files[IO].exists(directory / ".git", false).flatMap { marker =>
      if (marker) IO.pure(true)
      else directory.parent.fold(IO.pure(false))(hasGitParent)
    }
  }

  private def inWorkTree(directory: Path): IO[Boolean] = {
    hasGitParent(directory).flatMap { marker =>
      if (!marker) IO.pure(false)
      else git(directory, List("rev-parse", "--is-inside-work-tree")).flatMap {
        case (0, output, _) => IO.pure(output.trim == "true")
        case (_, _, error) if error.contains("not a git repository") => IO.pure(false)
        case (_, _, error) => IO.raiseError(new IOException(s"git in $directory: ${error.trim}"))
      }
    }
  }

  private def git(directory: Path, arguments: List[String]): IO[(Int, String, String)] = {
    val command = ProcessBuilder("git", arguments).withWorkingDirectory(directory)
      .withExtraEnv(Map("LC_ALL" -> "C"))
    Processes[IO].spawn(command).use { process =>
      (
        process.exitValue,
        process.stdout.through(fs2.text.utf8.decode).compile.string,
        process.stderr.through(fs2.text.utf8.decode).compile.string,
      ).parTupled
    }
  }

  private def trackedAndUntracked(directory: Path): Stream[IO, Either[Throwable, Path]] = {
    val arguments = List("ls-files", "--cached", "--others", "--exclude-standard", "--deduplicate", "-z")
    Stream.eval(git(directory, arguments)).flatMap {
      case (0, output, _) =>
        Stream.emits(output.split("\u0000", -1).toList.filter(_.nonEmpty)).flatMap { name =>
          val path = (directory / name).normalize
          if (path.extName == ".scala") gitFile(directory, path) else Stream.empty
        }
      case (_, _, error) => Stream.emit(Left(new IOException(s"git in $directory: ${error.trim}")))
    }
  }

  private def gitFile(directory: Path, path: Path): Stream[IO, Either[Throwable, Path]] = {
    val parents = Iterator.iterate(path.parent)(_.flatMap(_.parent))
      .takeWhile(_.exists(_.startsWith(directory))).flatMap(_.toList).toList
    Stream.eval(parents.traverse(Files[IO].getBasicFileAttributes(_))).flatMap { ancestors =>
      if (ancestors.exists(_.isSymbolicLink)) Stream.empty
      else Stream.eval(Files[IO].getBasicFileAttributes(path)).flatMap { attributes =>
        if (attributes.isRegularFile) Stream.emit(Right(path)) else Stream.empty
      }
    }.handleErrorWith {
      case _: NoSuchFileException => Stream.empty
      case error => Stream.emit(Left(error))
    }
  }

  private def walk(path: Path): Stream[IO, Either[Throwable, Path]] = {
    Stream.eval(Files[IO].getBasicFileAttributes(path)).flatMap { attributes =>
      if (attributes.isSymbolicLink || path.endsWith(".git")) Stream.empty
      else if (attributes.isDirectory) Files[IO].list(path).flatMap(walk)
      else if (attributes.isRegularFile && path.extName == ".scala") Stream.emit(Right(path))
      else Stream.empty
    }.handleErrorWith(error => Stream.emit(Left(error)))
  }
}
