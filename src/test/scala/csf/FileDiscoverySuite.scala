package csf

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.{Files, Path}
import fs2.io.process.{ProcessBuilder, Processes}
import munit.CatsEffectSuite

class FileDiscoverySuite extends CatsEffectSuite {
  private def write(directory: Path, name: String, contents: String = ""): IO[Path] = {
    val path = directory / name
    Files[IO].createDirectories(path.parent.get) *>
      Stream.emit(contents).through(Files[IO].writeUtf8(path)).compile.drain.as(path)
  }

  private def git(directory: Path, arguments: String*): IO[Unit] = {
    Processes[IO].spawn(ProcessBuilder("git", arguments.toList).withWorkingDirectory(directory)).use { process =>
      (
        process.exitValue,
        process.stdout.compile.drain,
        process.stderr.through(fs2.text.utf8.decode).compile.string,
      ).parTupled.flatMap {
        case (0, _, _) => IO.unit
        case (_, _, error) => IO.raiseError(new IllegalStateException(error))
      }
    }
  }

  private def found(root: Path): IO[Set[Path]] = {
    FileDiscovery.files(root).evalMap(_.liftTo[IO]).compile.toList.map(_.toSet)
  }

  test("standalone directories walk Scala files recursively without applying gitignore") {
    Files[IO].tempDirectory.use { directory =>
      for {
        _ <- write(directory, ".gitignore", "Ignored.scala\nnested/\n")
        ignored <- write(directory, "Ignored.scala")
        nested <- write(directory, "nested/Nested.scala")
        _ <- write(directory, "notes.txt")
        actual <- found(directory)
      } yield assertEquals(actual, Set(ignored, nested))
    }
  }

  test("Git discovery includes tracked and nonignored untracked Scala files") {
    Files[IO].tempDirectory.use { directory =>
      for {
        _ <- git(directory, "init", "--quiet")
        tracked <- write(directory, "Tracked.scala")
        _ <- git(directory, "add", "Tracked.scala")
        _ <- write(directory, ".gitignore", "Tracked.scala\nIgnored.scala\nignored/\n")
        untracked <- write(directory, "Untracked.scala")
        _ <- write(directory, "Ignored.scala")
        _ <- write(directory, "ignored/Hidden.scala")
        _ <- write(directory, ".git/Metadata.scala")
        _ <- write(directory, "notes.txt")
        actual <- found(directory)
      } yield assertEquals(actual, Set(tracked, untracked))
    }
  }

  test("Git discovery honors nested gitignore negations and scopes subdirectories") {
    Files[IO].tempDirectory.use { directory =>
      for {
        _ <- git(directory, "init", "--quiet")
        _ <- write(directory, ".gitignore", "*.scala\n!Kept.scala\n")
        kept <- write(directory, "Kept.scala")
        _ <- write(directory, "Ignored.scala")
        _ <- write(directory, "src/.gitignore", "!Nested.scala\n")
        nested <- write(directory, "src/Nested.scala")
        _ <- write(directory, "src/Other.scala")
        all <- found(directory)
        scoped <- found(directory / "src")
      } yield {
        assertEquals(all, Set(kept, nested))
        assertEquals(scoped, Set(nested))
      }
    }
  }

  test("explicit files bypass ignore and extension filtering") {
    Files[IO].tempDirectory.use { directory =>
      for {
        _ <- git(directory, "init", "--quiet")
        _ <- write(directory, ".gitignore", "Ignored.scala\nnotes.txt\n")
        ignored <- write(directory, "Ignored.scala")
        other <- write(directory, "notes.txt")
        ignoredResult <- found(ignored)
        otherResult <- found(other)
      } yield {
        assertEquals(ignoredResult, Set(ignored))
        assertEquals(otherResult, Set(other))
      }
    }
  }

  test("Git discovery preserves filenames containing whitespace and Unicode") {
    Files[IO].tempDirectory.use { directory =>
      for {
        _ <- git(directory, "init", "--quiet")
        spaced <- write(directory, "with spaces.scala")
        newline <- write(directory, "with\nnewline.scala")
        unicode <- write(directory, "héllo.scala")
        actual <- found(directory)
      } yield assertEquals(actual, Set(spaced, newline, unicode))
    }
  }

  test("recursive discovery skips file and directory symlinks") {
    Files[IO].tempDirectory.use { directory =>
      for {
        source <- write(directory, "source/Example.scala")
        _ <- Files[IO].createSymbolicLink(directory / "Link.scala", source)
        _ <- Files[IO].createSymbolicLink(directory / "linked", directory / "source")
        plain <- found(directory)
        _ <- git(directory, "init", "--quiet")
        gitResult <- found(directory)
      } yield {
        assertEquals(plain, Set(source))
        assertEquals(gitResult, Set(source))
      }
    }
  }

  test("Git file markers work without a .git directory") {
    Files[IO].tempDirectory.use { directory =>
      val worktree = directory / "worktree"
      val metadata = directory / "metadata"
      for {
        _ <- Files[IO].createDirectory(worktree)
        _ <- git(worktree, "init", "--quiet", "--separate-git-dir", metadata.toString)
        _ <- write(worktree, ".gitignore", "Ignored.scala\n")
        kept <- write(worktree, "Kept.scala")
        _ <- write(worktree, "Ignored.scala")
        actual <- found(worktree)
      } yield assertEquals(actual, Set(kept))
    }
  }

  test("missing explicit input is reported as an error") {
    Files[IO].tempDirectory.use { directory =>
      FileDiscovery.files(directory / "Missing.scala").compile.toList.map { actual =>
        assertEquals(actual.size, 1)
        assert(actual.head.isLeft)
      }
    }
  }
}
