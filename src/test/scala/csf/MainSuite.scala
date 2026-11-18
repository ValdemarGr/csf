package csf

import cats.effect.{ExitCode, IO}
import cats.syntax.all.*
import com.monovore.decline.Command
import fs2.Stream
import fs2.io.file.{Files, Path}
import fs2.io.process.{ProcessBuilder, Processes}
import munit.CatsEffectSuite
import scala.concurrent.duration.*

class MainSuite extends CatsEffectSuite {
  private val command = Command(name = "csf", header = "Normalize Scala source")(Main.main)

  private def run(arguments: List[String]): IO[ExitCode] = {
    command.parse(arguments).fold(help => IO.raiseError(new AssertionError(help.toString)), identity)
  }

  private def pipe(source: String, arguments: List[String] = List("-"),
      directory: Option[Path] = None): IO[(Int, String, String)] = {
    val classpath = (Iterator.iterate(getClass.getClassLoader)(_.getParent).takeWhile(_ != null)
      .collect { case loader: java.net.URLClassLoader => loader.getURLs.toVector }.flatten
      .map(url => java.nio.file.Path.of(url.toURI).toString).toVector ++
      System.getProperty("java.class.path").split(java.io.File.pathSeparator).toVector)
      .distinct.mkString(java.io.File.pathSeparator)
    val javaExecutable = (Path(System.getProperty("java.home")) / "bin" / "java").toString
    val builder = ProcessBuilder(javaExecutable, List("-cp", classpath, "csf.Main") ++ arguments)

    Processes[IO].spawn(directory.fold(builder)(builder.withWorkingDirectory)).use { process =>
      (
        Stream.emit(source).covary[IO].through(fs2.text.utf8.encode).through(process.stdin).compile.drain,
        process.exitValue,
        process.stdout.through(fs2.text.utf8.decode).compile.string,
        process.stderr.through(fs2.text.utf8.decode).compile.string,
      ).parTupled.map { case (_, exit, stdout, stderr) => (exit, stdout, stderr) }
    }.timeout(20.seconds)
  }

  test("paths are mandatory") {
    val result = command.parse(Nil)

    assert(result.left.exists(_.errors.nonEmpty))
  }

  test("stdin accepts the dash sentinel and positive columns") {
    assert(command.parse(List("-")).isRight)
    assert(command.parse(List("-", "--columns", "40")).isRight)
    assert(command.parse(List("--columns", "40", "-")).isRight)
    assert(command.parse(List("./-")).isRight)
  }

  List(
    List("-", "Example.scala"),
    List("Example.scala", "-"),
    List("-", "-"),
    List("--write", "-"),
    List("-", "--write"),
    List("--check", "-"),
    List("-", "--check"),
  ).foreach { arguments =>
    test(s"stdin rejects mixed inputs and file modes: ${arguments.mkString(" ")}") {
      assert(command.parse(arguments).left.exists(_.errors.nonEmpty))
    }
  }

  List("0", "-1", "forty").foreach { columns =>
    test(s"stdin rejects invalid columns: $columns") {
      assert(command.parse(List("-", "--columns", columns)).left.exists(_.errors.nonEmpty))
    }
  }

  test("stdin emits only formatted source and succeeds for both changed and canonical buffers") {
    val source = "object Example{val value=1}"

    for {
      expected <- IO.fromEither(Formatter.format(source))
      changed <- pipe(source)
      canonical <- pipe(expected)
    } yield {
      assertNotEquals(source, expected)
      assertEquals(changed._1, 0, changed._3)
      assertEquals(changed._2, expected)
      assertEquals(canonical._1, 0, canonical._3)
      assertEquals(canonical._2, expected)
    }
  }

  test("empty stdin emits no bytes and succeeds") {
    pipe("").map { case (exit, stdout, stderr) =>
      assertEquals(exit, 0, stderr)
      assertEquals(stdout, "")
    }
  }

  List(
    "Unicode and CRLF" -> "object Example{\r\nval message=\"héllo λ 🙂\"\r\n}",
    "UTF-8 across multiple input buffers" -> s"val message=\"${"λé🙂" * 4096}\"",
  ).foreach { case (name, source) =>
    test(s"stdin preserves $name") {
      for {
        expected <- IO.fromEither(Formatter.format(source))
        result <- pipe(source)
      } yield {
        assertEquals(result._1, 0, result._3)
        assertEquals(result._2, expected)
        assert(!result._2.contains('\r'))
      }
    }
  }

  test("columns controls stdout formatting of the unsaved buffer") {
    val source = "object Example{val result=combine(firstArgument,secondArgument,thirdArgument)}"

    for {
      wide <- IO.fromEither(Formatter.format(source))
      narrow <- IO.fromEither(Formatter.format(source, 40))
      result <- pipe(source, List("--columns", "40", "-"))
    } yield {
      assertNotEquals(narrow, wide)
      assertEquals(result._1, 0, result._3)
      assertEquals(result._2, narrow)
    }
  }

  test("malformed stdin reports diagnostics without writing partial source to stdout") {
    pipe("val valid=1\nobject Broken {").map { case (exit, stdout, stderr) =>
      assertEquals(exit, 2)
      assertEquals(stdout, "")
      assert(stderr.contains("error"), stderr)
      assert(stderr.contains("object Broken"), stderr)
    }
  }

  test("stdin ignores filesystem inputs and leaves a literal dash file untouched") {
    Files[IO].tempDirectory.use { directory =>
      val path = directory / "-"
      val broken = directory / "Broken.scala"
      val invalid = "object Broken {"
      val source = "object Buffer{val value=1}"

      for {
        _ <- List(path, broken).traverse_ { file =>
          Stream.emit(invalid).covary[IO].through(Files[IO].writeUtf8(file)).compile.drain
        }
        expected <- IO.fromEither(Formatter.format(source))
        result <- pipe(source, directory = Some(directory))
        dashAfter <- Files[IO].readUtf8(path).compile.string
        brokenAfter <- Files[IO].readUtf8(broken).compile.string
        remaining <- Files[IO].list(directory).compile.toList
      } yield {
        assertEquals(result._1, 0, result._3)
        assertEquals(result._2, expected)
        assertEquals(dashAfter, invalid)
        assertEquals(brokenAfter, invalid)
        assertEquals(remaining.toSet, Set(path, broken))
      }
    }
  }

  List(List("--check", "--write"), List("--write", "--check")).foreach { flags =>
    test(s"check and write flags are mutually exclusive: ${flags.mkString(" ")}") {
      val result = command.parse(flags :+ "Example.scala")

      assert(result.left.exists(_.errors.nonEmpty))
    }
  }

  test("help parses without requiring paths") {
    val result = command.parse(List("--help"))

    assert(result.left.exists(_.errors.isEmpty))
    assert(result.left.exists(_.toString.contains("--check")))
    assert(result.left.exists(_.toString.contains("--write")))
    assert(result.left.exists(_.toString.contains("--columns")))
  }

  List("0", "-1").foreach { columns =>
    test(s"nonpositive columns are rejected: $columns") {
      val result = command.parse(List("--columns", columns, "Example.scala"))

      assert(result.left.exists(_.errors.exists(_.contains("Columns must be greater than zero."))))
    }
  }

  test("noninteger columns are rejected") {
    val result = command.parse(List("--columns", "forty", "Example.scala"))

    assert(result.left.exists(_.errors.nonEmpty))
  }

  List(Nil, List("--check")).foreach { flags =>
    test(s"${if (flags.isEmpty) "default" else "explicit"} check reports changes without writing") {
      Files[IO].tempDirectory.use { directory =>
        val path = directory / "Example.scala"
        val source = "object Example{val x=1}"

        for {
          _ <- Stream.emit(source).covary[IO].through(Files[IO].writeUtf8(path)).compile.drain
          before <- Files[IO].getBasicFileAttributes(path)
          exit <- run(flags :+ path.toString)
          actual <- Files[IO].readUtf8(path).compile.string
          after <- Files[IO].getBasicFileAttributes(path)
        } yield {
          assertEquals(exit, ExitCode(1))
          assertEquals(actual, source)
          assertEquals(after.fileKey, before.fileKey)
          assertEquals(after.lastModifiedTime, before.lastModifiedTime)
        }
      }
    }
  }

  test("check succeeds when all supplied files are canonical") {
    Files[IO].tempDirectory.use { directory =>
      val first = directory / "First.scala"
      val second = directory / "Second.scala"

      for {
        source <- IO.fromEither(Formatter.format("object Example { val x = 1 }"))
        _ <- List(first, second).traverse_ { path =>
          Stream.emit(source).covary[IO].through(Files[IO].writeUtf8(path)).compile.drain
        }
        exit <- run(List(first.toString, second.toString))
      } yield assertEquals(exit, ExitCode.Success)
    }
  }

  test("write formats files and subsequently passes check") {
    Files[IO].tempDirectory.use { directory =>
      val path = directory / "Example.scala"
      val source = "object Example{val x=1}"

      for {
        expected <- IO.fromEither(Formatter.format(source))
        _ <- Stream.emit(source).covary[IO].through(Files[IO].writeUtf8(path)).compile.drain
        writeExit <- run(List("--write", path.toString))
        actual <- Files[IO].readUtf8(path).compile.string
        checkExit <- run(List("--check", path.toString))
      } yield {
        assertEquals(writeExit, ExitCode.Success)
        assertEquals(actual, expected)
        assertEquals(checkExit, ExitCode.Success)
      }
    }
  }

  test("columns controls write and check for explicit files and directory inputs") {
    Files[IO].tempDirectory.use { directory =>
      val nested = directory / "nested"
      val first = directory / "First.scala"
      val second = nested / "Second.scala"
      val files = List(first, second)
      val arguments = List(first.toString, nested.toString)
      val source = "object Example{val result=combine(firstArgument,secondArgument,thirdArgument)}"

      for {
        wide <- IO.fromEither(Formatter.format(source))
        narrow <- IO.fromEither(Formatter.format(source, 40))
        _ <- Files[IO].createDirectory(nested)
        _ <- files.traverse_ { path =>
          Stream.emit(source).covary[IO].through(Files[IO].writeUtf8(path)).compile.drain
        }
        defaultWrite <- run("--write" :: arguments)
        defaultContents <- files.traverse(path => Files[IO].readUtf8(path).compile.string)
        narrowCheckBefore <- run(List("--columns", "40", "--check") ++ arguments)
        checkedContents <- files.traverse(path => Files[IO].readUtf8(path).compile.string)
        narrowWrite <- run(List("--columns", "40", "--write") ++ arguments)
        narrowContents <- files.traverse(path => Files[IO].readUtf8(path).compile.string)
        narrowCheckAfter <- run(List("--columns", "40", "--check") ++ arguments)
        defaultCheckAfter <- run("--check" :: arguments)
      } yield {
        assertNotEquals(narrow, wide)
        assertEquals(defaultWrite, ExitCode.Success)
        assertEquals(defaultContents, List(wide, wide))
        assertEquals(narrowCheckBefore, ExitCode(1))
        assertEquals(checkedContents, defaultContents)
        assertEquals(narrowWrite, ExitCode.Success)
        assertEquals(narrowContents, List(narrow, narrow))
        assertEquals(narrowCheckAfter, ExitCode.Success)
        assertEquals(defaultCheckAfter, ExitCode(1))
      }
    }
  }

  test("Git directory arguments recurse over Scala files and honor gitignore") {
    Files[IO].tempDirectory.use { directory =>
      val nested = directory / "nested"
      val ignored = directory / "ignored"
      val path = nested / "Example.scala"
      val ignoredPath = ignored / "Broken.scala"
      val textPath = directory / "notes.txt"
      val source = "object Example{val x=1}"
      val invalid = "object Broken {"

      for {
        expected <- IO.fromEither(Formatter.format(source))
        initialized <- Processes[IO]
          .spawn(ProcessBuilder("git", "init", "--quiet", directory.toString))
          .use(_.exitValue)
        _ <- IO(assertEquals(initialized, 0))
        _ <- Files[IO].createDirectory(nested)
        _ <- Files[IO].createDirectory(ignored)
        _ <- List(
          (directory / ".gitignore", "ignored/\n"),
          (path, source),
          (ignoredPath, invalid),
          (textPath, invalid),
        ).traverse_ { case (file, contents) =>
          Stream.emit(contents).covary[IO].through(Files[IO].writeUtf8(file)).compile.drain
        }
        exit <- run(List("--write", directory.toString))
        actual <- Files[IO].readUtf8(path).compile.string
        ignoredActual <- Files[IO].readUtf8(ignoredPath).compile.string
        textActual <- Files[IO].readUtf8(textPath).compile.string
      } yield {
        assertEquals(exit, ExitCode.Success)
        assertEquals(actual, expected)
        assertEquals(ignoredActual, invalid)
        assertEquals(textActual, invalid)
      }
    }
  }

  test("write continues after missing and malformed files and preserves failed input") {
    Files[IO].tempDirectory.use { directory =>
      val missing = directory / "Missing.scala"
      val broken = directory / "Broken.scala"
      val valid = directory / "Valid.scala"
      val invalidSource = "object Broken {"
      val validSource = "object Valid{val x=1}"

      for {
        expected <- IO.fromEither(Formatter.format(validSource))
        _ <- List(broken -> invalidSource, valid -> validSource).traverse_ { case (path, source) =>
          Stream.emit(source).covary[IO].through(Files[IO].writeUtf8(path)).compile.drain
        }
        exit <- run(List("--write", missing.toString, broken.toString, valid.toString))
        actual <- Files[IO].readUtf8(valid).compile.string
        brokenActual <- Files[IO].readUtf8(broken).compile.string
      } yield {
        assertEquals(exit, ExitCode(2))
        assertEquals(actual, expected)
        assertEquals(brokenActual, invalidSource)
      }
    }
  }

  test("check errors take precedence over files needing formatting") {
    Files[IO].tempDirectory.use { directory =>
      val dirty = directory / "Dirty.scala"
      val broken = directory / "Broken.scala"
      val dirtySource = "object Dirty{val x=1}"
      val brokenSource = "object Broken {"

      for {
        _ <- List(dirty -> dirtySource, broken -> brokenSource).traverse_ { case (path, source) =>
          Stream.emit(source).covary[IO].through(Files[IO].writeUtf8(path)).compile.drain
        }
        exit <- run(List("--check", dirty.toString, broken.toString))
        dirtyActual <- Files[IO].readUtf8(dirty).compile.string
        brokenActual <- Files[IO].readUtf8(broken).compile.string
      } yield {
        assertEquals(exit, ExitCode(2))
        assertEquals(dirtyActual, dirtySource)
        assertEquals(brokenActual, brokenSource)
      }
    }
  }

  test("batched writes isolate comments and continue after a malformed file") {
    Files[IO].tempDirectory.use { directory =>
      val examples = (0 until 64).toList.map { index =>
        val source = s"""// file $index
object Example$index{
/* body $index */
val value=$index // trailing $index

val text="file $index"
}"""
        (directory / s"Example$index.scala", source)
      }
      val broken = directory / "Broken.scala"
      val invalidSource = "object Broken { /* incomplete */"
      val paths = examples.map(_._1.toString)

      for {
        expected <- examples.traverse { case (path, source) =>
          IO.fromEither(Formatter.format(source)).map(path -> _)
        }
        _ <- (examples :+ (broken -> invalidSource)).traverse_ { case (path, source) =>
          Stream.emit(source).covary[IO].through(Files[IO].writeUtf8(path)).compile.drain
        }
        writeExit <- run("--write" :: (paths.take(32) ++ List(broken.toString) ++ paths.drop(32)))
        _ <- expected.traverse_ { case (path, canonical) =>
          Files[IO].readUtf8(path).compile.string.flatMap { actual =>
            IO(assertEquals(actual, canonical, path.toString))
          }
        }
        firstCheck <- run("--check" :: paths)
        secondCheck <- run("--check" :: paths)
        brokenActual <- Files[IO].readUtf8(broken).compile.string
        remaining <- Files[IO].list(directory).compile.toList
      } yield {
        assertEquals(writeExit, ExitCode(2))
        assertEquals(firstCheck, ExitCode.Success)
        assertEquals(secondCheck, ExitCode.Success)
        assertEquals(brokenActual, invalidSource)
        assertEquals(remaining.toSet, examples.map(_._1).toSet + broken)
      }
    }
  }
}
