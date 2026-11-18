package csf

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.{Files, PosixPermission, PosixPermissions}
import munit.CatsEffectSuite

class FormatterFileSuite extends CatsEffectSuite {
  test("concurrent formatting keeps comment cursors isolated") {
    (0 until 64).toVector.parTraverse_ { index =>
      val source = s"// header $index\nobject Example$index { val x = 1\n\n// member $index\nval y = 2 }"
      IO.delay(Formatter.format(source)).flatMap(IO.fromEither).map { result =>
        assertEquals(result.linesIterator.filter(_.trim.startsWith("//")).map(_.trim).toVector,
          Vector(s"// header $index", s"// member $index"))
        assertEquals(Formatter.format(result), Right(result))
      }
    }
  }

  List(false, true).foreach { canonical =>
    test(s"checkFile reports canonical=$canonical without modifying the file") {
      Files[IO].tempDirectory.use { directory =>
        val path = directory / "Example.scala"
        val original = "object Example{val x=1}"

        for {
          formatted <- IO.fromEither(Formatter.format(original))
          source = if (canonical) formatted else original
          _ <- Stream.emit(source).covary[IO].through(Files[IO].writeUtf8(path)).compile.drain
          before <- Files[IO].getBasicFileAttributes(path)
          result <- Formatter.checkFile(path)
          after <- Files[IO].getBasicFileAttributes(path)
          actual <- Files[IO].readUtf8(path).compile.string
        } yield {
          assertEquals(result, canonical)
          assertEquals(actual, source)
          assertEquals(after.fileKey, before.fileKey)
          assertEquals(after.lastModifiedTime, before.lastModifiedTime)
        }
      }
    }
  }

  test("checkFile propagates formatting errors without modifying the file") {
    Files[IO].tempDirectory.use { directory =>
      val path = directory / "Broken.scala"
      val source = "object Broken {"

      for {
        _ <- Stream.emit(source).covary[IO].through(Files[IO].writeUtf8(path)).compile.drain
        before <- Files[IO].getBasicFileAttributes(path)
        result <- Formatter.checkFile(path).attempt
        actual <- Files[IO].readUtf8(path).compile.string
        after <- Files[IO].getBasicFileAttributes(path)
      } yield {
        assert(result.swap.exists(_.isInstanceOf[FormatError]))
        assertEquals(actual, source)
        assertEquals(after.fileKey, before.fileKey)
        assertEquals(after.lastModifiedTime, before.lastModifiedTime)
      }
    }
  }

  test("formatFile writes canonical UTF-8, preserves permissions, and cleans temporary files") {
    Files[IO].tempDirectory.use { directory =>
      val path = directory / "Example.scala"
      val source = "object Example{val café=\"héllo\"}"
      val permissions = PosixPermissions(
        PosixPermission.OwnerRead,
        PosixPermission.OwnerWrite,
        PosixPermission.GroupRead,
      )

      for {
        expected <- IO.fromEither(Formatter.format(source))
        _ <- Stream.emit(source).covary[IO].through(Files[IO].writeUtf8(path)).compile.drain
        _ <- Files[IO].setPosixPermissions(path, permissions)
        _ <- Formatter.formatFile(path)
        actual <- Files[IO].readUtf8(path).compile.string
        actualPermissions <- Files[IO].getPosixPermissions(path)
        remaining <- Files[IO].list(directory).compile.toList
      } yield {
        assertEquals(actual, expected)
        assertEquals(actualPermissions, permissions)
        assertEquals(remaining, List(path))
      }
    }
  }

  test("formatFile leaves already formatted files untouched") {
    Files[IO].tempDirectory.use { directory =>
      val path = directory / "Example.scala"

      for {
        source <- IO.fromEither(Formatter.format("object Example { val x = 1 }"))
        _ <- Stream.emit(source).covary[IO].through(Files[IO].writeUtf8(path)).compile.drain
        before <- Files[IO].getBasicFileAttributes(path)
        _ <- Formatter.formatFile(path)
        after <- Files[IO].getBasicFileAttributes(path)
        actual <- Files[IO].readUtf8(path).compile.string
      } yield {
        assertEquals(actual, source)
        assertEquals(after.fileKey, before.fileKey)
        assertEquals(after.lastModifiedTime, before.lastModifiedTime)
        assertEquals(after.creationTime, before.creationTime)
      }
    }
  }

  List("object Example {", "class Example(").foreach { source =>
    test(s"formatFile leaves original intact on formatting error: $source") {
      Files[IO].tempDirectory.use { directory =>
        val path = directory / "Example.scala"

        for {
          _ <- Stream.emit(source).covary[IO].through(Files[IO].writeUtf8(path)).compile.drain
          before <- Files[IO].getBasicFileAttributes(path)
          result <- Formatter.formatFile(path).attempt
          after <- Files[IO].getBasicFileAttributes(path)
          actual <- Files[IO].readUtf8(path).compile.string
          remaining <- Files[IO].list(directory).compile.toList
        } yield {
          assert(result.swap.exists(_.isInstanceOf[FormatError]), result.toString)
          assertEquals(actual, source)
          assertEquals(after.fileKey, before.fileKey)
          assertEquals(after.lastModifiedTime, before.lastModifiedTime)
          assertEquals(remaining, List(path))
        }
      }
    }
  }
}
