package csf

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import cats.syntax.all.*
import fs2.io.file.{Files, Path}
import scala.meta.*

object CorpusCheck extends IOApp {
  private given Dialect = dialects.Scala3

  private case class Result(kinds: Map[String, (Long, String)], failures: List[String])
  private case class Report(files: Int, passed: Int, kinds: Map[String, (Long, String)], failures: List[String])

  def run(arguments: List[String]): IO[ExitCode] = arguments match {
    case root :: options if options.isEmpty || options == List("--inventory") =>
      val inventoryOnly = options.nonEmpty
      FileDiscovery.files(Path(root)).evalMap {
        case Left(error) => IO.pure(Result(Map.empty, List(s"discovery: ${error.getMessage}")))
        case Right(path) =>
          Files[IO].readUtf8(path).compile.string.flatMap { source =>
            IO.delay(inspect(path, source, inventoryOnly))
          }.handleError(error => Result(Map.empty, List(s"$path exception: ${error.getClass.getName}: ${error.getMessage}")))
      }.compile.fold(Report(0, 0, Map.empty, Nil)) { (report, result) =>
        val kinds = result.kinds.foldLeft(report.kinds) { case (all, (kind, (count, example))) =>
          all.updatedWith(kind) {
            case Some((previous, representative)) => Some((previous + count, representative))
            case None => Some((count, example))
          }
        }
        Report(report.files + 1, report.passed + (if (result.failures.isEmpty) 1 else 0), kinds,
          report.failures ++ result.failures)
      }.flatMap { report =>
        val heading = s"files=${report.files} passed=${report.passed} failed=${report.files - report.passed} findings=${report.failures.size} nodeKinds=${report.kinds.size} inventoryOnly=$inventoryOnly"
        val inventory = report.kinds.toList.sortBy(_._1).map { case (kind, (count, example)) =>
          s"NODE\t$kind\t$count\t$example"
        }
        Console[IO].println(heading) *>
          (if (inventoryOnly) inventory.traverse_(Console[IO].println) else IO.unit) *>
          report.failures.traverse_(Console[IO].println) *>
          IO.pure(if (report.failures.isEmpty) ExitCode.Success else ExitCode.Error)
      }
    case _ => Console[IO].errorln("Usage: CorpusCheck <root> [--inventory]").as(ExitCode(2))
  }

  private def snippet(value: String): String = value.replaceAll("\\s+", " ").take(180)

  private def inspect(path: Path, source: String, inventoryOnly: Boolean): Result = {
    source.parse[Source] match {
      case error: Parsed.Error => Result(Map.empty, List(s"$path parse: $error"))
      case Parsed.Success(original) =>
        val nodes = original.collect { case tree: Tree => tree }
        val kinds = nodes.foldLeft(Map.empty[String, (Long, String)]) { (all, node) =>
          all.updatedWith(node.productPrefix) {
            case Some((count, example)) => Some((count + 1L, example))
            case None => Some((1L, s"$path:${node.pos.startLine + 1} ${snippet(node.pos.text)}"))
          }
        }
        if (inventoryOnly) Result(kinds, Nil)
        else Formatter.format(source) match {
          case Left(FormatError.Unsupported(node)) =>
            val example = nodes.find(_.productPrefix == node).fold("")(tree =>
              s":${tree.pos.startLine + 1} ${snippet(tree.pos.text)}")
            Result(kinds, List(s"$path unsupported: $node$example"))
          case Left(error) => Result(kinds, List(s"$path format: ${error.getMessage}"))
          case Right(formatted) =>
            val reparsed = {
              given Dialect = dialects.Scala3.withAllowSignificantIndentation(false)
              formatted.parse[Source]
            }
            val structureFailures = reparsed match {
              case error: Parsed.Error => List(s"$path output-parse: $error")
              case Parsed.Success(revised) => mismatch(original, revised, "Source").toList.map { detail =>
                s"$path structure: $detail"
              }
            }
            val originalComments = source.tokenize.get.collect { case comment: Token.Comment => comment.text }.toList
            val revisedTokens = formatted.tokenize.get
            val commentTokens = revisedTokens.collect { case comment: Token.Comment => comment }.toList
            val revisedComments = commentTokens.map(_.text)
            val lines = formatted.linesIterator.toVector
            val commentLayoutFailures = commentTokens.filter { comment =>
              lines(comment.pos.startLine).take(comment.pos.startColumn).exists(!_.isWhitespace) ||
                lines(comment.pos.endLine).drop(comment.pos.endColumn).exists(!_.isWhitespace)
            }.map(comment => s"$path comment-layout:${comment.pos.startLine + 1} ${snippet(comment.text)}")
            val whitespaceFailures = revisedTokens.iterator.filter { token =>
              token.isInstanceOf[Token.Trivia] && !token.isInstanceOf[Token.Comment] && token.text.nonEmpty &&
                token.text.forall(character => character == ' ' || character == '\t') &&
                (token.end == formatted.length || formatted.charAt(token.end) == '\n' ||
                  formatted.charAt(token.end) == '\r')
            }.map(token => s"$path trailing-whitespace:${token.pos.startLine + 1}").toList
            val commentFailures = if (originalComments == revisedComments) Nil
            else List(s"$path comments: original=${originalComments.size} revised=${revisedComments.size} first=" +
              originalComments.zipAll(revisedComments, "<missing>", "<missing>").find { case (left, right) => left != right }
                .map { case (left, right) => s"${snippet(left)} != ${snippet(right)}" }.getOrElse(""))
            val secondPassFailures = Formatter.format(formatted) match {
              case Left(error) => List(s"$path second-pass: ${error.getMessage}")
              case Right(second) if second != formatted =>
                val offset = formatted.zip(second).indexWhere { case (left, right) => left != right } match {
                  case -1 => math.min(formatted.length, second.length)
                  case index => index
                }
                List(s"$path idempotence: offset=$offset first=${snippet(formatted.slice(math.max(0, offset - 50), offset + 120))}" +
                  s" second=${snippet(second.slice(math.max(0, offset - 50), offset + 120))}")
              case _ => Nil
            }
            Result(kinds, structureFailures ++ commentFailures ++ commentLayoutFailures ++ whitespaceFailures ++ secondPassFailures)
        }
    }
  }

  private def mismatch(left: Any, right: Any, location: String): Option[String] = (left, right) match {
    case (Term.Block(List(term: Term)), other) => mismatch(term, other, location)
    case (other, Term.Block(List(term: Term))) => mismatch(other, term, location)
    case (a: Tree, b: Tree) if a.productPrefix != b.productPrefix =>
      Some(s"$location ${a.productPrefix} != ${b.productPrefix}; original=${snippet(a.pos.text)} revised=${snippet(b.pos.text)}")
    case (a: Tree, b: Tree) if a.productArity != b.productArity =>
      Some(s"$location ${a.productPrefix} arity ${a.productArity} != ${b.productArity}")
    case (a: Tree, b: Tree) =>
      a.productIterator.zip(b.productIterator).zipWithIndex.flatMap { case ((x, y), index) =>
        mismatch(x, y, s"$location.${a.productFields(index)}")
      }.nextOption()
    case (a: List[?], b: List[?]) if a.size != b.size => Some(s"$location list size ${a.size} != ${b.size}")
    case (a: List[?], b: List[?]) =>
      a.iterator.zip(b.iterator).zipWithIndex.flatMap { case ((x, y), index) =>
        mismatch(x, y, s"$location[$index]")
      }.nextOption()
    case (Some(a), Some(b)) => mismatch(a, b, location)
    case (a, b) if a == b => None
    case (a, b) => Some(s"$location ${snippet(a.toString)} != ${snippet(b.toString)}")
  }
}
