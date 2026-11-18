package csf

import com.github.difflib.DiffUtils
import com.github.difflib.algorithm.myers.MyersDiffWithLinearSpace
import scala.jdk.CollectionConverters.*

private[csf] object TextDiff {
  def apply(original: String, revised: String): String = {
    val originalLines = original.linesWithSeparators.toVector.asJava
    val revisedLines = revised.linesWithSeparators.toVector.asJava
    DiffUtils.diff(originalLines, revisedLines, new MyersDiffWithLinearSpace[String]())
      .applyTo(originalLines)
      .asScala
      .mkString
  }
}
