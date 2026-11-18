package csf

import munit.FunSuite

class TextDiffSuite extends FunSuite {
  private val examples = List(
    ("insert into empty file", "", "object Example {}\n"),
    ("delete complete file", "object Example {}\n", ""),
    ("empty file remains empty", "", ""),
    ("unchanged text preserves mixed endings", "first\r\n\rsecond\nlast", "first\r\n\rsecond\nlast"),
    ("add final newline", "object Example {}", "object Example {}\n"),
    ("remove final newline", "object Example {}\n", "object Example {}"),
    ("normalize CRLF to LF", "first\r\n\r\nlast\r\n", "first\n\nlast\n"),
    ("preserve revised CRLF", "first\nlast\n", "first\r\nlast\r\n"),
    ("preserve blank lines and bare CR", "first\rlast\r", "\rfirst\r\rlast\r\r"),
    (
      "apply separated insertions deletions and replacements",
      "remove\nkeep one\nreplace\nkeep two\nkeep three\n",
      "keep one\nreplacement\nkeep two\ninserted\nkeep three\nlast",
    ),
    (
      "preserve Unicode and literal line contents",
      "val café = \"héllo\"\nval emoji = \"😀\"\nval text = \"\\r\\n\"\n",
      "val café = \"héllo 世界\"\nval emoji = \"🦊😀\"\nval text = \"\\r\\n\"\n",
    ),
  )

  examples.foreach { case (name, original, revised) =>
    test(name) {
      assertEquals(TextDiff(original, revised), revised)
    }
  }
}
