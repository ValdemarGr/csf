package csf

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.security.MessageDigest
import java.util.HexFormat
import java.util.jar.JarFile
import munit.FunSuite
import scala.jdk.CollectionConverters.*
import scala.meta.Tree
import scala.meta.internal.trees.Quasi
import scala.util.Using

class AstSchemaSuite extends FunSuite {
  test("Scalameta AST schema changes require printer review") {
    // Tree is open. This detects dependency schema drift, not printer exhaustiveness.
    val source = Path.of(classOf[Tree].getProtectionDomain.getCodeSource.getLocation.toURI)
    val interfaces = Using.resource(new JarFile(source.toFile)) { jar =>
      jar.entries().asScala.filter(_.getName.endsWith(".class")).map { entry =>
        val name = entry.getName.stripSuffix(".class").replace('/', '.')
        Class.forName(name, false, classOf[Tree].getClassLoader)
      }.filter { node =>
        node.isInterface && classOf[Tree].isAssignableFrom(node) &&
          !classOf[Quasi].isAssignableFrom(node)
      }.toList
    }
    val leaves = interfaces.filterNot { node =>
      interfaces.exists(other => node != other && node.isAssignableFrom(other))
    }.map(_.getName.stripPrefix("scala.meta.").replace('$', '.')).sorted.mkString("\n")
    val fingerprint = HexFormat.of().formatHex(
      MessageDigest.getInstance("SHA-256").digest(leaves.getBytes(StandardCharsets.UTF_8)),
    )

    // Reviewed leaf names from Scalameta 4.17.4; review changes before updating this snapshot.
    assertEquals(
      fingerprint,
      "02d81284e8e191e775246a8dc2e334ad282405a280716385e27ae5313bf30aca",
      s"Scalameta AST leaf schema changed. Review printer handling before updating snapshot:\n$leaves",
    )
  }
}
