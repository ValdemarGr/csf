import sbt.*
import scala.sys.process.*

object NativeImage {
  def build(jars: Seq[File], destination: File, log: Logger): File = {
    val graalvm = file(sys.env.getOrElse("GRAALVM_HOME",
      sys.error("GRAALVM_HOME is required: nix develop -c sbt --server nativeImage")))
    val output = destination / "csf"
    IO.createDirectory(destination)
    val arguments = Seq(
      (graalvm / "bin" / "native-image").getAbsolutePath,
      "--no-fallback", "-O2",
      "--static", "--libc=musl", "-march=compatibility",
      "-R:StackSize=8388608",
      "-R:MinHeapSize=1610612736",
      s"--parallelism=${java.lang.Runtime.getRuntime.availableProcessors().min(16).max(1)}",
      "-J-Xmx8g",
      "-cp", jars.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator),
      "-o", output.getAbsolutePath,
      "csf.Main",
    )
    val exit = Process(arguments, None, "JAVA_HOME" -> graalvm.getAbsolutePath)
      .!(ProcessLogger(log.info(_), log.error(_)))
    require(exit == 0, s"native-image exited with status $exit")
    log.info(s"Native launcher: ${output.getAbsolutePath}")
    output
  }
}
