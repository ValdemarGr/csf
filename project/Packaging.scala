import sbt.*
import scala.sys.process.*

object Packaging {
  def build(jars: Seq[File], mainJar: String, examples: File, destination: File, log: Logger): File = {
    require(java.lang.Runtime.version().feature() >= 27, "Packaging requires JDK 27+: nix develop -c sbt jpackage")
    require(sys.props("os.name") == "Linux", "This packaging task builds a Linux app image.")

    val jdk = file(sys.props("java.home"))
    val input = destination / "input"
    val image = destination / "csf"
    val launcher = image / "bin" / "csf"
    val app = image / "lib" / "app"
    val archive = destination / "csf.tar.gz"
    val recordingOption = "-XX:AOTCacheOutput=$APPDIR/csf.aot"
    val productionOption = "-XX:AOTCache=$APPDIR/csf.aot"

    def run(arguments: Seq[String], environment: (String, String)*): Unit = {
      val exit = Process(arguments, None, environment*).!(ProcessLogger(log.info(_), log.error(_)))
      require(exit == 0, s"${arguments.head} exited with status $exit")
    }

    IO.delete(destination)
    IO.createDirectory(input)
    jars.foreach(jar => IO.copyFile(jar, input / jar.getName, preserveLastModified = true))

    run(Seq(
      (jdk / "bin" / "jpackage").getAbsolutePath,
      "--type", "app-image",
      "--name", "csf",
      "--input", input.getAbsolutePath,
      "--dest", destination.getAbsolutePath,
      "--main-jar", mainJar,
      "--main-class", "csf.Main",
      "--jlink-options", "--strip-debug --no-man-pages --no-header-files",
      "--java-options", recordingOption,
    ))

    IO.withTemporaryDirectory { training =>
      IO.copyDirectory(examples, training, preserveLastModified = true)
      IO.createDirectory(training / "ignored")
      IO.append(training / ".gitignore", "\nignored/\n")
      IO.write(training / "ignored" / "Invalid.scala", "object Invalid {")
      run(Seq("git", "init", "--quiet", training.getAbsolutePath))
      run(Seq(launcher.getAbsolutePath, "--write", training.getAbsolutePath,
        (training / "Clean.scala").getAbsolutePath))
      require((app / "csf.aot").isFile, "AOT training did not produce csf.aot")

      val configuration = app / "csf.cfg"
      val contents = IO.read(configuration)
      require(contents.contains(recordingOption), "Packaged launcher is missing its AOT recording option")
      IO.write(configuration, contents.replace(recordingOption, productionOption))
      run(Seq(launcher.getAbsolutePath, "--check", training.getAbsolutePath),
        "JAVA_TOOL_OPTIONS" -> "-XX:AOTMode=required")
    }

    run(Seq("tar", "--create", "--gzip", "--file", archive.getAbsolutePath,
      "--directory", destination.getAbsolutePath, "csf"))
    log.info(s"Launcher: ${launcher.getAbsolutePath}")
    log.info(s"Archive: ${archive.getAbsolutePath}")
    archive
  }
}
