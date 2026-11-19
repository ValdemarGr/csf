scalaVersion := "3.9.0"

name := "csf"

scalacOptions += "-no-indent"

val jpackage = taskKey[File]("Build a Linux app image and archive with a Leyden AOT cache.")
val nativeImage = taskKey[File]("Build a GraalVM native CLI executable.")

jpackage := Def.uncached {
  val converter = fileConverter.value
  val jars = (Runtime / fullClasspathAsJars).value.map(entry => converter.toPath(entry.data).toFile)
  val mainJar = converter.toPath((Compile / packageBin).value).getFileName.toString
  Packaging.build(jars, mainJar, baseDirectory.value / "examples",
    baseDirectory.value / "target" / "jpackage", streams.value.log)
}

nativeImage := Def.uncached {
  val converter = fileConverter.value
  val jars = (Runtime / fullClasspathAsJars).value.map(entry => converter.toPath(entry.data).toFile)
  NativeImage.build(jars, baseDirectory.value / "target" / "native", streams.value.log)
}

val fs2Version = "3.14.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.13.0",
  "org.typelevel" %% "cats-effect" % "3.7.1",
  "co.fs2" %% "fs2-core" % fs2Version,
  "co.fs2" %% "fs2-io" % fs2Version,
  "org.scalameta" %% "scalameta" % "4.17.4",
  "org.typelevel" %% "paiges-core" % "0.4.4",
  "com.monovore" %% "decline-effect" % "2.6.2",
  "org.tomlj" % "tomlj" % "1.1.1",
  "io.github.java-diff-utils" % "java-diff-utils" % "4.18",
  "org.typelevel" %% "munit-cats-effect" % "2.2.1" % Test,
)
