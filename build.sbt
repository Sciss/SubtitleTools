lazy val baseName       = "SubtitleTools"
lazy val projectVersion = "0.1.0-SNAPSHOT"

lazy val root = project.in(file("."))
  .settings(
    name         := baseName,
    version      := projectVersion,
    scalaVersion := "2.13.5",
    libraryDependencies ++= Seq(
      "de.sciss"    %% "fileutil" % "1.1.5",
      "org.rogach"  %% "scallop"  % "4.0.2",
    ),
  )
