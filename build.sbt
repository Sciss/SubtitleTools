lazy val baseName       = "SubtitleTools"
lazy val projectVersion = "0.1.0-SNAPSHOT"

lazy val root = project.in(file("."))
  .settings(
    name         := baseName,
    version      := projectVersion,
    scalaVersion := "2.13.5",
    description  := "Tools for manipulating srt (SubRip) files",
    homepage     := Some(url(s"https://github.com/Sciss/$baseName")),
    licenses     := Seq("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt")),
    libraryDependencies ++= Seq(
      "de.sciss"    %% "fileutil" % "1.1.5",
      "org.rogach"  %% "scallop"  % "4.0.2",
    ),
  )
