import sbt._, Keys._

object RegionsBuild extends Build {
  val defaults = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.11.2"
  )

  lazy val root = Project(
    "root",
    file("."),
    settings = defaults,
    aggregate = Seq(src, macros)
  )

  lazy val src = Project(
    "src",
    file("src"),
    settings = defaults,
    dependencies = Seq(macros)
  )

  lazy val macros = Project(
    "macros",
    file("macros"),
    settings = defaults ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
    )
  )

  lazy val sandbox = Project(
    "sandbox",
    file("sandbox"),
    settings = defaults ++ Seq(
      scalacOptions += "-Xprint:all"
    ),
    dependencies = Seq(src)
  )
}
