import sbt._, Keys._

object RegionsBuild extends Build {
  val paradiseVersion = "2.0.1"
  val defaults = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.11.2",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
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
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies ++= (
        if (scalaVersion.value.startsWith("2.10")) List("org.scalamacros" %% "quasiquotes" % paradiseVersion)
        else Nil
      )
    )
  )

  lazy val sandbox = Project(
    "sandbox",
    file("sandbox"),
    settings = defaults /*++ Seq(
      scalacOptions += "-Xprint:all"
    )*/,
    dependencies = Seq(src)
  )

  lazy val tests = Project(
    "tests",
    file("tests"),
    settings = defaults ++ Seq(
      libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
      parallelExecution in Test := false
    ),
    dependencies = Seq(src)
  )
}
