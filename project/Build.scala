import sbt._, Keys._
import pl.project13.scala.sbt.SbtJmh._

object RegionsBuild extends Build {
  val paradiseVersion = "2.1.0-M5"
  val defaults = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.11.4",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
  )

  lazy val root = Project(
    "root",
    file("."),
    settings = defaults,
    aggregate = Seq(core, macros, annotations)
  )

  lazy val core = Project(
    "core",
    file("core"),
    settings = defaults,
    dependencies = Seq(macros, annotations)
  )

  lazy val annotations = Project(
    "annotations",
    file("annotations"),
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
    settings = defaults ++ Seq(
      incOptions := incOptions.value.withNameHashing(false),
      //scalacOptions += "-Xprint:typer",
      //scalacOptions += "-Xprint-types",
      //scalacOptions += "-uniqid",
      fork in run := true,
      javaOptions in run ++= Seq("-Xms64m", "-Xmx64m"),
      //javaOptions in run += "-agentpath:/Applications/YourKit.app/Contents/Resources/bin/mac/libyjpagent.jnilib"
      javaOptions in run += "-agentpath:/home/denys/.bin/yjp.d/bin/linux-x86-64/libyjpagent.so=delay=10000"
    ),
    dependencies = Seq(core)
  )

  lazy val tests = Project(
    "tests",
    file("tests"),
    settings = defaults ++ Seq(
      //scalacOptions += "-Xprint:typer",
      libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test",
      incOptions := incOptions.value.withNameHashing(false),
      parallelExecution in Test := false,
      fork in Test := true
    ),
    dependencies = Seq(core)
  )

  lazy val jmh = Project(
    "jmh",
    file("jmh"),
    settings = defaults ++ jmhSettings,
    dependencies = Seq(core)
  )
}
