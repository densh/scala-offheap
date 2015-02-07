import sbt._, Keys._

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
    settings = defaults ++ Seq(
      incOptions := incOptions.value.withNameHashing(false),
      scalacOptions += "-Xprint:typer",
      //scalacOptions += "-Xprint-types",
      //scalacOptions += "-uniqid",
      fork in run := true,
      javaOptions in run += "-Dcom.sun.management.jmxremote.port=3333",
      javaOptions in run += "-Dcom.sun.management.jmxremote.ssl=false",
      javaOptions in run += "-Dcom.sun.management.jmxremote.authenticate=false"
      //javaOptions in run += "-agentpath:/Applications/YourKit.app/Contents/Resources/bin/mac/libyjpagent.jnilib"
    ),
    dependencies = Seq(src)
  )

  lazy val tests = Project(
    "tests",
    file("tests"),
    settings = defaults ++ Seq(
      libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test",
      incOptions := incOptions.value.withNameHashing(false),
      parallelExecution in Test := false,
      fork in Test := true
      //scalacOptions += "-Xprint:typer"
    ),
    dependencies = Seq(src)
  )

  lazy val benchmarks = Project(
    "benchmarks",
    file("benchmarks"),
    settings = defaults ++ Seq(
      libraryDependencies += "com.github.axel22" %% "scalameter" % "0.5-M2",
      testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
      incOptions := incOptions.value.withNameHashing(false),
      parallelExecution in Test := false,
      fork in Test := true,
      logBuffered := false,
      javaOptions in Test ++= Seq("-Xms64m", "-Xmx64m")
      //scalacOptions += "-Xprint:typer"
    ),
    dependencies = Seq(src)
  )
}
