import sbt._, Keys._
import pl.project13.scala.sbt.SbtJmh._
import sbtassembly.Plugin._, AssemblyKeys._
import com.typesafe.sbt.pgp.PgpKeys._

object RegionsBuild extends Build {
  val paradiseVersion = "2.1.0-M5"
  lazy val defaults = Defaults.defaultSettings ++ Seq(
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.6",
    organization := "sh.den",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    initialCommands in console += "import offheap._; implicit val alloc = malloc",
    addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full),
    publishMavenStyle := true,
    publishOnlyWhenOnMaster := publishOnlyWhenOnMasterImpl.value,
    publishTo <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    pomIncludeRepository := { x => false },
    pomExtra := (
      <url>https://github.com/densh/scala-offheap</url>
      <inceptionYear>2014</inceptionYear>
      <licenses>
        <license>
          <name>BSD-like</name>
          <url>http://www.scala-lang.org/downloads/license.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git://github.com/densh/scala-offheap.git</url>
        <connection>scm:git:git://github.com/densh/scala-offheap.git</connection>
      </scm>
      <issueManagement>
        <system>GitHub</system>
        <url>https://github.com/densh/scala-offheap/issues</url>
      </issueManagement>
      <developers>
        <developer>
          <id>densh</id>
          <name>Denys Shabalin</name>
          <url>http://den.sh</url>
        </developer>
      </developers>
    )
  )

  // http://stackoverflow.com/questions/20665007/how-to-publish-only-when-on-master-branch-under-travis-and-sbt-0-13
  val publishOnlyWhenOnMaster = taskKey[Unit]("publish task for Travis (don't publish when building pull requests, only publish when the build is triggered by merge into master)")
  def publishOnlyWhenOnMasterImpl = Def.taskDyn {
    import scala.util.Try
    val travis   = Try(sys.env("TRAVIS")).getOrElse("false") == "true"
    val pr       = Try(sys.env("TRAVIS_PULL_REQUEST")).getOrElse("false") != "false"
    val branch   = Try(sys.env("TRAVIS_BRANCH")).getOrElse("??")
    val snapshot = version.value.trim.endsWith("SNAPSHOT")
    (travis, pr, branch, snapshot) match {
      case (true, false, "master", true) => publish
      case _                             => Def.task ()
    }
  }

  lazy val publishableDefaults = defaults ++ Seq(
    publishArtifact in Compile := true,
    publishArtifact in Test := false,
    credentials ++= {
      val mavenSettingsFile = System.getProperty("maven.settings.file")
      if (mavenSettingsFile != null) {
        println("Loading Sonatype credentials from " + mavenSettingsFile)
        try {
          import scala.xml._
          val settings = XML.loadFile(mavenSettingsFile)
          def readServerConfig(key: String) = (settings \\ "settings" \\ "servers" \\ "server" \\ key).head.text
          Some(Credentials(
            "Sonatype Nexus Repository Manager",
            "oss.sonatype.org",
            readServerConfig("username"),
            readServerConfig("password")
          ))
        } catch {
          case ex: Exception =>
            println("Failed to load Maven settings from " + mavenSettingsFile + ": " + ex)
            None
        }
      } else {
        for {
          realm <- sys.env.get("MAVEN_REALM")
          domain <- sys.env.get("MAVEN_DOMAIN")
          user <- sys.env.get("MAVEN_USER")
          password <- sys.env.get("MAVEN_PASSWORD")
        } yield {
          println("Loading Sonatype credentials from environment variables")
          Credentials(realm, domain, user, password)
        }
      }
    }.toList
  )

  lazy val mergeDependencies: Seq[sbt.Def.Setting[_]] = assemblySettings ++ Seq(
    test in assembly := {},
    logLevel in assembly := Level.Error,
    jarName in assembly := name.value + "_" + scalaVersion.value + "-" + version.value + "-assembly.jar",
    assemblyOption in assembly ~= { _.copy(includeScala = false) },
    Keys.`package` in Compile := {
      val slimJar = (Keys.`package` in Compile).value
      val fatJar = new File(crossTarget.value + "/" + (jarName in assembly).value)
      val _ = assembly.value
      IO.copy(List(fatJar -> slimJar), overwrite = true)
      slimJar
    },
    packagedArtifact in Compile in packageBin := {
      val temp = (packagedArtifact in Compile in packageBin).value
      val (art, slimJar) = temp
      val fatJar = new File(crossTarget.value + "/" + (jarName in assembly).value)
      val _ = assembly.value
      IO.copy(List(fatJar -> slimJar), overwrite = true)
      (art, slimJar)
    }
  )

  lazy val offheap = Project(
    "scala-offheap",
    file("."),
    settings = publishableDefaults ++ mergeDependencies,
    dependencies = Seq(core, macros)
  )

  lazy val core = Project(
    "core",
    file("core"),
    settings = defaults ++ Seq(
      packagedArtifacts := Map.empty
    ),
    dependencies = Seq(macros)
  )

  lazy val macros = Project(
    "macros",
    file("macros"),
    settings = defaults ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies ++= (
        if (!scalaVersion.value.startsWith("2.10")) Nil
        else List("org.scalamacros" %% "quasiquotes" % paradiseVersion)
      ),
      packagedArtifacts := Map.empty
    )
  )

  lazy val sandbox = Project(
    "sandbox",
    file("sandbox"),
    settings = defaults ++ Seq(
      incOptions := incOptions.value.withNameHashing(false),
      scalacOptions += "-Xprint:typer",
      fork in run := true,
      javaOptions in run ++= Seq("-Xms256m", "-Xmx256m"),
      packagedArtifacts := Map.empty
    ),
    dependencies = Seq(macros, core)
  )

  lazy val tests = Project(
    "tests",
    file("tests"),
    settings = defaults ++ Seq(
      libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
      libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test",
      incOptions := incOptions.value.withNameHashing(false),
      parallelExecution in Test := false,
      fork in Test := true,
      packagedArtifacts := Map.empty
    ),
    dependencies = Seq(core, macros)
  )

  lazy val jmh = Project(
    "jmh",
    file("jmh"),
    settings = defaults ++ jmhSettings ++ Seq(packagedArtifacts := Map.empty),
    dependencies = Seq(core)
  )
}
