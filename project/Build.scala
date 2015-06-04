import sbt._, Keys._
import pl.project13.scala.sbt.SbtJmh._
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
    val travis     = Try(sys.env("TRAVIS")).getOrElse("false") == "true"
    val pr         = Try(sys.env("TRAVIS_PULL_REQUEST")).getOrElse("false") != "false"
    val branch     = Try(sys.env("TRAVIS_BRANCH")).getOrElse("??")
    val snapshot   = version.value.trim.endsWith("SNAPSHOT")
    val jdk6       = System.getProperty("java.version").contains("1.6.")
    (travis, pr, branch, snapshot, jdk6) match {
      case (true, false, "master", true, true) => publish
      case _                                   => Def.task ()
    }
  }

  lazy val publishDefaults = defaults ++ Seq(
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

  lazy val noPublishDefaults = defaults ++ Seq(
    publishArtifact := false,
    packagedArtifacts := Map.empty,
    publish := {},
    publishLocal := {}
  )

  lazy val root = Project(
    "root",
    file("."),
    settings = noPublishDefaults,
    aggregate = Seq(core, macros)
  )

  lazy val core = Project(
    "scala-offheap",
    file("core"),
    settings = publishDefaults,
    dependencies = Seq(macros)
  )

  lazy val macros = Project(
    "scala-offheap-macros",
    file("macros"),
    settings = publishDefaults ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies ++= (
        if (!scalaVersion.value.startsWith("2.10")) Nil
        else List("org.scalamacros" %% "quasiquotes" % paradiseVersion)
      )
    )
  )

  lazy val sandbox = Project(
    "sandbox",
    file("sandbox"),
    settings = noPublishDefaults ++ Seq(
      incOptions := incOptions.value.withNameHashing(false),
      scalacOptions += "-Xprint:typer",
      fork in run := true,
      javaOptions in run ++= Seq("-Xms256m", "-Xmx256m")
    ),
    dependencies = Seq(macros, core)
  )

  lazy val tests = Project(
    "tests",
    file("tests"),
    settings = noPublishDefaults ++ Seq(
      libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
      libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test",
      incOptions := incOptions.value.withNameHashing(false),
      parallelExecution in Test := false,
      fork in Test := true
    ),
    dependencies = Seq(core, macros)
  )

  lazy val jmh = Project(
    "jmh",
    file("jmh"),
    settings = noPublishDefaults ++ jmhSettings,
    dependencies = Seq(core)
  )
}
