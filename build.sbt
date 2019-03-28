import sbt._, Keys._
import com.typesafe.sbt.pgp.PgpKeys._
import pl.project13.scala.sbt.JmhPlugin

val paradiseVersion = "2.1.1"
lazy val defaults = Seq(
  version := "0.2-SNAPSHOT",
  scalaVersion := "2.12.8",
  crossScalaVersions := Seq("2.12.8", "2.11.12"),
  organization := "sh.den",
  resolvers += Resolver.sonatypeRepo("snapshots"),
  resolvers += Resolver.sonatypeRepo("releases"),
  initialCommands in console += "import scala.offheap._; implicit val alloc = malloc",
  addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full),
  publishMavenStyle := true,
  publishOnlyWhenOnMaster := publishOnlyWhenOnMasterImpl.value,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots") 
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
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
  val jdk7       = System.getProperty("java.version").contains("1.7.")
  (travis, pr, branch, snapshot, jdk7) match {
    case (true, false, "master", true, true) => publish
    case _                                   => Def.task((): Unit)
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

lazy val root = (project in file("."))
  .settings(noPublishDefaults)
  .aggregate(core, macros)

lazy val core = (project in file("core"))
  .settings(
    publishDefaults,
    name := "scala-offheap"
  )
  .dependsOn(macros)

lazy val macros = (project in file("macros"))
  .settings(
    name := "scala-offheap-macros",
    publishDefaults,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies ++= (
      if (!scalaVersion.value.startsWith("2.10")) Nil
      else List("org.scalamacros" %% "quasiquotes" % paradiseVersion)
    )
  )

lazy val sandbox = (project in file("sandbox"))
  .settings(
    noPublishDefaults,
    scalacOptions += "-Xprint:typer",
    fork in run := true,
    javaOptions in run ++= Seq("-Xms256m", "-Xmx256m")
  )
  .dependsOn(core, macros)

lazy val tests = (project in file("tests"))
  .settings(
    noPublishDefaults,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
    parallelExecution in Test := false,
    fork in Test := true
  )
  .dependsOn(core, macros)

lazy val jmh = (project in file("jmh"))
  .settings(noPublishDefaults)
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
