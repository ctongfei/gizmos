lazy val commonSettings = Seq(
  organization := "me.tongfei",
  isSnapshot := true,
  scalaVersion := "2.11.11",
  crossScalaVersions := Seq("2.11.11", "2.12.2"),

  resolvers += Resolver.sonatypeRepo("snapshots"),

  libraryDependencies ++= Seq(
    "org.typelevel"     %% "cats-core"       % "0.9.0",
    "org.typelevel"     %% "algebra"         % "0.6.0",
    "org.typelevel"     %% "spire"           % "0.14.1",
    "me.tongfei"        %% "poly-macroutil"  % "0.2.0",
    "org.scalatest"     %% "scalatest"       % "3.0.0"  % Test,
    "org.scalacheck"    %% "scalacheck"      % "1.13.4" % Test,
    "com.storm-enroute" %% "scalameter-core" % "0.8.2"  % Test
  ),
  scalacOptions ++= Seq("-deprecation", "-feature"),
  scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-Ymacro-debug-lite"),

  publishMavenStyle := true,
  publishArtifact in Test := false,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },

  pomExtra :=
    <url>http://github.com/ctongfei/poly-collection</url>
      <licenses>
        <license>
          <name>MIT</name>
          <url>http://opensource.org/licenses/MIT</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:ctongfei/poly-collection.git</url>
        <connection>scm:git:git@github.com:ctongfei/poly-collection.git</connection>
      </scm>
      <developers>
        <developer>
          <id>ctongfei</id>
          <name>Tongfei Chen</name>
          <url>http://tongfei.me/</url>
        </developer>
      </developers>
)

lazy val core = (project in file("core")).settings(commonSettings: _*).settings(
  name := "poly-collection-core",
  version := "0.0.10-SNAPSHOT"
)

lazy val rangequery = (project in file("rangequery")).settings(commonSettings: _*)
  .dependsOn(core).settings(
  name := "poly-collection-rangequery",
  version := "0.0.1-SNAPSHOT"
)

lazy val interval = (project in file("interval")).settings(commonSettings: _*)
  .dependsOn(core).settings(
  name := "poly-collection-interval",
  version := "0.0.1-SNAPSHOT"
)

lazy val transient = (project in file("transient")).settings(commonSettings: _*)
  .dependsOn(core).settings(
  name := "poly-collection-transient",
  version := "0.0.1-SNAPSHOT"
)

lazy val approx = (project in file("approx")).settings(commonSettings: _*)
  .dependsOn(core).settings(
  name := "poly-collection-approx",
  version := "0.0.1-SNAPSHOT"
)


val root = (project in file(".")).
  settings(commonSettings: _*).
  enablePlugins(ScalaUnidocPlugin).
  settings(
    name := "poly-collection"
  ).
  aggregate(core, rangequery, interval, transient, approx)
