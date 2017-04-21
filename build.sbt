lazy val commonSettings = Seq(
  organization := "me.tongfei",
  isSnapshot := true,
  version := "0.0.10-SNAPSHOT",
  scalaVersion := "2.11.11",
  crossScalaVersions := Seq("2.11.11", "2.12.2"),

  libraryDependencies ++= Seq(
    "me.tongfei"        %% "poly-algebra"    % "0.4.0",
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
  name := "poly-collection-core"
)

lazy val rangequery = (project in file("rangequery")).settings(commonSettings: _*)
  .dependsOn(core).settings(
  name := "poly-collection-rangequery"
)

lazy val approx = (project in file("approx")).settings(commonSettings: _*)
  .dependsOn(core).settings(
  name := "poly-collection-approx"
)


