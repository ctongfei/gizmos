name := "poly-collection"
version := "0.0.4-SNAPSHOT"
isSnapshot := true
organization := "me.tongfei"
scalaVersion := "2.11.8"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "me.tongfei"        %% "poly-algebra"    % "0.3.2-SNAPSHOT"

libraryDependencies += "org.scalatest"     %% "scalatest"       % "2.2.6"           % Test
libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.7"             % Test

scalacOptions += "-Ymacro-debug-lite"
scalacOptions in doc += "-diagrams; -deprecation"

publishMavenStyle := true
publishArtifact in Test := false
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

val docSettings = Seq(
  autoAPIMappings := true,
  apiMappings ++= {
    val cp = (fullClasspath in Compile).value
    def findJar(name: String): File = {
      val regex = ("/" + name + "[^/]*.jar$").r
      cp.find(jar => regex.findFirstIn(jar.data.toString).nonEmpty).get.data
    }
    Map(
      findJar("scala-library") → url("http://scala-lang.org/api/" + version + "/")
    )
  }
)

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

