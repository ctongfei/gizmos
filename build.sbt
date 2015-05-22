name := "poly-collection"

version := "0.1.0-SNAPSHOT"

isSnapshot := true

organization := "me.tongfei"

scalaVersion := "2.11.4"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "me.tongfei" %% "poly-algebra" % "0.2.0-SNAPSHOT"
