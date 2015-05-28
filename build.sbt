name := "poly-collection"

version := "0.1.0-SNAPSHOT"

isSnapshot := true

organization := "me.tongfei"

scalaVersion := "2.11.6"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "me.tongfei" %% "poly-algebra" % "0.2.0-SNAPSHOT"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.2"
