name := "scalac-chess-challenge"

version := "0.1"

scalaVersion := "2.12.8"
// https://mvnrepository.com/artifact/org.scalatest/scalatest
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0-SNAP10" % Test
// https://mvnrepository.com/artifact/org.scalacheck/scalacheck
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % Test

libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.3"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.8"
