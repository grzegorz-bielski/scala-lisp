import Dependencies._

ThisBuild / scalaVersion := "2.13.2"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

// scalacOptions += "-Ypartial-unification"

lazy val root = (project in file("."))
  .settings(
    name := "scala-lisp",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.tpolecat" %% "atto-core" % "0.7.0",
    libraryDependencies += "org.tpolecat" %% "atto-refined" % "0.7.0",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "2.1.3",
    libraryDependencies += "org.typelevel" %% "cats-mtl-core" % "0.7.0"
    // libraryDependencies += "org.typelevel" %% "kittens" % "2.1.0"
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
