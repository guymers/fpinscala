scalaVersion := "2.11.7"

// https://tpolecat.github.io/2014/04/11/scalac-flags.html
scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-unchecked",
  "-target:jvm-1.8",
  "-Xfatal-warnings",
  "-Xfuture",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard"
)

conflictManager := ConflictManager.strict

dependencyOverrides <+= scalaVersion { "org.scala-lang" % "scala-library" % _ }
dependencyOverrides <+= scalaVersion { "org.scala-lang" % "scala-reflect" % _ }
dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "1.0.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.5" % "test"
