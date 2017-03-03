name := "laoshi_bot"
organization := "laughedelic"

scalaVersion := "2.12.1"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  // "info.mukel" %% "telegrambot4s" % "2.0.1",
  "info.mukel" %% "telegrambot4s" % "2.1.0-SNAPSHOT",

  "com.typesafe.akka" %% "akka-http" % "10.0.4",
  "com.github.pathikrit" %% "better-files" % "2.17.1",

  "com.hankcs" % "hanlp" % "portable-1.3.2",

  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
  "ch.qos.logback" % "logback-classic" % "1.1.7"
)
