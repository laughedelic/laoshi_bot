name := "laoshi_bot"
organization := "laughedelic"

scalaVersion := "2.12.1"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-Xlint:_"
)

// resolvers += "jitpack" at "https://jitpack.io"
// libraryDependencies += "com.github.mukel" % "telegrambot4s" % "v2.0.1"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "com.github.pathikrit" %% "better-files"     %  "2.17.1",         // This is temporary dependency for persisting the auth info
  "com.hankcs"           %  "hanlp"            %  "portable-1.3.2", // Chinese language processing
  // FIXME: use stable version
  "info.mukel"           %% "telegrambot4s"    %  "2.1.0-SNAPSHOT", // The Telegram bot API
  "com.typesafe.akka"    %% "akka-http"        %  "10.0.4",         // telegrambot4s already depends on akka, updating the version
  // "de.heikoseeberger"    %% "akka-http-json4s" %  "1.12.0",         // JSON (un)marshalling for Skritter REST API
  "ch.qos.logback"       %  "logback-classic"  %  "1.1.7"           // Basic logging
)

// resolvers += "DynamoDBLocal" at "https://s3.eu-central-1.amazonaws.com/dynamodb-local-frankfurt/release"
// libraryDependencies += "com.amazonaws" % "DynamoDBLocal" % "1.11.0.1"
