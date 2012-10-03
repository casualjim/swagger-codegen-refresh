organization := "com.wordnik"

name := "swagger-codegen"

version := "2.0.0-SNAPSHOT"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
  "com.ning"                          % "async-http-client"  % "1.7.6",
  "org.json4s"                       %% "json4s-jackson"     % "3.0.0-SNAPSHOT",
  "io.backchat.inflector"            %% "scala-inflector"    % "1.3.4",
  "io.backchat.rl"                   %% "rl"                 % "0.3.2",
  "eu.medsea.mimeutil"                % "mime-util"          % "2.1.3" exclude("org.slf4j", "slf4j-log4j12") exclude("log4j", "log4j"),
  "com.googlecode.juniversalchardet"  % "juniversalchardet"  % "1.0.3",
  "com.typesafe.akka"                 % "akka-actor"         % "2.0.3" // This akka dependency can be removed in scala 2.10
)

resolvers += "Wordnik Snapshots" at "https://ci.aws.wordnik.com/artifactory/m2-snapshots/"

resolvers += "Wordnik Releases" at "https://ci.aws.wordnik.com/artifactory/m2-releases/"

resolvers += "Wordnik Remote Repos" at "https://ci.aws.wordnik.com/artifactory/remote-repos/"

resolvers += "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

resolvers += "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/"
