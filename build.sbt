name := "TradeTracker"

version := "0.1.0"

scalaVersion := "2.11.7"

//resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4-SNAPSHOT"
  
resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies += "com.github.melrief" %% "purecsv" % "0.0.6"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"
