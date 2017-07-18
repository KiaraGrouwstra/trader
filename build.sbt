import Dependencies._

resolvers ++= Seq("releases", "snapshots")
              .map(Resolver.sonatypeRepo(_))

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case PathList("reference.conf") => MergeStrategy.concat
  case x => MergeStrategy.first
}

libraryDependencies ++=
Seq("core", "generic", "parser") //, "yaml"
.map(s => "io.circe" %% s"circe-$s" % "0.8.0") ++
Seq("actor" /*, "testkit", "stream", "persistence",
  "cluster", "cluster-sharding", "distributed-data"*/)
.map(s => "com.typesafe.akka" %% s"akka-$s" % "2.4.19") ++ // 2.5.3
// Seq("core") // , "macro", "law"
// .map(s => "com.github.julien-truffaut" %% s"monocle-$s" % "1.4.0") ++
Seq("core") // , "examples"
.map(s => "org.knowm.xchange" % s"xchange-$s" % "4.2.0") ++
Seq(
  "commons-io" % "commons-io" % "2.5",
  "com.typesafe.akka" %% "akka-http" % "10.0.9",
  "io.circe" %% "circe-yaml" % "0.6.1",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.0",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  // "org.scalatest" %% "scalatest" % "3.0.1",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "net.ruippeixotog" %% "scala-scraper" % "2.0.0-RC2" // -SNAPSHOT
)

lazy val root = (project in file("."))
  .settings(net.virtualvoid.sbt.graph.Plugin.graphSettings:_*)
  .settings(
    name := "Trader",
    // libraryDependencies ++= ...,
    inThisBuild(List(
      organization := "com.tgs",
      scalaVersion := "2.12.2",
      version      := "0.1.0-SNAPSHOT"
    ))
  )

mainClass in Compile := Some(
  // "trader.TestXchange"
  "trader.CryptoPingScraper"
)
