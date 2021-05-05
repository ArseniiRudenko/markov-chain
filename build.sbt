
name := "markov-chain"
organization := "work.arudenko"
version := "0.1-SNAPSHOT"

scalaVersion := "2.13.5"

val circeVersion = "0.12.3"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)



libraryDependencies ++= Seq(
"org.scalanlp" %% "breeze" % "1.1",
// Native libraries are not included by default. add this if you want them
// Native libraries greatly improve performance, but increase jar sizes.
// It also packages various blas implementations, which have licenses that may or may not
// be compatible with the Apache License. No GPL code, as best I know.
"org.scalanlp" %% "breeze-natives" % "1.1",

// The visualization library is distributed separately as well.
// It depends on LGPL code
"org.scalanlp" %% "breeze-viz" % "1.1",

"edu.stanford.nlp" % "stanford-corenlp" % "4.0.0",
"com.github.pathikrit" %% "better-files" % "3.9.1",
"com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
"ch.qos.logback" % "logback-classic" % "1.2.3" ,
"org.scalatest" %% "scalatest" % "3.2.0" % Test

)

Compile / PB.targets := Seq(
  scalapb.gen() -> (Compile / sourceManaged).value / "scalapb"
)

// (optional) If you need scalapb/scalapb.proto or anything from
// google/protobuf/*.proto
libraryDependencies ++= Seq(
  "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"
)