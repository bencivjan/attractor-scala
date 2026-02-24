ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.5.2"
ThisBuild / organization := "ai.attractor"

val http4sVersion = "0.23.30"
val circeVersion = "0.14.10"
val catsEffectVersion = "3.5.7"
val fs2Version = "3.11.0"
val munitVersion = "1.0.3"
val munitCEVersion = "2.0.0"
val declineVersion = "2.4.1"

lazy val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect" % catsEffectVersion,
    "co.fs2" %% "fs2-core" % fs2Version,
    "co.fs2" %% "fs2-io" % fs2Version,
    "io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion,
    "org.scalameta" %% "munit" % munitVersion % Test,
    "org.typelevel" %% "munit-cats-effect" % munitCEVersion % Test
  ),
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xfatal-warnings"
  )
)

lazy val root = (project in file("."))
  .aggregate(unifiedLlm, codingAgent, attractor)
  .settings(
    name := "scala-attractor",
    publish / skip := true
  )

lazy val unifiedLlm = (project in file("unified-llm"))
  .settings(
    name := "unified-llm",
    commonSettings,
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-client" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,
      "org.http4s" %% "http4s-ember-client" % http4sVersion,
      "org.http4s" %% "http4s-dsl" % http4sVersion
    )
  )

lazy val codingAgent = (project in file("coding-agent"))
  .dependsOn(unifiedLlm)
  .settings(
    name := "coding-agent",
    commonSettings
  )

lazy val attractor = (project in file("attractor"))
  .dependsOn(codingAgent, unifiedLlm)
  .settings(
    name := "attractor",
    commonSettings,
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-ember-server" % http4sVersion,
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "com.monovore" %% "decline-effect" % declineVersion
    ),
    Compile / mainClass := Some("ai.attractor.pipeline.cli.Main"),
    assembly / mainClass := Some("ai.attractor.pipeline.cli.Main"),
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case "module-info.class"           => MergeStrategy.discard
      case x                             => MergeStrategy.first
    }
  )
