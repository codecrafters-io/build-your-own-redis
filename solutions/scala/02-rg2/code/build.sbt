ThisBuild / scalaVersion     := "3.7.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.codecrafters"
ThisBuild / organizationName := "CodeCrafters"

assembly / assemblyJarName := "redis.jar"

lazy val root = (project in file("."))
  .settings(
    name := "codecrafter-redis",
    // List your dependencies here
    libraryDependencies ++= Seq(
    )
  )
