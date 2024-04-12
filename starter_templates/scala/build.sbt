ThisBuild / scalaVersion     := "2.13.12"

assembly / assemblyJarName := "redis.jar"

lazy val root = (project in file("."))
  .settings(
    name := "codecrafter-redis"
  )
