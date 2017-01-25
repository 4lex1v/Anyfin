import scala.Console.{GREEN, RESET}

name        := "Anyfin"
description := "Improving FP experience with Metaprogramming"

scalaVersion := "2.11.8"
organization := "org.typelevel"

resolvers += Resolver.url("scalameta", url("http://dl.bintray.com/scalameta/maven"))(Resolver.ivyStylePatterns)
libraryDependencies ++= Seq(
  compilerPlugin("org.scalameta" %% "paradise" % "3.0.0.152" cross CrossVersion.full)
)

scalacOptions ++= Seq(
  "-language:_",
  "-target:jvm-1.8",
  "-Xplugin-require:macroparadise"
)

shellPrompt := { s => s"$GREEN[${Project.extract(s).currentProject.id}]$RESET >> " }
