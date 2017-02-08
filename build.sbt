import de.heikoseeberger.sbtheader.license.Apache2_0
import scala.Console.{GREEN, RESET}

inThisBuild(Seq(
  /** Scala */
  scalaVersion      := "2.11.8",
  scalaOrganization := "org.typelevel",

  /** Compiler stuff */
  resolvers += Resolver.bintrayIvyRepo("scalameta", "maven"),

  scalacOptions += "-language:_",

  /** SBT Experience & Environment */
  shellPrompt := { s => s"$GREEN[${Project.extract(s).currentProject.id}]$RESET >> " }
))

lazy val Internal = project.
  in(file("./Internal")).
  enablePlugins(AutomateHeaderPlugin).
  settings(
    headers := Map("scala" -> Apache2_0("2017", "Aleksandr Ivanov")),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalameta" % "1.4.0",
      "org.scalatest" %% "scalatest" % "3.0.1" % Test
    )
  )

lazy val Anyfin = project.
  in(file(".")).
  dependsOn(Internal).
  settings(
    name         := "Anyfin",
    description  := "Empowering Scala with Metaprogramming",
    licenses     := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
    startYear    := Option(2017),
    scmInfo      := Some(ScmInfo(url("https://github.com/4lex1v/anyfin"), "scm:git:git@github.com:4lex1v/anyfin.git")),
    homepage     := Some(url("https://github.com/4lex1v/anyfin")),
    organization := "io.github.4lex1v",

    libraryDependencies += compilerPlugin("org.scalameta" %% "paradise" % "3.0.0.152" cross CrossVersion.full),
    scalacOptions       += "-Xplugin-require:macroparadise",
    scalacOptions in (Compile, console) := Seq("-Yrepl-class-based"),

    /** Publish settings */
    publishArtifact in Test := false,
    publishMavenStyle       := true,
    pomIncludeRepository    := { _ => false },

    /** SBT Release settings */
    releaseIgnoreUntrackedFiles := true,
    releaseVersionBump          := sbtrelease.Version.Bump.Next,
    releaseCrossBuild           := true
  )
