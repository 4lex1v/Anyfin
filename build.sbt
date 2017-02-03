import de.heikoseeberger.sbtheader.license.Apache2_0
import scala.Console.{GREEN, RESET}

inThisBuild(Seq(
  /** Scala */
  scalaVersion      := "2.11.8",
  scalaOrganization := "org.typelevel",

  /** Compiler stuff */
  resolvers += Resolver.url("scalameta", url("http://dl.bintray.com/scalameta/maven"))(Resolver.ivyStylePatterns),
  addCompilerPlugin("org.scalameta" %% "paradise" % "3.0.0.152" cross CrossVersion.full),
  scalacOptions ++= Seq("-language:_", "-Xplugin-require:macroparadise"),

  /** SBT Experience & Environment */
  shellPrompt := { s => s"$GREEN[${Project.extract(s).currentProject.id}]$RESET >> " }
))

lazy val Anyfin = project.in(file(".")).
  enablePlugins(AutomateHeaderPlugin).
  settings(
    name         := "Anyfin",
    description  := "Opinionated way to improving FP experience with Metaprogramming",

    organization := "io.github.4lex1v",

    headers      := Map("scala" -> Apache2_0("2017", "Aleksandr Ivanov")),

    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalameta" % "1.4.0",
      "org.scalatest" %% "scalatest" % "3.0.1" % "test"
    ),

    /** Publish settings */
    publishArtifact in Test := false,
    publishMavenStyle       := true,
    pomIncludeRepository    := { _ => false },
    licenses                := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
    scmInfo                 := Some(ScmInfo(url("https://github.com/4lex1v/anyfin"), "scm:git:git@github.com:4lex1v/anyfin.git")),
    homepage                := Some(url("https://github.com/4lex1v/anyfin")),

    /** SBT Release settings */
    releaseIgnoreUntrackedFiles := true,
    releaseVersionBump          := sbtrelease.Version.Bump.Next,
    releaseCrossBuild           := true
  )

lazy val Examples = project.in(file("./Examples")).dependsOn(Anyfin)
