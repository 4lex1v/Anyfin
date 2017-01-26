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

