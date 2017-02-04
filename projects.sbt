import de.heikoseeberger.sbtheader.license.Apache2_0

lazy val Anyfin = project.in(file(".")).
  enablePlugins(AutomateHeaderPlugin).
  settings(
    name         := "Anyfin",
    organization := "io.github.4lex1v",
    description  := "Opinionated way to improving FP experience with Metaprogramming",
    headers      := Map("scala" -> Apache2_0("2017", "Aleksandr Ivanov")),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalameta" % "1.4.0",
      "org.scalatest" %% "scalatest" % "3.0.1" % "test"
    ),
    mimaPreviousArtifacts := Set(
      organization.value %% "anyfin" % "0.1.1"
    )
  )

lazy val Examples = project.in(file("./Examples")).dependsOn(Anyfin)

