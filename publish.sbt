
// Release configuration
releaseIgnoreUntrackedFiles := true
releaseVersionBump := sbtrelease.Version.Bump.Next

// Publish configurations
publishArtifact in Test := false
publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}