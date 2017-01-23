
// Release configuration
releaseIgnoreUntrackedFiles := true
releaseVersionBump          := sbtrelease.Version.Bump.Next
releaseCrossBuild           := true

// Publish configurations
publishArtifact in Test := false
publishMavenStyle := true

licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))

/** BINTRAY */
bintrayRepository := "snapshots"
