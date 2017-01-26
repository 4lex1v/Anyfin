/** Publish settings */
publishArtifact in Test     := false
publishMavenStyle           := true
pomIncludeRepository        := { _ => false }
licenses                    := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))
scmInfo                     := Some(ScmInfo(url("https://github.com/4lex1v/anyfin"), "scm:git:git@github.com:4lex1v/anyfin.git"))
homepage                    := Some(url("https://github.com/4lex1v/anyfin"))

/** SBT Release settings */
releaseIgnoreUntrackedFiles := true
releaseVersionBump          := sbtrelease.Version.Bump.Next
releaseCrossBuild           := true

