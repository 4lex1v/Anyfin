
/** Make definitions under `anyfin.internal` available in console */
fullClasspath in (Compile, console) += file("./console.scala")
initialCommands in console          := "import anyfin.internal.console._"

excludeFilter := new ExactFilter("console.scala")