package examples

import anyfin.data._

sealed trait SystemCall[A]

object SystemCall {
  @constr def ReadLine: SystemCall[String]
  @constr def PrintLine (value: String): SystemCall[Unit]
  @constr def Signal (process: Int, signal: Int): SystemCall[Unit]
}
