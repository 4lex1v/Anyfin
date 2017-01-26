package examples

import anyfin.data._

sealed trait SystemCall[A]

object SystemCall {
  @constr def PrintLine (value: String): SystemCall[Unit]
  @constr def ReadLine: SystemCall[String]
}
