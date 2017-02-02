package examples

import anyfin.data._

@dataType trait SystemCall[A] {
  def PrintLine (value: String): SystemCall[Unit]
  def ReadLine: SystemCall[String]
}

object SystemCall {
  @constr def Signal (process: Int, signal: Int): SystemCall[Unit]
}
