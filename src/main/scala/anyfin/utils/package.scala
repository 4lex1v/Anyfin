package anyfin

import scala.meta._

package object utils {

  implicit class OptionFold[A](opt: Option[A]) {
    def fold[B](none: => B, some: A => B): B = {
      opt match {
        case None => none
        case Some(a) => some(a)
      }
    }
  }

}
