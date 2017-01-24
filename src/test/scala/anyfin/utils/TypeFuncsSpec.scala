package anyfin.utils

import org.scalatest.{Matchers, WordSpec}
import anyfin.Equalities._
import anyfin.utils.TypeFuncs._
import scala.meta._

class TypeFuncsSpec extends WordSpec with Matchers {
  "deconstructType" should {
    "return the type itself" in {
      deconstructType(t"String") shouldEqual List(t"String")
    }

    "deconstruct into type-constructor with its arguments" in {
      deconstructType(t"Option[String]") shouldEqual List(t"Option[_]", t"String")
      deconstructType(t"Either[Int, String]") shouldEqual List(t"Either[_, _]", t"Int", t"String")
    }

    "deconstruct tuple types" in {
      val types = List(t"Map[_, _]", t"String", t"List[_]", t"Int", t"B")
      deconstructType(t"(Map[String, List[Int]], B)") shouldEqual types
    }
  }

}
