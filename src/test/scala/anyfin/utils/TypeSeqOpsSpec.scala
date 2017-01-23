package anyfin.utils

import org.scalatest._
import TypeSeqOps._
import anyfin.Equalities._
import scala.meta._

class TypeSeqOpsSpec extends WordSpec with Matchers {

  "removeDuplicates" should {

    "return the same list" in {
      val list = List(t"F[_]", t"Either[_, _]", t"A")
      removeDuplicates(list) shouldEqual list
    }

    "remove duplicates" in {
      val list = List(t"A", t"F[_]", t"B", t"F[_]", t"B")
      removeDuplicates(list) shouldEqual List(t"A", t"F[_]", t"B")
    }

  }

  "diffTypes" should {

    "return original list" in {
      diffTypes(List(t"A", t"B"), List.empty) shouldEqual List(t"A", t"B")
    }

    "return empty list" in {
      diffTypes(List.empty, List(t"A")) shouldEqual List.empty
    }

    "return proper diff" in {
      diffTypes(List(t"A", t"B"), List(t"B")) shouldEqual List(t"A")
    }

    "diff type constructor" in {
      diffTypes(List(t"Either[_, _]", t"Option[_]"), List(t"Either[_, _]")) shouldEqual List(t"Option[_]")
    }

  }


}
