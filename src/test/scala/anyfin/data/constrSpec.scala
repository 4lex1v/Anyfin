package anyfin.data

import org.scalatest._

import scala.meta._
import anyfin.Equalities._

import scala.meta.internal.inline.AbortException

class constrSpec extends WordSpec with Matchers {

  "inferDataType" should {
    "refine applied type-constructor into data type" in {
      constr.inferDataType(t"Option[String]") shouldEqual ctor"Option[String]"
    }

    "refine simple type into data type" in {
      constr.inferDataType(t"String") shouldEqual ctor"String()"
    }

    "not compile" in {
      intercept[AbortException] {
        constr.inferDataType(t"(Future[String], Int)")
      }
    }
  }

}
