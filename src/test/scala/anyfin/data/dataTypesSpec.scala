package anyfin.data

import org.scalatest._

import scala.meta._

class dataTypeSpec extends WordSpec with Matchers {

  "parseAnnotationMode" should {
    "return 'true' by default" in {
      dataType.parseAnnotationMode(q"new dataType()") shouldEqual true
    }

    "return literal value" in {
      dataType.parseAnnotationMode(q"new dataType(true)") shouldEqual true
      dataType.parseAnnotationMode(q"new dataType(false)") shouldEqual false
    }

    "return named literal" in {
      dataType.parseAnnotationMode(q"new dataType(strict = true)") shouldEqual true
      dataType.parseAnnotationMode(q"new dataType(strict = false)") shouldEqual false
    }
  }

}
