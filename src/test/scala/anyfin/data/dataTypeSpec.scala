package anyfin.data

import org.scalatest.{Matchers, WordSpec}

import scala.meta._
import scala.collection.immutable.Seq
import scala.meta.internal.inline.AbortException

class dataTypeSpec extends WordSpec with Matchers {

  "parseAnnotationMode" should {
    import dataType.{parseAnnotationMode => parse}
    "default to 'true' if no parameters provided" in {
      parse(q"new dataType()") shouldBe true
    }

    "support literal parameters" in {
      parse(q"new dataType(true)") shouldBe true
      parse(q"new dataType(false)") shouldBe false
    }

    "support named parameters" in {
      parse(q"new dataType(strict = false)") shouldBe false
      parse(q"new dataType(strict = true)") shouldBe true
    }
  }

  "extractDataConstructors" when {
    val empty = (Seq.empty[Stat], Seq.empty[Decl.Def])

    def withEmptyBody(stats: Stat*) = (Seq.empty[Stat], Seq(stats: _*))
    def withNoConstructors(stats: Stat*) = (Seq(stats: _*), Seq.empty[Decl.Def])

    "in `strict` mode" should {
      def extract(body: Stat*) =
        dataType.extractDataConstructors(Seq(body: _*), isInStrictMode = true)

      "support empty body" in {
        extract() shouldBe empty
      }

      "treat 'def' declarations as data constructors" in {
        val defDecl = q"def Print(s: String): SystemCall[Unit]"
        extract(defDecl) shouldBe withEmptyBody(defDecl)
      }

      "treat 'def' declarations annotated with '@constr' as data constructors" in {
        val defDecl    = q"def Read: SystemCall[String]"
        val annDefDecl = q"@constr def Print(s: String): SystemCall[Unit]"

        extract(defDecl, annDefDecl) shouldBe withEmptyBody(defDecl, annDefDecl)
      }

      "not support other stats" in {
        val valDef = q"val number = 42"
        an [AbortException] should be thrownBy extract(valDef)
      }

    }

    "in `non-strict` mode" should {
      def extract(body: Stat*) =
        dataType.extractDataConstructors(Seq(body: _*), isInStrictMode = false)

      "support empty body" in {
        extract() shouldBe empty
      }

      "treat 'def' declarations annotated with '@constr' as data constructors" in {
        val annDefDecl = q"@constr def Print (s: String): SystemCall[Unit]"
        extract(annDefDecl) shouldBe withEmptyBody(annDefDecl)
      }

      "not support abstract functions" in {
        val defDecl = q"def Print (s: String): SystemCall[Unit]"
        an [AbortException] should be thrownBy extract(defDecl)
      }

      "support other stats" in {
        val valDef = q"val number = 42"
        val absValDef = q"val check: Boolean"
        val innerClass = q"class Inner { class Foo }"
        extract(valDef, absValDef, innerClass) shouldBe withNoConstructors(valDef, absValDef, innerClass)
      }
    }
  }

}
