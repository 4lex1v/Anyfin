package anyfin.internal
package data

import org.scalatest._

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.internal.inline.AbortException
import Equalities._

class DataConstructorSpec extends WordSpec with Matchers {
  import DataConstructors._

  "DataType inference" should {
    import DataTypeInference._

    "refine simple type into data type" in {
      inferDataType(t"String") shouldEqual ctor"String()"
    }

    "refine applied type-constructor into data type" in {
      inferDataType(t"Option[String]") shouldEqual ctor"Option[String]"
    }

    "not compile tuple types" in {
      intercept[AbortException] {
        inferDataType(t"(Future[String], Int)")
      }
    }
  }

  "ShapeConstructor" when {
    import ShapeConstructor._

    val dataType = DataTypeInference.inferDataType(t"SystemCall[Frame]")

    "calls 'liftConstructor'" should {
      "always fix parameterless constructor" in {
        liftConstructor(Seq.empty) shouldEqual Seq(Seq.empty)
      }

      "set 'val' modifier for single param list constructor" in {
        val input = Seq(Seq(param"name: String", param"age: int"))
        val output = Seq(Seq(param"val name: String", param"val age: int"))
        liftConstructor(input) shouldEqual output
      }

      "set 'val' modifier only for the first param list" in {
        val input = Seq(Seq(param"name: String"), Seq(param"age: int"))
        val output = Seq(Seq(param"val name: String"), Seq(param"age: int"))
        liftConstructor(input) shouldEqual output
      }
    }

    "calls 'genDataShape'" should {
      "generate 'object' structure" in {
        val shape = genDataShape("Test", Seq.empty, Seq.empty, dataType)
        shape shouldEqual q"private object Test extends $dataType {}"
      }

      "generate parameterless 'class' structure" in {
        val shape = genDataShape("Test", Seq(tparam"A"), Seq.empty, dataType)
        shape shouldEqual q"private final class Test[A]() extends $dataType {}"
      }

      "generate 'class' structure with public fields" in {
        val shape = genDataShape("Test", Seq.empty, Seq(Seq(param"app: Application")), dataType)
        shape shouldEqual q"private final class Test(val app: Application) extends $dataType {}"
      }

      "generate 'class' structure with multiple param lists, where first one is public and implicit param list support" in {
        val paramss = Seq(Seq(param"app: Application"), Seq(param"foo: Foo"), Seq(param"implicit S: SystemCall :<: S"))
        val shape = genDataShape("Test", Seq.empty, paramss, dataType)
        shape shouldEqual q"private final class Test(val app: Application)(foo: Foo)(implicit S: SystemCall :<: S) extends $dataType {}"
      }

      "generate 'class' structure" in {
        val shape = genDataShape("Test", Seq(tparam"A"), Seq(Seq(param"value: A")), dataType)
        shape shouldEqual q"private final class Test[A](val value: A) extends $dataType {}"
      }
    }
  }

  "ApplyGenerator" should {
    import ApplyGenerator._

    val cls = q"class Snapshot(app: Application) extends SystemCall[Frame] {}"

    "construct parameterless 'apply' for the 'object' structure" in {
      val constr = generateApply(q"object Signal extends Contract[Int] {}")
      constr shouldEqual q"def apply(): Contract[Int] = Signal"
    }

    "construct 'apply' for the 'class' structure" in {
      val constr = generateApply(q"class Snapshot(val app: Application) extends SystemCall[Frame] {}")
      constr shouldEqual q"def apply(app: Application): SystemCall[Frame] = new Snapshot(app)"
    }

    "require empty param list" in {
      intercept[AbortException] {
        generateApply(q"class Snapshot[A] extends SystemCall[A] {}")
      }
    }

    "construct 'apply' for parameterless 'class' structure" in {
      val constr = generateApply(q"class Snapshot[A]() extends SystemCall[A] {}")
      constr shouldEqual q"def apply[A](): SystemCall[A] = new Snapshot[A]()"
    }

    "construct 'apply' with correct param lists" in {
      val constr = generateApply(q"class Snapshot(val a: String)(b: Int)(implicit str: String) extends SystemCall[A] {}")
      constr shouldEqual q"def apply(a: String)(b: Int)(implicit str: String): SystemCall[A] = new Snapshot(a)(b)(str)"
    }

  }

  "generateUnapply" should {
    import UnapplyGenerator._

    "not generate anything for the 'object' structure" in {
      generateUnapply(q"object Test") shouldEqual None
    }

    "abort compilation if 'class' structure has no param lists" in {
      intercept[AbortException] {
        generateUnapply(q"class Snapshot extends Something {}")
      }
    }

    "generate simple deconstructor for parameterless class" in {
      val deconstr = generateUnapply(q"class Snapshot() extends Something {}")
      deconstr shouldEqual Some(q"def unapply(shape: Snapshot): Boolean = if (shape == null) false else true")
    }

    "generate proper deconstructor for a 'class' with a single fields" in {
      val deconstr = generateUnapply(q"class Person(name: String) extends Human {}")
      deconstr shouldEqual Some(q"def unapply(shape: Person): Option[String] = if (shape == null) None else Some(shape.name)")
    }

    "generate proper deconstructor for a 'class' with multiple fields" in {
      val deconstr = generateUnapply(q"class Person(name: String, age: Int, addr: Address) extends Human {}")
      val elseBranch = q"Some((shape.name, shape.age, shape.addr))"
      val retType = t"Option[(String, Int, Address)]"
      deconstr shouldEqual Some(q"def unapply(shape: Person): $retType = if (shape == null) None else $elseBranch")
    }

    "generate deconstructor with type variable" in {
      val deconstr = generateUnapply(q"class Foo[A]() extends Bar[A] {}")
      deconstr shouldEqual Some(q"def unapply[A](shape: Foo[A]): Boolean = if (shape == null) false else true")
    }

    "generate deconstructor with mutltiple type variables and type parameters" in {
      val deconstr = generateUnapply(q"class Foo[F[_, _], A](value: F[A, A], cont: A) extends Bar[F[A], A] {}")
      val elseBranch = q"Some((shape.value, shape.cont))"
      val retType = t"Option[(F[A, A], A)]"
      deconstr shouldEqual Some(q"def unapply[F[_, _], A](shape: Foo[F, A]): $retType = if (shape == null) None else $elseBranch")
    }

  }

}
