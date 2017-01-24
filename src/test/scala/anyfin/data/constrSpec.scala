/*
 * Copyright 2017 Aleksandr Ivanov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package anyfin.data

import org.scalatest._
import scala.collection.immutable.Seq
import scala.meta._
import anyfin.Equalities._
import constr._

import scala.meta.internal.inline.AbortException

class constrSpec extends WordSpec with Matchers {

  "inferDataType" should {
    "refine applied type-constructor into data type" in {
      inferDataType(t"Option[String]") shouldEqual ctor"Option[String]"
    }

    "refine simple type into data type" in {
      inferDataType(t"String") shouldEqual ctor"String()"
    }

    "not compile" in {
      intercept[AbortException] {
        inferDataType(t"(Future[String], Int)")
      }
    }
  }

  "genDataShape" should {
    val dataType = inferDataType(t"SystemCall[Frame]")

    "generate 'object' structure" in {
      val shape = genDataShape("Test", Seq.empty, Seq.empty, dataType)
      shape shouldEqual q"private object Test extends $dataType {}"
    }

    "generate parameterless 'class' structure" in {
      val shape = genDataShape("Test", Seq(tparam"A"), Seq.empty, dataType)
      shape shouldEqual q"private final class Test[A] extends $dataType {}"
    }

    "generate 'class' structure with public fields" in {
      val shape = genDataShape("Test", Seq.empty, Seq(Seq(param"app: Application")), dataType)
      shape shouldEqual q"private final class Test(val app: Application) extends $dataType {}"
    }

    "generate 'class' structure with multiple param lists, where first one is public and implicit param list support" in {
      val paramss = Seq(Seq(param"app: Application"), Seq(param"implicit S: SystemCall :<: S"))
      val shape = genDataShape("Test", Seq.empty, paramss, dataType)
      shape shouldEqual q"private final class Test(val app: Application)(implicit S: SystemCall :<: S) extends $dataType {}"
    }

    "generate 'class' structure" in {
      val shape = genDataShape("Test", Seq(tparam"A"), Seq(Seq(param"value: A")), dataType)
      shape shouldEqual q"private final class Test[A](val value: A) extends $dataType {}"
    }
  }

  "genDataConstructor" should {
    val cls = q"class Snapshot(app: Application) extends SystemCall[Frame] {}"

    "construct parameterless 'apply' for the 'object' structure" in {
      val constr = genDataConstructor(q"object Signal extends Contract[Int] {}")
      constr shouldEqual q"def apply(): Contract[Int] = Signal"
    }

    "construct 'apply' for the 'class' structure" in {
      val constr = genDataConstructor(q"class Snapshot(val app: Application) extends SystemCall[Frame] {}")
      constr shouldEqual q"def apply(app: Application): SystemCall[Frame] = new Snapshot(app)"
    }

    "require empty param list" in {
      intercept[AbortException] {
        genDataConstructor(q"class Snapshot[A] extends SystemCall[A] {}")
      }
    }

    "construct 'apply' for parameterless 'class' structure" in {
      val constr = genDataConstructor(q"class Snapshot[A]() extends SystemCall[A] {}")
      constr shouldEqual q"def apply[A](): SystemCall[A] = new Snapshot[A]()"
    }

    "construct 'apply' with correct param lists" in {
      val constr = genDataConstructor(q"class Snapshot(val a: String)(b: Int)(implicit str: String) extends SystemCall[A] {}")
      constr shouldEqual q"def apply(a: String)(b: Int)(implicit str: String): SystemCall[A] = new Snapshot(a)(b)(str)"
    }

  }

  "genDataDeconstructor" should {
    "not generate anything for the 'object' structure" in {
      genDataDeconstructor(q"object Test") shouldEqual None
    }

    "abort compilation if 'class' structure has no param lists" in {
      intercept[AbortException] {
        genDataDeconstructor(q"class Snapshot extends Something {}")
      }
    }

    "generate simple deconstructor for parameterless class" in {
      val deconstr = genDataDeconstructor(q"class Snapshot() extends Something {}")
      deconstr shouldEqual Some(q"def unapply(shape: Snapshot) = if (shape == null) false else true")
    }

    "generate proper deconstructor for a 'class' with a single fields" in {
      val deconstr = genDataDeconstructor(q"class Person(name: String) extends Human {}")
      deconstr shouldEqual Some(q"def unapply(shape: Person) = if (shape == null) None else Some(shape.name)")
    }

    "generate proper deconstructor for a 'class' with multiple fields" in {
      val deconstr = genDataDeconstructor(q"class Person(name: String, age: Int, addr: Address) extends Human {}")
      val elseBranch = q"Some((shape.name, shape.age, shape.addr))"
      deconstr shouldEqual Some(q"def unapply(shape: Person) = if (shape == null) None else $elseBranch")
    }

    "generate deconstructor with type variable" in {
      val deconstr = genDataDeconstructor(q"class Foo[A]() extends Bar[A] {}")
      deconstr shouldEqual Some(q"def unapply[A](shape: Foo[A]) = if (shape == null) false else true")
    }

    "generate deconstructor with mutltiple type variables and type parameters" in {
      val deconstr = genDataDeconstructor(q"class Foo[F[_, _], A](value: F[A], cont: A) extends Bar[F[A], A] {}")
      val elseBranch = q"Some((shape.value, shape.cont))"
      deconstr shouldEqual Some(q"def unapply[F[_, _], A](shape: Foo[F, A]) = if (shape == null) None else $elseBranch")
    }

  }

}
