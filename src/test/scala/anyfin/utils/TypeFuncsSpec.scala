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
