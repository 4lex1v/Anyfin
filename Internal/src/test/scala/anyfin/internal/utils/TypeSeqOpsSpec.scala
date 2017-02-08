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

package anyfin.internal
package utils

import anyfin.internal.utils.TypeSeqOps._
import org.scalatest._

import scala.meta._
import Equalities._

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
