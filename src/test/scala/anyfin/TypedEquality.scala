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

package anyfin

import org.scalactic.Equality
import scala.collection.immutable.Seq
import scala.meta.{Tree, Type}
import scala.reflect.ClassTag

abstract class TypedEquality[A: ClassTag] extends Equality[A] {
  final override def areEqual (a: A, b: Any): Boolean = {
    b match {
      case typedB: A ⇒ checkEqual(a, typedB)
      case _ ⇒ false
    }
  }

  def checkEqual(left: A, right: A): Boolean
}

object Equalities {

  implicit def OptionEquality[T](implicit Eq: TypedEquality[T]): TypedEquality[Option[T]] = {
    new TypedEquality[Option[T]] {
      override def checkEqual(left: Option[T], right: Option[T]): Boolean = {
        (left, right) match {
          case (None, None) => true
          case (_, None) | (None, _) => false
          case (Some(l), Some(r)) => Eq.checkEqual(l, r)
        }
      }
    }
  }


  implicit def SeqEquality[T](implicit Eq: TypedEquality[T]): TypedEquality[Seq[T]] = {
    new TypedEquality[Seq[T]] {
      override def checkEqual (left: Seq[T], right: Seq[T]): Boolean = {
        (left, right) match {
          case (Seq(), Seq()) ⇒ true
          case (_, Seq()) | (Seq(), _) ⇒ false
          case (Seq(headA, _*), Seq(headB, _*)) ⇒
            Eq.checkEqual(headA, headB) && checkEqual(left.tail, right.tail)
        }
      }
    }
  }

  implicit def TreeEquality[T <: Tree: ClassTag]: TypedEquality[T] = {
    new TypedEquality[T] {
      override def checkEqual (left: T, right: T) = left.structure == right.structure
    }
  }

}
