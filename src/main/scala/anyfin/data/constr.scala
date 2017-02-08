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

import anyfin.internal.data.DataConstructors

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.meta._

/**
 * Constructor annotation - transforms function declaration into data type constructor.
 *
 * In Haskell, data type (i.e ADT) is defined in term of its constructors, e.g:
 *
 * {{{
 *   data Maybe a = Empty | Just a
 * }}}
 *
 * In the snippet above `Nothing` and `Just` a data constructors, simple functions.
 * The canonical version of the same ADT in Scala would looks this way:
 *
 * {{{
 *   sealed trait Maybe[+A]
 *   final case class Just[A](value: A) extends Maybe[A]
 *   case object Empty extends Maybe[Nothing]
 * }}}
 *
 * The definition above has multiple drawbacks, which may cause undesired behavious or
 * usage of the data type. E.g:
 *
 * {{{
 *   scala> val s = Some(1)
 *   s: Some[Int] = Some(1) // not Option[Int]
 * }}}
 *
 * There are multiple approaches to fix this, these can be found in projects like
 * [[https://github.com/scalaz/scalaz Scalaz]], [[https://github.com/typelevel/cats Cats]],
 * [[https://github.com/functional-streams-for-scala/fs2 FS2]] and others. Unfortunately,
 * using standard Scala constructs won't enable this level of control.
 *
 * `@constr` annotation should fix this, or at least hide all the corner cases from the end user.
 * The annotation can be applied to function declaration:
 *
 * {{{
 *   @constr def Just[A](value: A): Maybe[A]
 * }}}
 *
 * Following the Haskell's representation, publicly it's just a data constructor with a corresponding
 * deconstructor, which can be used in pattern matching. 
 */
@compileTimeOnly("@constr expansion failed. Please check that the Paradise plugin enabled.")
class constr extends StaticAnnotation {
  inline def apply(tree: Any): Any = meta {
    tree match {
      case funcDecl: Decl.Def ⇒
        DataConstructors.expandAnnotation(funcDecl)
      case other ⇒ abort("@constr annotation can be used with function declarations only")
    }
  }
}