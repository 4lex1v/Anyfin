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

import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq
import scala.meta._
import anyfin.utils.TypeFuncs._

final class dataType (strict: Boolean = true) extends StaticAnnotation {
  inline def apply(tree: Any): Any = meta {
    val mode = dataType.parseAnnotationMode(this)
    tree match {
      case trt: Defn.Trait =>
        dataType.expand(mode, trt)

      case Term.Block(Seq(trt: Defn.Trait, obj: Defn.Object)) =>
        dataType.expand(mode, trt, Some(obj))

      case other =>
        abort("@dataType annotation can be used with traits only")
    }
  }
}

object dataType {

  /**
   * @param defn
   * @return
   */
  def expand (
    isStrictMode: Boolean,
            defn: Defn.Trait,
       companion: Option[Defn.Object] = None
  ): Term.Block = {
    val q"..$mods trait $name[..$tparams] extends ..$parents { ..$stats }" = defn

    if (mods.nonEmpty)
      abort(mods.head.pos, "@dataType doesn't support modifiers")

    if (isStrictMode && parents.nonEmpty)
      abort(parents.head.pos, "@dataType annotation doesn't support inheritance in strict mode")

    val (body, constructors) = extractDataConstructors(stats, isStrictMode)

    Term.Block(Seq(
      q"sealed trait $name[..$tparams] extends ..$parents { ..$body }",
      q"""
         object ${Term.Name(name.value)} {
           ..${constructors.map(constr.expand)}
           ..${companion.flatMap(_.templ.stats).getOrElse(Seq.empty)}
         }
       """
    ))
  }

  /**
   * @return in non-strict mode return a seq of preserved statements and extracted list of data constructors
   * @param isInStrictMode
   * @param bodyStats
   */
  def extractDataConstructors (
         bodyStats: Seq[Stat],
    isInStrictMode: Boolean
  ): (Seq[Stat], Seq[Decl.Def]) = {
    bodyStats.foldLeft((Seq.empty[Stat], Seq.empty[Decl.Def])) {

      // In 'strict' mode [[dataType]] support only `def` declarations, otherwise we can mix
      // declarations and definitions with data constructors, in which case all constructor
      // declarations must be annotated with `@constr`.
      case ((bodyAcc, constrAcc), func: Decl.Def) =>
        // In 'strict' mode `@constr` is not required
        // All def decls are constructors
        if (isInStrictMode || func.isAnnotatedWith("constr")) (bodyAcc, constrAcc :+ func)
        else abort("Def declarations must be annotation with '@constr'")

      case ((bodyAcc, constrAcc), stat) =>
        if (!isInStrictMode) (bodyAcc :+ stat, constrAcc)
        else abort("Only function declarations are allowed")
    }
  }

  def parseAnnotationMode (stat: Stat): Boolean = {
    stat match {
      case q"new $_()" ⇒ true
      case q"new $_(${Lit(value: Boolean)})" ⇒ value
      case q"new $_(strict = ${Lit(value: Boolean)})" ⇒ value
      case arg ⇒ abort(stat.pos, s"Argument '$arg' is not supported")
    }
  }

}