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

import anyfin.utils.TypeFuncs._

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.collection.immutable.Seq
import scala.meta._

/**
 *
 *
 * @param strict parameter used to switch between expansion modes.
 */
@compileTimeOnly("@dataType expansion failed. Please check that the Paradise plugin enabled.")
class dataType (strict: Boolean = true) extends StaticAnnotation {
  inline def apply (tree: Any): Any = meta {
    tree match {
      case traitDefn: Defn.Trait =>
        dataType.expand(traitDefn, dataType.parseAnnotationMode(this))
      case other =>
        abort("@dataType annotation can be used with traits only")
    }
  }
}

object dataType {

  /**
   *
   *
   * @param traitDefn  trait's AST tree
   * @param strictMode strict or flexible annotation mode
   * @return Code block with a data type's trait and a corresponding companion object
   */
  def expand (traitDefn: Defn.Trait, strictMode: Boolean): Term.Block = {
    val q"..$mods trait $name[..$tparams] extends $parent { ..$stats }" = traitDefn

    // NOTE :: maybe it's worth revisiting ??
    if (mods.nonEmpty)
      abort(mods.head.pos, "@dataType annotation doesn't support modifiers")

    // List of function declarations that represent data constructors
    val (newBody, constructors) = extractDataConstructors(stats, strictMode)

    Term.Block(Seq(
      q"sealed trait $name { ..$newBody }",
      q"object ${Term.Name(name.value)} { ..${constructors.map(constr.expand)} }"
    ))
  }

  /**
   * @return in non-strict mode return a seq of preserved statements and extracted list of data constructors
   * @param isInStrictMode
   * @param bodyStats
   */
  def extractDataConstructors (bodyStats: Seq[Stat], isInStrictMode: Boolean): (Seq[Stat], Seq[Decl.Def]) = {
    bodyStats.foldLeft((Seq.empty[Stat], Seq.empty[Decl.Def])) {
      case ((bodyAcc, constrAcc), func: Decl.Def) =>
        if (isInStrictMode) (Seq.empty, func +: constrAcc)
        else if (annotatedWith(func.mods, "constr"))
          (bodyAcc, func +: constrAcc)
        else (func +: bodyAcc, constrAcc)

      case ((bodyAcc, constrAcc), stat) =>
        if (!isInStrictMode) (stat +: bodyAcc, constrAcc)
        else abort("Only function declarations are allowed")
    }
  }

  /**
   * @param stat
   * @return
   */
  def parseAnnotationMode (stat: Stat): Boolean = {
    stat match {
      case q"new $_()" ⇒ true
      case q"new $_(${Lit(value: Boolean)})" ⇒ value
      case q"new $_(strict = ${Lit(value: Boolean)})" ⇒ value
      case arg ⇒ abort(stat.pos, s"Argument '$arg' is not supported")
    }
  }
}
