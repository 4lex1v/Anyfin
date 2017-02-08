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
package data

import scala.collection.immutable.Seq
import scala.meta._

import anyfin.internal.utils.TypeFuncs._

private[anyfin] object DataConstructors {
  type DataType = Ctor.Call

  /**
   * Internal implementation for [[constr]] annotation.
   *
   * Generates required (de)constructor with the corresponding data shape.
   *
   * @param funcDecl declaration used to generate data (de)constructor
   * @return object that encapsulates generated (de)constructor
   */
  def expandAnnotation (funcDecl: Decl.Def): Defn.Object = {
    val q"..$mods def $name[..$tparams](...$paramss): $retType" = funcDecl

    if (mods.nonEmpty)
      abort(mods.head.pos, "@constr annotation doesn't support modifiers")

    val dataType      = DataTypeInference.inferDataType(retType)
    val dataShape     = ShapeConstructor.genDataShape("$constr$" + name, tparams, paramss, dataType)
    val constructor   = ApplyGenerator.generateApply(dataShape)
    val deconstructor = UnapplyGenerator.generateUnapply(dataShape)

    q"object $name { ..${Seq(dataShape, constructor) ++ deconstructor} }"
  }

  /**
   * The return type of our "data constructor" will be used as a "data type". To some
   * extent it corresponds to the way we build ADT's in Scala, e.g:
   *
   * {{{
   *   trait Option[A]
   *   case class Some[A](value: A) extends Option[A]
   * }}}
   *
   * In the snippet above, case class definition corresponds to the following function
   * signature:
   *
   * {{{
   *   @constr def Some[A](value: A): Option[A]
   * }}}
   *
   * NOTE :: At this point there's no way to constraint the result type, i.e it can be anything,
   *         including complete gibberish or final classes (e.g `String`). Waiting for Scalameta 2.0
   */
  object DataTypeInference {

    /**
     * Infers the "data type" from the declaration's return type.
     *
     * @param dataType declared return type
     * @return inferred parent "data type"
     */
    def inferDataType (dataType: Type): DataType = dataType match {
      case Type.Apply(constr, args) ⇒ Term.ApplyType(Ctor.Ref.Name(constr.toString()), args)
      case Type.Name(name) ⇒ Term.Apply(Ctor.Ref.Name(name), Seq.empty)
      case other ⇒ abort(s"$other can't be used as a return type for data constructor")
    }

  }

  object ShapeConstructor {

    /**
     * To generate data constructor (i.e `apply` function) and deconstructor (i.e `unapply` function)
     * we need to replicate `case` behaviour in class definition - parameters in the first param list
     * have to be public.
     *
     * @param paramss - list of parameter lists, e.g `class Foo(a: String)(b: Int)(implicit bar: Bar)`
     * @return parameters lists where first list declares public fields
     */
    def liftConstructor (paramss: Paramss): Paramss = {
      if (paramss.isEmpty) Seq(Seq.empty) // corresponds to `class Foo()`
      else paramss.head.map(_.copy(mods = Seq(Mod.ValParam()))) +: paramss.tail
    }

    /**
     * Using the data provided generate the corresponding data constructor.
     *
     * NOTE :: If the declaration has no type parameters or parameter lists, it's possible to use
     *         object as a shape, otherwise class is generated
     *
     * @return "data shape" implemented in terms of an object or a class
     */
    def genDataShape (name: String, tparams: TParams, paramss: Paramss, dataType: DataType): Defn = {
      if (tparams.isEmpty && paramss.isEmpty) q"private object ${Term.Name(name)} extends $dataType {}"
      else q"private final class ${Type.Name(name)}[..$tparams](...${liftConstructor(paramss)}) extends $dataType {}"
    }

  }

  object ApplyGenerator {

    /**
     * Deconstructs a "data type" into a plain type
     */
    def inferResultType (dataType: DataType): Type = dataType match {
      case Term.ApplyType(Ctor.Ref.Name(constr), args) ⇒ Type.Apply(Type.Name(constr), args)
      case Term.Apply(Ctor.Ref.Name(name), _) ⇒ Type.Name(name)
      case other ⇒ abort(s"$other can't be user as a return type")
    }

    def normalize (params: Seq[Term.Param]): Seq[Term.Param] = {
      params.map { param ⇒
        param.copy(mods = param.collect { case m: Mod.Implicit ⇒ m })
      }
    }

    def genImplementation(name: String, tparams: TParams, paramss: Paramss) = {
      val typeVars = tParamsToArgs(tparams)
      val pArgs    = paramssToArgs(paramss)
      val ctorCall = Ctor.Ref.Name(name)

      if (tparams.isEmpty) q"new $ctorCall(...$pArgs)"
      else q"new $ctorCall[..${typeVars}](...$pArgs)"
    }

    /**
     * Data constructor (i.e `apply` function) generator.
     *
     * @param dataShape object or class definition
     * @return generated `apply` function
     */
    def generateApply (dataShape: Stat): Defn.Def = {
      dataShape match {
        case q"..$_ object $name extends $retType {}" ⇒
          q"def apply(): ${inferResultType(retType)} = $name"

        case cls: Defn.Class if cls.ctor.paramss.length < 1 ⇒
          // The same requirement to have a param list for classes
          abort("classes without parameter list are not allowed")

        case q"..$_ class $name[..$tparams](...$paramss) extends $retType {}" ⇒
          val params = paramss.map(normalize)
          val impl   = genImplementation(name.value, tparams, paramss)

          q"def apply[..$tparams](...$params): ${inferResultType(retType)} = $impl"
      }

    }

  }

  object UnapplyGenerator {
    /**
     * Data deconstructor (i.e `unapply` function) generator.
     *
     * @param dataShape object or class definition
     * @return generated `unapply` function
     */
    def generateUnapply (dataShape: Stat): Option[Defn.Def] = {
      dataShape match {
        case obj: Defn.Object ⇒ None
        case q"..$_ class $name[..$tparams](...$paramss) extends $dataType {}" if paramss.length < 1 ⇒
          // TODO :: this should be fixed... on a separate branch?
          abort("classes without parameter list are not allowed")
        case q"..$_ class $name[..$tparams](...$paramss) extends $dataType {}" ⇒
          val unapplyRetType = paramss match {
            case Seq(head, _*) if head.length == 1 =>
              t"Option[${paramssTypes(head).head}]"
            case Seq(head, _*) if head.length > 1 =>
              t"Option[${Type.Tuple(paramssTypes(head))}]"
            case _ => Type.Name("Boolean")
          }

          def template (ifBranch: Term = Term.Name("None"), elseBranch: Term): Option[Defn.Def] = {
            val shapeType: Type = if (tparams.isEmpty) name else t"$name[..${ tParamsToArgs(tparams)}]"
            Some {
              q"""def unapply[..$tparams](shape: $shapeType): $unapplyRetType =
                  if (shape == null) $ifBranch else $elseBranch
             """
            }
          }

          val fieldCalls: Seq[Term.Select] = {
            if (paramss.isEmpty) Seq.empty
            else paramss.head.map(field ⇒ q"shape.${Term.Name(field.name.value)}")
          }

          if (fieldCalls.length > 1)
            template(elseBranch = q"Some(${Term.Tuple(fieldCalls)})")
          else if (fieldCalls.length == 1)
            template(elseBranch = q"Some(${fieldCalls.head})")
          else template(q"false", q"true")
      }
    }

  }

}
