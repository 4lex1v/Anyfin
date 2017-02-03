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

import anyfin.utils.TypeFuncs.{paramssToArgs, tParamsToArgs, paramssTypes}

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.collection.immutable.Seq
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
      case funcDecl: Decl.Def ⇒ constr.expand(funcDecl)
      case other ⇒ abort("@constr annotation can be used with function declarations only")
    }
  }
}

private[anyfin] object constr {

  /** TYPE ALIASES */
  type TParams = Seq[Type.Param]
  type Paramss = Seq[Seq[Term.Param]]

  /**
   * Internal implementation for [[constr]] annotation.
   *
   * Generates required (de)constructor with the corresponding data shape.
   * 
   * @param funcDecl declaration used to generate data (de)constructor
   * @return object that encapsulates generated (de)constructor
   */
  def expand (funcDecl: Decl.Def): Defn.Object = {
    val q"..$mods def $name[..$tparams](...$paramss): $retType" = funcDecl

    if (mods.nonEmpty)
      abort(mods.head.pos, "@constr annotation doesn't support modifiers")

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
    val dataType = inferDataType(retType)

    // Internally class or object
    val dataShape = genDataShape("$constr$" + name, tparams, paramss, dataType)

    // Apply function
    val constructor = genDataConstructor(dataShape)

    // Unapply function
    val deconstructor = genDataDeconstructor(dataShape)

    q"object $name { ..${Seq(dataShape, constructor) ++ deconstructor} }"
  }

  /**
   * Infers the "data type" from the declaration's return type.
   * 
   * @param retType declared return type
   * @return infered parent "data type"
   */
  def inferDataType (retType: Type): Ctor.Call = retType match {
    case Type.Apply(constr, args) ⇒ Term.ApplyType(Ctor.Ref.Name(constr.toString()), args)
    case Type.Name(name) ⇒ Term.Apply(Ctor.Ref.Name(name), Seq.empty)
    case other ⇒ abort(s"$other can't be used as a return type for data constructor")
  }

  /**
   * Using the data provided generate the corresponding data constructor.
   *
   * NOTE :: If the declaration has no type parameters or parameter lists, it's possible to use
   *         object as a shape, otherwise class is generated
   * 
   * @return "data shape" implementated in terms of an object or a class
   */
  def genDataShape (name: String, tparams: TParams, paramss: Paramss, dataType: Ctor.Call): Stat = {
    // If declaration has no type variables and parameterless, generate simple object
    if (tparams.isEmpty && paramss.isEmpty)
      q"private object ${Term.Name(name)} extends $dataType {}"
    else {
      // Simulate "case class" behaviour
      val params: Paramss = {
        if (paramss.isEmpty) Seq.empty
        else paramss.head.map(_.copy(mods = Seq(Mod.ValParam()))) +: paramss.tail
      }

      q"private final class ${Type.Name(name)}[..$tparams](...$params) extends $dataType {}"
    }
  }

  /**
   * Data constructor (i.e `apply` function) generator.
   * 
   * @param dataShape object or class definition
   * @return generated `apply` function
   */
  def genDataConstructor (dataShape: Stat): Defn.Def = {
    def normalize (params: Seq[Term.Param]): Seq[Term.Param] = {
      params.map { param ⇒
        param.copy(mods = param.collect { case m: Mod.Implicit ⇒ m })
      }
    }

    def inferResultType (ctor: Ctor.Call): Type = ctor match {
      case Term.ApplyType(Ctor.Ref.Name(constr), args) ⇒ Type.Apply(Type.Name(constr), args)
      case Term.Apply(Ctor.Ref.Name(name), _) ⇒ Type.Name(name)
      case other ⇒ abort(s"$other can't be user as a return type")
    }

    def genImplementation(name: String, tparams: TParams, paramss: Paramss) = {
      val typeVars = tParamsToArgs(tparams)
      val pArgs    = paramssToArgs(paramss)
      val ctorCall = Ctor.Ref.Name(name)

      if (tparams.isEmpty) q"new $ctorCall(...$pArgs)"
      else q"new $ctorCall[..${typeVars}](...$pArgs)"
    }

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

  /**
   * Data deconstructor (i.e `unapply` function) generator.
   * 
   * @param dataShape object or class definition
   * @return generated `unapply` function
   */
  def genDataDeconstructor (dataShape: Stat): Option[Defn.Def] = {
    dataShape match {
      case obj: Defn.Object ⇒ None
      case q"..$_ class $name[..$tparams](...$paramss) extends $dataType {}" if paramss.length < 1 ⇒
        // TODO :: this should be fixed... on a separate branch?
        abort("classes without parameter list are not allowed")
      case q"..$_ class $name[..$tparams](...$paramss) extends $dataType {}" ⇒
        val unapplyRetType = paramss match {
          case Seq() => Type.Name("Boolean")
          case Seq(head, _*) if head.length == 1 =>
            t"Option[${paramssTypes(head).head}]"
          case Seq(head, _*) =>
            t"Option[${Type.Tuple(paramssTypes(head))}]"
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
