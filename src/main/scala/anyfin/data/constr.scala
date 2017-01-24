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

import anyfin.utils.TypeFuncs.paramssToArgs

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.collection.immutable.Seq
import scala.meta._

/**
 * Example:
 *
 * {{{
 *   sealed trait SystemCall[A]
 *   object SomeType {
 *     @constr def FinaWindow (app: Application): SystemCall[Option[Window]]
 *     @constr def Snapshot   (window: Window)  : SystemCall[Frame]
 *     @constr def Foo[A]     (value: A)        : SystemCall[A]
 *   }
 * }}}
 */
@compileTimeOnly("@constr expansion failed. Please check that the Paradise plugin enabled.")
class constr extends StaticAnnotation {
  inline def apply(tree: Any): Any = meta {
    tree match {
      case funcDecl: Decl.Def ⇒
        constr.expand(funcDecl)
      case other ⇒
        abort("@constr annotation can be used with function declarations only")
    }
  }
}

object constr {

  /** TYPE ALIASES */
  type TParams = Seq[Type.Param]
  type Paramss = Seq[Seq[Term.Param]]

  /**
   * @param funcDecl
   * @return
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
     */
    val dataType = inferDataType(retType)

    // Internally class or object
    val dataShape = genDataShape("$constr$" + name, tparams, paramss, dataType)

    // Apply function
    val constructor = genDataConstructor(dataShape)

    // Unapply function
    val deconstructor = genDataDeconstructor(dataShape)

    q"object $name { ${Term.Block(Seq(dataShape, constructor) ++ deconstructor)} }"
  }

  /**
   * @param retType
   * @return
   */
  def inferDataType (retType: Type): Ctor.Call = retType match {
    case Type.Apply(constr, args) ⇒ Term.ApplyType(Ctor.Ref.Name(constr.toString()), args)
    case Type.Name(name) ⇒ Term.Apply(Ctor.Ref.Name(name), Seq.empty)
    case other ⇒ abort(s"$other can't be used as a return type for data constructor")
  }

  /**
   * @param decl
   * @return
   */
  def genDataShape(name: String, tparams: TParams, paramss: Paramss, dataType: Ctor.Call): Stat = {
    // If declaration has no type variables and parameterless, generate simple object
    if (tparams.isEmpty && paramss.isEmpty)
      q"private object ${Term.Name(name)} extends $dataType {}"
    else {
      // Simulate "case class" behaviour
      val params = {
        if (paramss.isEmpty) Seq.empty
        else paramss.head.map(_.copy(mods = Seq(Mod.ValParam()))) +: paramss.tail
      }

      q"private final class ${Type.Name(name)}[..$tparams](...$params) extends $dataType {}"
    }
  }

  /**
   * @param dataShape
   * @return
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

    dataShape match {
      case q"..$_ object $name extends $retType {}" ⇒
        q"def apply(): ${inferResultType(retType)} = $name"

      case q"..$_ class $name[..$tparams](...$paramss) extends $retType {}" if paramss.length < 1 ⇒
        // The same requirement to have a param list for classes
        abort("classes without parameter list are not allowed")

      case q"..$_ class $name[..$tparams](...$paramss) extends $retType {}" ⇒
        val normalized = paramss.map(normalize)
        val impl = {
          val typeVars = tparams.map(p ⇒ Type.Name(p.name.value))
          if (tparams.nonEmpty) q"new ${Ctor.Ref.Name(name.value)}[..$typeVars](...${paramssToArgs(paramss)})"
          else q"new ${Ctor.Ref.Name(name.value)}(...${paramssToArgs(paramss)})"
        }

        q"def apply[..$tparams](...$normalized): ${inferResultType(retType)} = $impl"
    }
  }

  /**
   * @param dataShape
   * @return
   */
  def genDataDeconstructor (dataShape: Stat): Option[Defn.Def] = {

    /**
     *
     * TODO :: Instead of inferring unapply's return type, build it manually
     *
     * @param name
     * @param tparams
     * @param paramss
     * @return
     */
    def build(name: String, tparams: Seq[Type.Param], paramss: Paramss): Defn.Def = {
      def template(ifBranch: Term = Term.Name("None"), elseBranch: Term) = {
        val shapeType = {
          if (tparams.isEmpty) Type.Name(name)
          else Type.Apply(Type.Name(name), tparams.map(p ⇒ Type.Name(p.name.value)))
        }
        q"def unapply[..$tparams](shape: $shapeType) = if (shape == null) $ifBranch else $elseBranch"
      }
        

      val fieldCalls = paramss match {
        case Seq() ⇒ Seq.empty
        case Seq(head, _*) ⇒
          head.map(field ⇒ q"shape.${Term.Name(field.name.value)}")
      }

      if (fieldCalls.length > 1)
        template(elseBranch = q"Some(${Term.Tuple(fieldCalls)})")
      else if (fieldCalls.length == 1)
        template(elseBranch = q"Some(${fieldCalls.head})")
      else template(q"false", q"true")
    }

    dataShape match {
      case obj: Defn.Object ⇒ None
      case q"..$_ class $name[..$tparams](...$paramss) extends $dataType {}" if paramss.length < 1 ⇒
        abort("classes without parameter list are not allowed")
      case q"..$_ class $name[..$tparams](...$paramss) extends $dataType {}" ⇒
        Some(build(name.value, tparams, paramss))
    }
  }
}
