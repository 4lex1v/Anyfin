package platform.foundation.purity

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.meta._
import scala.collection.immutable.Seq

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
@compileTimeOnly("@constr[T] expansion failed. Please check that the Paradise plugin enabled.")
class constr extends StaticAnnotation {

  /**
   * @param funcDecl annotated function declaration (abstract function without body)
   * @return
   */
  inline def apply(funcDecl: Any): Any = meta {
    funcDecl match {
      case Decl.Def(mods, fName @ Term.Name(name), tparams, paramss, retType) ⇒
        if (mods.nonEmpty) abort("modifiers not allowed")

        // generate object with a private class as a shape, apply and unapply functions
        // generate: module, inner private class, apply, unapply

        val mangledName = "$constr$" + name

        /** GENERATING DATATYPE'S SHAPE*/

        // Used as a parent type.
        val dataType = retType match {
          case Type.Name(name) ⇒ Term.Apply(Ctor.Ref.Name(name), Seq.empty)
          case Type.Apply(constr, args) ⇒ Term.ApplyType(Ctor.Ref.Name(constr.toString()), args)
        }

        val MONKEY: Stat = {
          if (paramss.nonEmpty) {
            /** GENERATE CLASS-BASED SHAPE */

            // Similar to case classes we need to make params from the first parameters list
            // declared as `val`'s to be available during decostruction.
            val pubParams = {
              val h = paramss.head.map(_.copy(mods = Seq(Mod.ValParam())))
              h +: paramss.tail
            }

            val className = Type.Name(mangledName)
            q"private final class ${className}[..$tparams](...$pubParams) extends $dataType {}"
          } else {
            /** GENERATE OBJECT-BASED SHAPE */
            val objectName = Term.Name(mangledName)
            q"private object $objectName extends $dataType {}"
          }
        }

        //def APPLY = constr.genApply(MONKEY)

        /** GENERATING CONSTRUCTOR */

        val tps = tparams.map(p ⇒ Type.Name(p.name.value))

        val ctorName      = Ctor.Ref.Name(mangledName)
        val classCtorCall = {
          if (tps.nonEmpty) q"new $ctorName[..$tps](...${constr.paramssToArgs(paramss)})"
          else q"new $ctorName(...${constr.paramssToArgs(paramss)})"
        }
        val APPLY = MONKEY match {
          case cls: Defn.Class =>
            Defn.Def(mods, Term.Name("apply"), tparams, paramss, Some(retType), classCtorCall)
          case obj: Defn.Object =>
            Defn.Def(mods, Term.Name("apply"), tparams, paramss, Some(retType), Term.Name(mangledName))
        }

        MONKEY match {
          case obj: Defn.Object =>
            q"object $fName { $MONKEY; $APPLY }"
          case cls: Defn.Class =>
            val UNAPPLY = constr.genUnapply(cls)
            q"object $fName { $MONKEY; $UNAPPLY; $APPLY }"
        }

      case other ⇒
        abort(s"@constr annotation can be applied to functions only, got: $other")
    }
  }

}

object constr {

  /**
   * Convenient way to convert parameters into arguments
   */
  def paramssToArgs(paramss: Seq[Seq[Term.Param]]): Seq[Seq[Term.Arg]] = {
    paramss.map(_.map(p => Term.Name(p.name.value)))
  }

  def genUnapply(cls: Defn.Class): Defn.Def = {
    val paramName = Term.fresh("x")

    val cparamt = if (cls.tparams.nonEmpty) Type.Apply(cls.name, cls.tparams.map(x ⇒ Type.Name(x.name.value))) else cls.name

    val cparam = Term.Param(Seq.empty, paramName, Some(cparamt), None)

    val fieldCalls = unapplier.monkey(paramName, cls)

    val elsetree =
      if (fieldCalls.length > 1) q"Some(${Term.Tuple(fieldCalls)})"
      else q"Some(${fieldCalls.head})"

    q"def unapply[..${cls.tparams}]($cparam) = if ($paramName == null) None else $elsetree"
  }

  private object unapplier {
    def monkey(name: Term.Name, cls: Defn.Class): Seq[Term.Select] = {
      val fields = cls.ctor.paramss.head.map { _.name }
      fields.map { fieldName ⇒ Term.Select(name, Term.Name(fieldName.value)) }
    }
  }

}
