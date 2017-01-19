package platform.foundation.purity

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.meta._
import scala.collection.immutable.Seq

/**
 * Example:
 *
 * {{{
 *   sealed trait SomeType[A]   
 *   object SomeType {
 *     @constr[SomeType[A]] class Subclass[A] (value: A)
 *   }
 * }}}
 */
@compileTimeOnly("@constr[T] expansion failed. Please check that the Paradise plugin enabled.")
class constr[T] extends StaticAnnotation {

  inline def apply (defn: Any): Any = meta {
    val (dtType, dtParams) = constr.extractDataType(this)

    defn match {
      case Defn.Class(_, name, tparams, ctor, _) =>

        /** Generating proper case class */
        val newMods        = Seq(Mod.Sealed(), Mod.Abstract(), Mod.Case())
        val parentDataType = Term.ApplyType(Ctor.Ref.Name(dtType.toString()), dtParams)

        val ctorClass = q"..$newMods class $name[..$tparams] private (...${ctor.paramss}) extends $parentDataType {}"

        /** Generating companion object */

        // Constructor class call tree
        val ctorName      = Ctor.Ref.Name(name.toString())
        val classCtorCall = q"new $ctorName(...${constr.paramssToArgs(ctor.paramss)}) {}"

        // `apply` function
        val scope     = Name.Indeterminate(dtType.toString())
        val retType   = Type.Apply(dtType, dtParams)
        val applyFunc = q"private[$scope] def apply[..$tparams](...${ctor.paramss}): $retType = $classCtorCall"

        // Companion object
        val classCompObj = q"object ${Term.Name(name.toString())} { $applyFunc }"

        Term.Block(Seq(ctorClass, classCompObj))

      case _ ⇒  abort("bad @constr annotation application, please check the docs")
    }
  }
}

object constr {


  /**
   * Extract provided data type and its parameters from the annotation.
   * Example:
   *   Given the following code:
   *   {{{
   *     @constr[Foo[A, B]] class Bar[A, B]
   *   }}}
   *
   *   [[extractDataType(annot)]] returns a tuple (Foo, Seq(A, B))
   *
   * @param annotation reference to the `@constr` annotation tree
   * @return a pair of type, for the data type and it's type parameters
   */
  def extractDataType(annotation: Stat): (Type, Seq[Type]) = {
    annotation match {
      case q"new $_[$targ]()" ⇒
        targ match {
          case targ"$tname[..$params]" ⇒ (tname, params)
        }
      case _ => abort("@constr annotation expect single type argument")
    }
  }

  /**
   * Convenient way to convert parameters into arguments
   */
  def paramssToArgs(paramss: Seq[Seq[Term.Param]]): Seq[Seq[Term.Arg]] = {
    paramss.map(_.map(p => Term.Name(p.name.value)))
  }

}
