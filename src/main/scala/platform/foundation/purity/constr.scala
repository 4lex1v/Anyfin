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
  inline def apply(defn: Any): Any = meta {
    defn match {
      case Decl.Def(mods, fName @ Term.Name(name), tparams, paramss, retType) ⇒
        if (mods.nonEmpty) abort("modifiers not allowed")

        /** GENERATING CLASS */
        val mangledClassName = "$constr$$" + name
        val mangledClassNameT = Type.Name(mangledClassName)

        val dataType: Ctor.Call = retType match {
          case Type.Apply(tname, targs) ⇒
            Term.ApplyType(Ctor.Ref.Name(tname.toString()),targs)
          case Type.Name(tname) ⇒
            Term.Apply(Ctor.Ref.Name(tname), Seq.empty)
        }

        val CLASS = q"sealed abstract case class $mangledClassNameT[..$tparams](...$paramss) extends $dataType {}"

        /** GENERATING CONSTRUCTOR */

        val tps = tparams.map(p ⇒ Type.Name(p.name.value))

        val ctorName      = Ctor.Ref.Name(mangledClassName)
        val classCtorCall = {
          if (tps.nonEmpty) q"new $ctorName[..$tps](...${constr.paramssToArgs(paramss)}) {}"
          else q"new $ctorName(...${constr.paramssToArgs(paramss)}) {}"
        }
        val FUNC = Defn.Def(mods, fName, tparams, paramss, Some(retType), classCtorCall)

        /**
         * TODO :: custom unapply generator.
         * The following creates an `abstract case class`, which generates `unapply` without `apply`. It's
         * OK for now to go this path, though this solution has downsides generating garbage for `case` classes.
         */
        Term.Block(Seq(CLASS, FUNC))

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

}