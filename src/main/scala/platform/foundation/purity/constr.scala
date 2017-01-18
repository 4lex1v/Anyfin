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
@compileTimeOnly("@constr[T] can only be applied to simple classes defined in the companion T's companion object")
class constr[T] extends StaticAnnotation {

  inline def apply (defn: Any): Any = meta {
    val q"new $_[$tparams]()" = this

    val targ"$tname[..$params]" = tparams


    def paramssToArgs(paramss: Seq[Seq[Term.Param]]): Seq[Seq[Term.Arg]] = {
      paramss.map(_.map(p => Term.Name(p.name.value)))
    }

    defn match {
      case cls @ Defn.Class(mods, name, tparams, ctor @ Ctor.Primary(_, cname, args), body @ Template(early, parents, self, stats)) =>
        val newMods = Seq(Mod.Sealed(), Mod.Abstract(), Mod.Case())

        val parentDataType: Ctor.Call = Term.ApplyType(Ctor.Ref.Name(tname.toString()), params)

        val outer = Name.Indeterminate(tname.toString())

        Defn.Def(
          Seq(Mod.Private(Name.Indeterminate(tname.toString()))),
          Term.Name("apply"),
          Seq.empty,
          Seq.empty,
          Some(Type.Apply(tname, params)),
          Term.Block(Seq.empty)
        )

        val retType = Type.Apply(tname, params)

        val ctorName = Ctor.Ref.Name(name.toString())
        val apply = q"private[$outer] def apply[..$tparams](...$args): $retType = new $ctorName(...${paramssToArgs(args)}) {}"

        Term.Block(Seq(
          q"..$newMods class $name[..$tparams] (...$args) extends $parentDataType {}",
          q"object ${Term.Name(name.toString())} { $apply }"
        ))

      case _ ⇒
        abort("annotation works only with simple classes")
    }
  }
}