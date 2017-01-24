package anyfin.utils

import scala.collection.immutable.Seq
import scala.meta._

object TypeFuncs {

  /**
   * Deconstructs type into its constituents. Example:
   * {{{ (F[Either[G[A], E]], E) -> Seq(F[_], G[_], A, E) }}}
   *
   *
   * @param tpe type to be deconstructed
   * @return sequence of constituent types
   */
  def deconstructType (tpe: Type.Arg): Seq[Type] = {
    tpe match {
      case t"(..$types)" ⇒ types.flatMap(deconstructType)
      case t"$f[..$args]" ⇒ t"$f[..${args.map(_ ⇒ t"_")}]" +: args.flatMap(deconstructType)
      case tpe: Type.Name ⇒ Seq(tpe)
      case _ => abort(s"unable to deconstruct type: ${tpe.structure}")
    }
  }

  def annotatedWith(mods: Seq[Mod], mod: String): Boolean = {
    val cache = Mod.Annot(Ctor.Ref.Name(mod))
    def loop(rest: Seq[Mod]): Boolean = {
      rest match {
        case Seq() => false
        case Seq(head, _*) =>
          if (head.structure == cache.structure) true
          else loop(rest.tail)
      }
    }
    loop(mods)
  }

  /**
   * Convenient way to convert parameters into arguments
   *
   * @param paramss
   */
  def paramssToArgs(paramss: Seq[Seq[Term.Param]]): Seq[Seq[Term.Arg]] = {
    paramss.map(_.map(p => Term.Name(p.name.value)))
  }

}
