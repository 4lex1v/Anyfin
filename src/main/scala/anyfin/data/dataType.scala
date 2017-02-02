package anyfin.data

import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq
import scala.meta._
import anyfin.utils.TypeFuncs

final class dataType(strict: Boolean = true) extends StaticAnnotation {
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
  def extractDataConstructors (bodyStats: Seq[Stat], isInStrictMode: Boolean): (Seq[Stat], Seq[Decl.Def]) = {
    bodyStats.foldLeft((Seq.empty[Stat], Seq.empty[Decl.Def])) {
      case ((bodyAcc, constrAcc), func: Decl.Def) =>
        if (isInStrictMode) (Seq.empty, func +: constrAcc)
        else if (TypeFuncs.annotatedWith(func.mods, "constr"))
          (bodyAcc, func +: constrAcc)
        else (func +: bodyAcc, constrAcc)

      case ((bodyAcc, constrAcc), stat) =>
        if (!isInStrictMode) (stat +: bodyAcc, constrAcc)
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