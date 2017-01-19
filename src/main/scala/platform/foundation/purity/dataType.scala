package platform.foundation.purity

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.meta._
import scala.collection.immutable.Seq

@compileTimeOnly("@constr[T] expansion failed. Please check that the Paradise plugin enabled.")
class dataType extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case Defn.Trait(mods, name, tparams, ctor, body) =>
        val nb = body.copy(stats = body.stats.map { stats =>
          stats.map {
            case Decl.Def(mods, fName @ Term.Name(name), tparams, paramss, retType) =>
              q"@constr def $fName[..$tparams](...$paramss): $retType"
          }
        })

        Term.Block(Seq(
          q"sealed trait $name[..$tparams]",
          Defn.Object(Seq.empty, Term.Name(name.value), body)
        ))
      case slf @ Term.Block(Seq(trt: Defn.Trait, obj: Defn.Object)) =>
        defn
      case _ =>
        abort("@dataType annotation can be used with traits only")
    }
  }
}
