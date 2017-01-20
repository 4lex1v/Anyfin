package platform.foundation.purity

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.meta._
import scala.collection.immutable.Seq

@compileTimeOnly("@constr[T] expansion failed. Please check that the Paradise plugin enabled.")
class dataType extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case Term.Block(Seq(trt: Defn.Trait, obj: Defn.Object)) =>
        val newBody = dataType.annotate(trt.templ)
        Term.Block(Seq(
          q"sealed trait ${trt.name}[..${trt.tparams}]",
          Defn.Object(Seq.empty, obj.name, newBody)
        ))
      case Defn.Trait(mods, name, tparams, ctor, body) =>
        val newBody = dataType.annotate(body)
        Term.Block(Seq(
          q"sealed trait $name[..$tparams]",
          Defn.Object(Seq.empty, Term.Name(name.value), newBody)
        ))
      case _ =>
        abort("@dataType annotation can be used with traits only")
    }
  }
}

object dataType {
  def annotate(body: Template) = {
    body.copy(stats = body.stats.map { stats =>
      stats.map {
        case Decl.Def(mods, fName @ Term.Name(name), tparams, paramss, retType) =>
          q"@constr def $fName[..$tparams](...$paramss): $retType"
      }
    })
  }
}
