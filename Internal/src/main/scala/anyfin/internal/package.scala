package anyfin

import scala.collection.immutable.Seq
import scala.meta.{Term, Type}

package object internal {

  type TParams = Seq[Type.Param]
  type Paramss = Seq[Seq[Term.Param]]

}
