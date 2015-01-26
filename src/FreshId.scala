package regions

import scala.language.experimental.{macros => CanMacro}
import regions.internal.macros

trait FreshId[Id <: Int]
object FreshId {
  implicit def materialize: FreshId[_ <: Int] = macro macros.FreshId.materialize
}


