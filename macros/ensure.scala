package regions.internal

import scala.language.experimental.{macros => CanMacro}

object ensure {
  def allocatable[T]: Unit = macro macros.ensure.allocatable[T]
}
