package regions.internal

import scala.language.experimental.{macros => CanMacro}

object Ensure {
  def allocatable[T]: Unit = macro macros.Ensure.allocatable[T]
}
