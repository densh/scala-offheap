package regions.internal

import scala.language.experimental.macros

object ensure {
  def allocatable[T]: Unit = macro macros.ensureAllocatable[T]
}
