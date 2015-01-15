package regions.internal

import scala.annotation.compileTimeOnly
import scala.language.experimental.{macros => CanMacro}

package object ct {
  def allocatable[T]: Unit = macro macros.Ct.allocatable[T]

  @compileTimeOnly("uninterpreted ct.ref")
  def ref[T](ref: _root_.regions.Ref[T]): T = ???
}
