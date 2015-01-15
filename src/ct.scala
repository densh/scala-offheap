package regions.internal

import scala.language.experimental.{macros => CanMacro}

package object ct {
  def allocatable[T]: Unit = macro macros.Ct.allocatable[T]
  def ref[T](ref: _root_.regions.Ref[T]): T = ???
  def lit[T](t: T): T = ???
}
