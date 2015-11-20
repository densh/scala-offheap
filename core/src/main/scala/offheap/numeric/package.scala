package scala.offheap

import scala.offheap.internal.macros

package object numeric {
  import scala.language.experimental.{macros => canMacro}

  def opt[T](t: T): T = macro macros.NumericMethod.opt
}
