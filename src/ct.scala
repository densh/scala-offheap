package regions.internal

import scala.annotation.compileTimeOnly
import scala.language.experimental.{macros => CanMacro}

package object ct {
  def assertAllocatable[T]: Unit =
    macro macros.Ct.assertAllocatable[T]

  def uncheckedAccessor[C, T](addr: Long, name: String): T =
    macro macros.Ct.uncheckedAcessor[C, T]

  def uncheckecMethodBody[C, T](body: T): T =
    macro macros.Ct.uncheckedMethodBody[C, T]

  def allocClass[C](r: regions.Region, args: Any*): C =
    macro macros.Ct.allocClass[C]
}
