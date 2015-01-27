package regions.internal

import scala.annotation.compileTimeOnly
import scala.language.experimental.{macros => CanMacro}
import regions.{Region, Ref}

package object ct {
  def assertAllocatable[T]: Unit =
    macro macros.Ct.assertAllocatable[T]

  def uncheckedAccessor[C, T](addr: Long, name: String): T =
    macro macros.Ct.uncheckedAcessor[C, T]

  def allocClass[R <: Region[_], C <: Ref[_]](r: R, args: Any*): C =
    macro macros.Ct.allocClass[C]
}
