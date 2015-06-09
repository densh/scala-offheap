package scala.offheap
package internal

import scala.language.experimental.{macros => CanMacro}

object Method {
  def access[C, T](self: Any, name: String): T =
    macro macros.Method.accessor[C, T]

  def assign[C, T](self: Any, name: String, value: T): Unit =
    macro macros.Method.assigner[C, T]

  def toString[C](self: C): String =
    macro macros.Method.toString[C]
}
