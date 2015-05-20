package offheap
package internal

import scala.language.experimental.{macros => CanMacro}

object Method {
  def access[C, T](addr: Any, name: String): T =
    macro macros.Method.accessor[C, T]

  def assign[C, T](addr: Any, name: String, value: T): Unit =
    macro macros.Method.assigner[C, T]

  def toString[C](self: C): String =
    macro macros.Method.toString[C]
}
