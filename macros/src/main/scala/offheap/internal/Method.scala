package offheap
package internal

import scala.language.experimental.{macros => CanMacro}

object Method {
  def accessor[C, T](addr: Any, name: String): T =
    macro macros.Method.accessor[C, T]

  def assigner[C, T](addr: Any, name: String, value: T): Unit =
    macro macros.Method.assigner[C, T]

  def allocator[C](memory: Any, args: Any*): C =
    macro macros.Method.allocator[C]

  def toString[C](self: C): String =
    macro macros.Method.toString[C]
}
