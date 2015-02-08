package offheap
package internal

import scala.language.experimental.{macros => CanMacro}

object Method {
  def accessor[C, T](addr: Addr, name: String): T =
    macro macros.Method.accessor[C, T]

  def assigner[C, T](addr: Addr, name: String, value: T): Unit =
    macro macros.Method.assigner[C, T]

  def allocator[C](r: offheap.Region, args: Any*): C =
    macro macros.Method.allocator[C]

  def method[T](body: T): T =
    macro macros.Method.method[T]

  def copy[C](r: Region, args: Any*): C =
    macro macros.Method.copy[C]

  def toString[C]: String =
    macro macros.Method.toString[C]
}
