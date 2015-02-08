package offheap
package internal

import scala.language.experimental.{macros => CanMacro}

class Ptr[T](val addr: Long) extends AnyVal {
  def apply(): T         = macro macros.Ptr.apply
  def update(v: T): Unit = macro macros.Ptr.update
  def +(n: Int): Ptr[T]  = macro macros.Ptr.+
  def -(n: Int): Ptr[T]  = macro macros.Ptr.-
  def free: Unit         = macro macros.Ptr.free
}

object Ptr {
  def alloc[T](v: T): Ptr[T] = macro macros.Ptr.alloc[T]
}
