package offheap
package internal

import scala.language.experimental.{macros => CanMacro}

class Ptr[T](val addr: Long) extends AnyVal {
  def apply(): T                  = macro macros.Ptr.apply
  def apply(n: Long): T           = macro macros.Ptr.applyN
  def update(v: T): Unit          = macro macros.Ptr.update
  def update(n: Long, v: T): Unit = macro macros.Ptr.updateN
  def free: Unit                  = macro macros.Ptr.free
  def resize(n: Long): Ptr[T]     = macro macros.Ptr.resize
  def unary_! : T                 = macro macros.Ptr.unary_!
  def `unary_!_=`(v: T): Unit     = macro macros.Ptr.`unary_!_=`
  def +(n: Long): Ptr[T]          = macro macros.Ptr.+
  def -(n: Long): Ptr[T]          = macro macros.Ptr.-
}

object Ptr {
  def alloc[T](n: Long = 1): Ptr[T] =
    macro macros.Ptr.alloc[T]

  def copy[T](from: Ptr[T], fromIndex: Long,
              to: Ptr[T], toIndex: Long, length: Long): Unit =
    macro macros.Ptr.copy[T]
}
