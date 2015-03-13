package offheap
package x64

import scala.language.experimental.{macros => CanMacro}
import offheap.internal.macros

final class Array[A] private (val $ref: Ref) extends AnyVal {
  def isEmpty: Boolean                                = macro macros.Array.isEmpty
  def nonEmpty: Boolean                               = macro macros.Array.nonEmpty
  def size: Size                                      = macro macros.Array.size
  def length: Size                                    = macro macros.Array.size
  def apply(index: Addr): A                           = macro macros.Array.apply
  def update(index: Addr, value: A): Unit             = macro macros.Array.update
  def foreach(f: A => Unit): Unit                     = macro macros.Array.foreach
  def map[B](f: A => B)(implicit m: Memory): Array[B] = macro macros.Array.map[B]

  override def toString =
    if ($ref == null) s"offheap.x64.Array.empty"
    else super.toString
}
object Array {
  def uninit[T](n: Size)(implicit m: Memory): Array[T]       = macro macros.Array.uninit[T]
  def apply[T](values: T*)(implicit m: Memory): Array[T]     = macro macros.Array.vararg[T]
  def fill[T](n: Size)(elem: => T)
             (implicit m: Memory): Array[T]                  = macro macros.Array.fill[T]
  def copy[T](from: Array[T], fromIndex: Addr,
              to: Array[T], toIndex: Addr, size: Size): Unit = macro macros.Array.copy[T]
  def empty[T]: Array[T]                                     = new Array[T](null)
  def fromRef[T](ref: Ref): Array[T]                         = new Array[T](ref)
  def toRef[T](arr: Array[T]): Ref                           = arr.$ref
}
