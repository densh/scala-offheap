package offheap

import scala.language.experimental.{macros => CanMacro}
import offheap.internal.macros

final class Array[A] private (val $addr: Addr) extends AnyVal {
  def isEmpty: Boolean                                   = macro macros.Array.isEmpty
  def nonEmpty: Boolean                                  = macro macros.Array.nonEmpty
  def size: Array.Size                                   = macro macros.Array.size
  def length: Array.Size                                 = macro macros.Array.size
  def apply(index: Addr): A                              = macro macros.Array.apply
  def update(index: Addr, value: A): Unit                = macro macros.Array.update
  def foreach(f: A => Unit): Unit                        = macro macros.Array.foreach
  def map[B](f: A => B)(implicit a: Allocator): Array[B] = macro macros.Array.map[B]

  override def toString =
    if ($addr == 0L) s"offheap.x64.Array.empty"
    else super.toString
}
object Array {
  type Size = Int
  def uninit[T](n: Size)(implicit a: Allocator): Array[T]    = macro macros.Array.uninit[T]
  def apply[T](values: T*)(implicit a: Allocator): Array[T]  = macro macros.Array.vararg[T]
  def fill[T](n: Size)(elem: => T)
             (implicit a: Allocator): Array[T]               = macro macros.Array.fill[T]
  def copy[T](from: Array[T], fromIndex: Addr,
              to: Array[T], toIndex: Addr, size: Size): Unit = macro macros.Array.copy[T]
  def empty[T]: Array[T]                                     = new Array[T](0L)
  def fromAddr[T](addr: Addr): Array[T]                      = new Array[T](addr)
  def toAddr[T](arr: Array[T]): Addr                         = arr.$addr
}
