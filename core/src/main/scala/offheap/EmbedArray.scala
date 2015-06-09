package scala.offheap

import scala.language.experimental.{macros => CanMacro}
import offheap.internal.macros

/** An alternative implemenation of an array that inlines
 *  allocation of given offheap class into the array layout.
 *  This effectively makes it an array of structs in C sense.
 */
final class EmbedArray[A] private (val addr: Addr) extends AnyVal {
  def isEmpty: Boolean                                        = macro macros.EmbedArrayApi.isEmpty
  def nonEmpty: Boolean                                       = macro macros.EmbedArrayApi.nonEmpty
  def size: EmbedArray.Size                                   = macro macros.EmbedArrayApi.size
  def length: EmbedArray.Size                                 = macro macros.EmbedArrayApi.size
  def apply(index: Addr): A                                   = macro macros.EmbedArrayApi.apply
  def update(index: Addr, value: A): Unit                     = macro macros.EmbedArrayApi.update
  def foreach(f: A => Unit): Unit                             = macro macros.EmbedArrayApi.foreach
  def map[B](f: A => B)(implicit a: Allocator): EmbedArray[B] = macro macros.EmbedArrayApi.map[B]
  def todArray: scala.Array[A]                                = macro macros.EmbedArrayApi.toArray
  def clone(implicit a: Allocator): EmbedArray[A]             = macro macros.EmbedArrayApi.clone_

  override def toString =
    if (addr == 0L) s"offheap.x64.EmbedArray.empty"
    else super.toString
}
object EmbedArray {
  type Size = Int
  def uninit[T](n: Size)(implicit a: Allocator): EmbedArray[T]    = macro macros.EmbedArrayModule.uninit[T]
  def apply[T](values: T*)(implicit a: Allocator): EmbedArray[T]  = macro macros.EmbedArrayModule.vararg[T]
  def fill[T](n: Size)(elem: => T)
             (implicit a: Allocator): EmbedArray[T]               = macro macros.EmbedArrayModule.fill[T]
  def copy[T](from: EmbedArray[T], fromIndex: Addr,
              to: EmbedArray[T], toIndex: Addr, size: Size): Unit = macro macros.EmbedArrayModule.copy[T]
  def fromArray[T](arr: scala.Array[T])
                  (implicit a: Allocator): EmbedArray[T]          = macro macros.EmbedArrayModule.fromArray[T]

  def empty[T]: EmbedArray[T]                                     = new EmbedArray[T](0L)
  def fromAddr[T](addr: Addr): EmbedArray[T]                      = new EmbedArray[T](addr)
}
