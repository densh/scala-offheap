package scala.offheap

import scala.language.experimental.{macros => CanMacro}
import offheap.internal.macros

/** Off-heap equivalent of `scala.Array`. Can only be used
 *  with statically known values of type parameter which also
 *  has to be off-heap allocatable.
 *
 *  All combinator methods are implemented through macros and
 *  attempt to eliminate closures that are passed to them whenever
 *  possible.
 */
final class Array[A] private (val addr: Addr) extends AnyVal {
  def isEmpty: Boolean                                   = macro macros.ArrayApi.isEmpty
  def nonEmpty: Boolean                                  = macro macros.ArrayApi.nonEmpty
  def size: Array.Size                                   = macro macros.ArrayApi.size
  def length: Array.Size                                 = macro macros.ArrayApi.size
  def apply(index: Array.Index): A                       = macro macros.ArrayApi.apply
  def update(index: Array.Index, value: A): Unit         = macro macros.ArrayApi.update
  def foreach(f: A => Unit): Unit                        = macro macros.ArrayApi.foreach
  def map[B](f: A => B)(implicit a: Allocator): Array[B] = macro macros.ArrayApi.map[B]
  def toArray: scala.Array[A]                            = macro macros.ArrayApi.toArray
  def clone(implicit a: Allocator): Array[A]             = macro macros.ArrayApi.clone_

  override def toString =
    if (addr == 0L) s"offheap.x64.Array.empty"
    else super.toString
}
object Array {
  type Index = Int
  type Size = Int
  def uninit[T](n: Array.Size)
               (implicit a: Allocator): Array[T]      = macro macros.ArrayModule.uninit[T]
  def apply[T](values: T*)
              (implicit a: Allocator): Array[T]       = macro macros.ArrayModule.vararg[T]
  def fill[T](n: Array.Size)(elem: => T)
             (implicit a: Allocator): Array[T]        = macro macros.ArrayModule.fill[T]
  def copy[T](from: Array[T], fromIndex: Array.Index,
              to: Array[T], toIndex: Array.Index,
              size: Array.Size): Unit                 = macro macros.ArrayModule.copy[T]
  def fromArray[T](arr: scala.Array[T])
                  (implicit a: Allocator): Array[T]   = macro macros.ArrayModule.fromArray[T]

  def empty[T]: Array[T]                              = new Array[T](0L)
  def fromAddr[T](addr: Addr): Array[T]               = new Array[T](addr)
}
