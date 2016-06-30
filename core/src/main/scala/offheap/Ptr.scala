package scala.offheap

import scala.language.experimental.{ macros => canMacro }
import scala.reflect.ClassTag
import scala.offheap.internal.macros

/** C-style pointer. */
final class Ptr[T] private (val addr: Addr) extends AnyVal {

  /** Dereference a pointer. */
  def unary_!(implicit ct: ClassTag[T]): T =
    macro macros.Ptr.load[T]

  /** Store a value to the address pointed at by a pointer. */
  def `unary_!_=`(value: T)(implicit ct: ClassTag[T]): Unit =
    macro macros.Ptr.store[T]

  /** Compute a derived pointer by adding given offset. */
  def +(offset: Word)(implicit ct: ClassTag[T]): Ptr[T] =
    macro macros.Ptr.add[T]

  /** Compute a derived pointer by substricting given offset. */
  def -(offset: Word)(implicit ct: ClassTag[T]): Ptr[T] =
    macro macros.Ptr.sub[T]

  /** Read a value at given offset. Equivalent to !(offset + word). */
  def apply(offset: Word)(implicit ct: ClassTag[T]): T =
    macro macros.Ptr.apply[T]

  /** Store a value to given offset. Equivalent to !(offset + word) = value. */
  def update(offset: Word, value: T)(implicit ct: ClassTag[T]): T =
    macro macros.Ptr.update[T]
}
