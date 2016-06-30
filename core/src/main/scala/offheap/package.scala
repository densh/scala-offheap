package scala

import scala.language.experimental.{ macros => canMacro }
import scala.offheap.internal.macros

package object offheap {
  type Word = Long

  /** Physical address representation.
   *  It's always an alias to Long on JVM but
   *  can also be Int on other platforms (e.g. on JS)
   */
  type Addr = Word

  /** Physical memory size representation
   *  It's always an alias to Long on JVM but
   *  can also be Int on other platforms (e.g. on JS)
   */
  type Size = Word

  /** Alignment of given type `T` when used as a field in offheap class. */
  def alignmentOf[T]: Size = macro macros.Util.alignmentOf_[T]

  /** Size of given type `T` when used as a field in offheap class. */
  def sizeOf[T]: Size = macro macros.Util.sizeOf_[T]
}
