package scala

import scala.language.experimental.{ macros => canMacro }
import offheap.internal.macros

package object offheap {
  /** Physical address representation.
   *  It's always an alias to Long on JVM but
   *  can also be Int on other platforms (e.g. on JS)
   */
  type Addr = Long

  /** Physical memory size representation
   *  It's always an alias to Long on JVM but
   *  can also be Int on other platforms (e.g. on JS)
   */
  type Size = Long

  /** Offset of given `field` in offheap class `T`. */
  def offsetOf[T](field: String): Size = macro macros.Util.offsetOf[T]

  /** Alignment of given type `T` when used as a field in offheap class. */
  def alignmentOf[T]: Size             = macro macros.Util.alignmentOf_[T]

  /** Alignment of given type `T` when used as an embed field in offheap class. */
  def alignmentOfEmbed[T]: Size        = macro macros.Util.alignmentOfEmbed_[T]

  /** Size of given type `T` when used as a field in offheap class. */
  def sizeOf[T]: Size                  = macro macros.Util.sizeOf_[T]

  /** Size of given type `T` when used as an embed field in offheap class. */
  def sizeOfEmbed[T]: Size             = macro macros.Util.sizeOfEmbed_[T]

  /** Size of memory between successive elements in `Array[T]`. */
  def strideOf[T]: Size                = macro macros.Util.strideOf_[T]

  /** Size of memory between successive elements in `EmbedArray[T]`. */
  def strideOfEmbed[T]: Size           = macro macros.Util.strideOfEmbed_[T]
}
