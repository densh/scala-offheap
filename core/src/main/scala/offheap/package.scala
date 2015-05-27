import scala.language.experimental.{ macros => canMacro }
import offheap.internal.macros

package object offheap {
  type Addr = Long
  type Size = Long

  def alignmentOf[T]: Size             = macro macros.Util.alignmentOf_[T]
  def alignmentOfEmbed[T]: Size        = macro macros.Util.alignmentOfEmbed_[T]
  def sizeOf[T]: Size                  = macro macros.Util.sizeOf_[T]
  def sizeOfEmbed[T]: Size             = macro macros.Util.sizeOfEmbed_[T]
  def offsetOf[T](field: String): Size = macro macros.Util.offsetOf[T]
  def strideOf[T]: Size                = macro macros.Util.strideOf[T]
}
