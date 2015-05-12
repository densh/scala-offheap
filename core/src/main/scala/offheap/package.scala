import scala.language.experimental.{ macros => canMacro }
import offheap.internal.macros

package object offheap {
  type Addr = Long
  type Size = Long

  def alignmentOf[T]: Size             = macro macros.Util.alignmentOf_[T]
  def alignmentOfData[T]: Size         = macro macros.Util.alignmentOfData_[T]
  def sizeOf[T]: Size                  = macro macros.Util.sizeOf_[T]
  def sizeOfData[T]: Size              = macro macros.Util.sizeOfData_[T]
  def offsetOf[T](field: String): Size = macro macros.Util.offsetOf[T]
}
