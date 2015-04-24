package offheap

import scala.language.experimental.{ macros => canMacro }
import offheap.internal.macros

package object x64 {
  type Addr = Long
  type Size = Long

  def sizeOf[T]: Size                  = macro macros.Util.sizeOf_[T]
  def sizeOfData[T]: Size              = macro macros.Util.sizeOfData_[T]
  def offsetOf[T](field: String): Size = macro macros.Util.offsetOf[T]
}
