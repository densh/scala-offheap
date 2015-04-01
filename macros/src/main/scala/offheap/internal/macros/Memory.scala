package offheap
package internal
package macros

import scala.reflect.macros.blackbox

class Memory(val c: blackbox.Context) extends Common {
  import c.universe._
  def sizeOf_[T: WeakTypeTag] = q"${sizeOf(weakTypeOf[T]).toLong}"
  def sizeOfData_[T: WeakTypeTag] = q"${sizeOfData(weakTypeOf[T]).toLong}"
}
