package offheap
package internal
package macros

import scala.reflect.macros.blackbox

class Memory(val c: blackbox.Context) extends Common {
  import c.universe._
  def sizeof_[T: WeakTypeTag] = q"${sizeof(weakTypeOf[T]).toLong}"
}
