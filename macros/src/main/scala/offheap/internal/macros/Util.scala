package scala.offheap
package internal
package macros

import scala.reflect.macros.blackbox

class Util(val c: blackbox.Context) extends Common {
  import c.universe.{ weakTypeOf => wt, _ }

  def alignmentOf_[T: WeakTypeTag] = q"${alignmentOf(wt[T])}"
  def sizeOf_[T: WeakTypeTag]      = q"${sizeOf(wt[T])}"
}
