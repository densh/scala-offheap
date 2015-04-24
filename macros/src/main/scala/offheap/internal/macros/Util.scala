package offheap
package internal
package macros

import scala.reflect.macros.blackbox

class Util(val c: blackbox.Context) extends Common {
  import c.universe.{ weakTypeOf => wt, _ }
  def sizeOf_[T: WeakTypeTag] = q"${sizeOf(wt[T]).toLong}"
  def sizeOfData_[T: WeakTypeTag] = q"${sizeOfData(wt[T]).toLong}"
  def offsetOf[T: WeakTypeTag](field: Tree) = {
    val q"${value: String}" = field
    wt[T] match {
      case tpe @ ClassOf(fields, _, _) =>
        fields.collectFirst {
          case f if f.name.toString == value => q"${f.offset}"
        }.getOrElse {
          abort(s"$tpe does not have field $field")
        }
      case tpe =>
        abort(s"$tpe is not an offheap class")
    }
  }
}
