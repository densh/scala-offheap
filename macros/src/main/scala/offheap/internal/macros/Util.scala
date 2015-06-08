package scala.offheap
package internal
package macros

import scala.reflect.macros.blackbox

class Util(val c: blackbox.Context) extends Common {
  import c.universe.{ weakTypeOf => wt, _ }

  def offsetOf[T: WeakTypeTag](field: Tree) = {
    val q"${value: String}" = field
    wt[T] match {
      case tpe @ Clazz(clazz) =>
        clazz.fields.collectFirst {
          case f if f.name.toString == value => q"${f.offset}"
        }.getOrElse {
          abort(s"$tpe does not have field $field")
        }
      case tpe =>
        abort(s"$tpe is not an offheap class")
    }
  }

  def alignmentOf_[T: WeakTypeTag]      = q"${alignmentOf(wt[T])}"
  def alignmentOfEmbed_[T: WeakTypeTag] = q"${alignmentOfEmbed(wt[T])}"
  def sizeOf_[T: WeakTypeTag]           = q"${sizeOf(wt[T])}"
  def sizeOfEmbed_[T: WeakTypeTag]      = q"${sizeOfEmbed(wt[T])}"
  def strideOf_[T: WeakTypeTag]         = q"${strideOf(wt[T], isEmbed = false)}"
  def strideOfEmbed_[T: WeakTypeTag]    = q"${strideOf(wt[T], isEmbed = true)}"
}
