package scala.offheap
package internal
package macros

import scala.reflect.macros.whitebox

class Region(val c: whitebox.Context) extends Common {
  import c.universe._
  import c.universe.definitions._

  def apply(f: Tree)(props: Tree) = {
    val r    = freshVal("r", RegionClass.toType, q"$props.open()")
    val res  = fresh("res")
    val body = app(f, q"${r.symbol}")
    q"""
      $r
      val $res =
        try $body
        finally ${r.symbol}.close()
      $res
    """
  }
}
