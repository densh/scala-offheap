package offheap
package internal
package macros

import scala.reflect.macros.whitebox

class Region(val c: whitebox.Context) extends Common {
  import c.universe._
  import c.universe.definitions._

  def apply(f: Tree)(pool: Tree) = {
    val r = fresh("r")
    val res = fresh("res")
    val body = app(f, q"$r")
    q"""
      val $r = $RegionModule.open($pool)
      val $res =
        try $body
        finally $r.close()
      $res
    """
  }
}
