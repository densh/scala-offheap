package scala.offheap
package internal
package macros

import scala.reflect.macros.blackbox

class Ptr(val c: blackbox.Context) extends Common {
  import c.universe.{ weakTypeOf => wt, _ }

  def load[T](ct: Tree): Tree = q"???"
  def store[T](value: Tree)(ct: Tree): Tree = q"???"
  def add[T](offset: Tree)(ct: Tree): Tree = q"???"
  def sub[T](offset: Tree)(ct: Tree): Tree = q"???"
  def apply[T](offset: Tree)(ct: Tree): Tree = q"???"
  def update[T](offset: Tree, value: Tree)(ct: Tree): Tree = q"???"
}
