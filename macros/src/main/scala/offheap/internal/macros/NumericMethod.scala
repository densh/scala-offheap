package scala.offheap
package internal
package macros

import scala.collection.mutable
import scala.reflect.macros.blackbox

class NumericMethod(val c: blackbox.Context) extends Common {
  import c.universe._

  val MatrixClass = rootMirror.staticClass("scala.offheap.numeric.Matrix")

  // TODO: add other ops (matrix, scalar, vector)
  sealed abstract class Op
  final case class MatrixMultiply(left: Op, right: Op) extends Op
  final case class MatrixLeaf(tree: Tree) extends Op

  // TODO: add other type tests
  def isMatrix(tpe: Type): Boolean = tpe <:< MatrixClass.toType

  def toOp(tree: Tree): Op = {
    val q"{ ..$init; $last }" = tree
    val bindings: Map[Symbol, Tree] = init.map {
      case vd @ q"val $_: $_ = $rhs" =>
        (vd.symbol, rhs)
    }.toMap
    // TODO: other conversions
    def loop(expr: Tree): Op = expr match {
      case id: RefTree if bindings.contains(id.symbol) =>
        ???
      case id: RefTree =>
        if (isMatrix(id.tpe)) MatrixLeaf(id)
        else ???
      case q"$a * $b" =>
        if (isMatrix(b.tpe)) MatrixMultiply(loop(a), loop(b))
        else ???
      case e =>
        throw new Exception("unknown expr")
    }

    loop(last)
  }

  // TODO: preserve original names?
  def toTree(op: Op): Tree = {
    var schedule = List.empty[Tree]
    val scheduleMap = mutable.Map.empty[Op, TermName]
    def loop(op: Op): TermName =
      if (scheduleMap.contains(op))
        scheduleMap(op)
      else
        op match {
          case MatrixLeaf(tree) =>
            val name = fresh("matrix_leaf")
            schedule = q"val $name = $tree" :: schedule
            scheduleMap += ((op, name))
            name
          case MatrixMultiply(left, right) =>
            val leftname = loop(left)
            val rightname = loop(right)
            val name = fresh("matrix_multiply")
            schedule = q"val $name = $leftname * $rightname" :: schedule
            scheduleMap += ((op, name))
            name
        }

    val last = loop(op)
    q"{ ..${schedule.reverse}; $last }"
  }

  // TODO: Global Value Numbering
  // TODO: Common Subexpression Elimination
  // TODO: Strength Reduction
  // TODO: Eliminate Intermediate Results
  def optimise(op: Op): Op = op

  def opt(t: Tree): Tree = {
    val op = toOp(t)
    println(s"parsed op: $op")
    val optOp = optimise(op)
    println(s"optimised op: $optOp")
    val res = toTree(optOp)
    println(s"macro result: ${showCode(res)}")
    res
  }
}
