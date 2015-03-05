package offheap.test.jmh

import org.openjdk.jmh.annotations._
import offheap.x64._

@State(Scope.Thread)
class GCBinaryTree {
  @Param(Array("16", "18", "20"))
  var n: Int = _

  @Benchmark
  def run = GCHeap.run(n)
}

@State(Scope.Thread)
class OffheapBinaryTree {
  @Param(Array("16", "18", "20"))
  var n: Int = _

  @Benchmark
  def run = Offheap.run(n)
}

object GCHeap {
  def run(n: Int) = {
    val minDepth = 4
    val maxDepth = n max (minDepth+2)
    val longLivedTree = Tree(0,maxDepth)
    var depth = minDepth
    while (depth <= maxDepth) {
      val iterations = 1 << (maxDepth - depth + minDepth)
      var i,sum = 0
      while (i < iterations) {
        i += 1
        sum += Tree(i,depth).isum + Tree(-i,depth).isum
      }
      depth += 2
    }
  }
  final class Tree(i: Int, left: Tree, right: Tree) {
    def isum: Int = {
      val tl = left
      if (tl eq null) i
      else i + tl.isum - right.isum
    }
  }
  object Tree {
    def apply(i: Int, depth: Int): Tree = {
      if (depth > 0) new Tree(i, Tree(i*2-1, depth-1), Tree(i*2, depth-1))
      else new Tree(i, null, null)
    }
  }
}

object Offheap {
  implicit val pool = Pool(UnsafeMemory)
  def run(n: Int) = {
    val outer = Region.open
    val minDepth = 4
    val maxDepth = n max (minDepth+2)
    val longLivedTree = tree(0,maxDepth)(outer)
    var depth = minDepth
    while (depth <= maxDepth) {
      val iterations = 1 << (maxDepth - depth + minDepth)
      var i,sum = 0
      def rsum(i: Int, depth: Int): Int = {
        val r = Region.open
        val res = isum(tree(i, depth)(r))
        r.close()
        res
      }
      while (i < iterations) {
        i += 1
        sum += rsum(i, depth) + rsum(-i, depth)
      }
      depth += 2
    }
    outer.close()
  }
  @offheap case class Tree(i: Int, left: Tree, right: Tree)
  def isum(tree: Tree): Int = {
    val left = tree.left
    if (left.isEmpty) tree.i
    else tree.i + isum(left) - isum(tree.right)
  }
  def tree(i: Int, depth: Int)(implicit region: Region): Tree = {
    if (depth > 0) {
      val left = tree(i*2-1, depth-1)
      val right = tree(i*2, depth-1)
      Tree(i, left, right)
    } else Tree(i, Tree.empty, Tree.empty)
  }
}
