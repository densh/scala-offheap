package test
import regions._
object OffHeap extends App {
  import regions._
  def run(n: Int) = Region { outer =>
    val minDepth = 4
    val maxDepth = n max (minDepth+2)
    val longLivedTree = tree(0,maxDepth)(outer)
    var depth = minDepth
    while (depth <= maxDepth) {
      val iterations = 1 << (maxDepth - depth + minDepth)
      var i,sum = 0
      def rsum(i: Int, depth: Int): Int = Region { r =>
        isum(tree(i, depth)(r))
      }
      while (i < iterations) {
        i += 1
        sum += rsum(i, depth) + rsum(-i, depth)
      }
      depth += 2
    }
  }
  @offheap class Tree(i: Int, left: Tree, right: Tree)
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
  while(true) run(15)
}
