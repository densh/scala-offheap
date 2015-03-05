package test
import offheap.x64._
object Offheap extends App{
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
        r.close
        res
      }
      while (i < iterations) {
        i += 1
        sum += rsum(i, depth) + rsum(-i, depth)
      }
      depth += 2
    }
    outer.close
  }
  @offheap case class Tree(i: Int, left: Tree, right: Tree)
  def isum(tree: Tree): Int = {
    val left = tree.left
    if (left.isEmpty) tree.i
    else tree.i + isum(left) - isum(tree.right)
  }
  def tree(i: Int, depth: Int)(implicit region: Region): Tree = {
    if (depth > 0) Tree(i, tree(i*2-1, depth-1), tree(i*2, depth-1))
    else Tree(i, Tree.empty, Tree.empty)
  }
  while(true) run(20)
}

