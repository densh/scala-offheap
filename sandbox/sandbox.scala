package test
import offheap._, x64._
@data class OhTree(i: Int, left: OhTree, right: OhTree)
object Offheap extends App {
  implicit val pool = Pool(Memory())
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
  def isum(tree: OhTree): Int = {
    val left = tree.left
    if (left.isEmpty) tree.i
    else tree.i + isum(left) - isum(tree.right)
  }
  def tree(i: Int, depth: Int)(implicit region: Region): OhTree = {
    if (depth > 0) OhTree(i, tree(i*2-1, depth-1), tree(i*2, depth-1))
    else OhTree(i, OhTree.empty, OhTree.empty)
  }
  while (true) run(20)
}
