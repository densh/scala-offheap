object OffHeap extends App {
  import regions._
  def run(n: Int) = Region { outer =>
    val minDepth = 4
    val maxDepth = n max (minDepth+2)
    val longLivedTree = tree(0,maxDepth)(outer)
    var depth = minDepth
    while (depth <= maxDepth) Region { implicit inner =>
      val iterations = 1 << (maxDepth - depth + minDepth)
      var i,sum = 0
      while (i < iterations) {
        i += 1
        sum += isum(tree(i,depth)(outer)) +
               isum(tree(-i,depth)(outer))
      }
      depth += 2
    }
  }
  @struct class Tree(i: Int, left: Ref[Tree], right: Ref[Tree])
  def isum(tree: Ref[Tree]): Int = {
    val left = tree.left
    if (left.isEmpty) tree.i
    else tree.i + isum(left) - isum(tree.right)
  }
  def tree(i: Int, depth: Int)(implicit region: Region): Ref[Tree] = {
    if (depth > 0) {
      val left = tree(i*2-1, depth-1)
      val right = tree(i*2, depth-1)
      Ref[Tree](i, left, right)
    } else Ref[Tree](i, Ref.empty[Tree], Ref.empty[Tree])
  }
  while(true) run(16)
}
