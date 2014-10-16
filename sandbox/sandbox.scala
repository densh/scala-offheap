object OffHeap extends App {
  import regions._
  def run(n: Int) = {
    val outer = Region()
    val minDepth = 4
    val maxDepth = n max (minDepth+2)
    val longLivedTree = tree(0,maxDepth)(outer)
    var depth = minDepth
    while (depth <= maxDepth) {
      val iterations = 1 << (maxDepth - depth + minDepth)
      var i,sum = 0
      def rsum(i: Int, depth: Int): Int = {
        val r = Region()
        val res = isum(tree(i, depth)(r))
        r.dispose()
        res
      }
      while (i < iterations) {
        i += 1
        sum += rsum(i, depth) + rsum(-i, depth)
      }
      depth += 2
    }
    outer.dispose()
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

/*
object GCHeap extends App {
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
  run(20)
}
*/
