package regions
import sun.misc.Unsafe
import scala.collection.immutable.IntMap
import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

package internal {
  class struct extends StaticAnnotation
}

package object internal {
  private[regions] case class Node(loc: Long, var next: Node)

  val unsafe: Unsafe = {
    val f = classOf[Unsafe].getDeclaredField("theUnsafe");
    f.setAccessible(true);
    f.get(null).asInstanceOf[Unsafe]
  }

  val nodeSize = 4096
  var free: Node = null
  def retainNode(): Node = {
    if (free == null)
      allocArena()
    val res = free
    free = free.next
    res.next = null
    res
  }
  def allocArena(): Unit = {
    val nodes = 32
    val arena = unsafe.allocateMemory(nodeSize * nodes)
    var i = 0
    while (i < nodes) {
      free = Node(arena + i * nodeSize, free)
      i += 1
    }
  }
  def releaseNode(node: Node): Unit = {
    var n = node
    while (n != null) {
      val cur = n
      n = n.next
      cur.next = free
      free = cur
    }
  }

  def allocRegion(): Region = new Region(retainNode(), 0)

  def disposeRegion(region: Region): Unit = releaseNode(region.node)

  def allocMemory[T](region: Region, size: Long): Ref[T] = {
    val old = region.offset
    val offset =
      if (old + size < nodeSize) {
        region.offset = old + size
        old
      } else {
        val newnode = retainNode()
        newnode.next = region.node
        region.node = newnode
        region.offset = size
        0
      }
    new Ref[T](region.node.loc + offset)
  }

  def ensureFixedSizeAlloc[T]: Unit = macro internal.macros.ensureFixedSizeAlloc[T]
}
