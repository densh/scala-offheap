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

  val nodeSize = 4096000
  var free: Node = null
  def retainNode(): Node = {
    if (free != null) {
      val res = free
      free = free.next
      res.next = null
      res
    } else {
      allocArena()
      retainNode()
    }
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
    if (node.next == null) {
      node.next = free
      free = node
    } else {
      val next = node.next
      node.next = null
      releaseNode(node)
      releaseNode(next)
    }
  }

  def allocRegion(): Region = new Region(retainNode(), 0)

  def disposeRegion(region: Region): Unit = releaseNode(region.node)

  def allocMemory[T](region: Region, size: Long): Ref[T] = {
    val offset =
      if (region.offset + size < nodeSize) {
        val offset = region.offset
        region.offset = offset + size
        offset
      } else {
        val newnode = retainNode()
        newnode.next = region.node
        region.node = newnode
        0
      }
    new Ref[T](region.node.loc + offset)
  }

  def ensureFixedSizeAlloc[T]: Unit = macro internal.macros.ensureFixedSizeAlloc[T]
}
