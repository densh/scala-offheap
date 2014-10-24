package regions.internal

import sun.misc.Unsafe
import scala.collection.immutable.IntMap
import scala.annotation.StaticAnnotation
import regions.{Region, Ref}

package runtime {
  class struct extends StaticAnnotation
  class union extends StaticAnnotation
  case class Node(loc: Long, var next: Node)
}

package object runtime {
  val unsafe: Unsafe = {
    val f = classOf[Unsafe].getDeclaredField("theUnsafe");
    f.setAccessible(true);
    f.get(null).asInstanceOf[Unsafe]
  }

  val nodeSize = 409600
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

  var regions: Array[Region] = (1 to 16).map { _ => new Region(null, 0) }.toArray
  var regionNext: Int = 0

  def allocRegion(): Region = {
    val region = regions(regionNext)
    regionNext += 1
    region.node = retainNode()
    region.offset = 0
    region
  }

  def disposeRegion(region: Region): Unit = {
    releaseNode(region.node)
    region.node = null
    regionNext -= 1
  }

  def allocMemory(region: Region, size: Long): Long = {
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
    region.node.loc + offset
  }
}
