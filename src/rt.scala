package regions.internal

import sun.misc.Unsafe
import scala.collection.immutable.IntMap
import scala.annotation.StaticAnnotation
import scala.language.experimental.{macros => CanMacro}
import regions.{Region, Ref}

package rt {
  final case class Node(loc: Long, var next: Node)
  final class Offheap extends StaticAnnotation
  final class Layout(fields: (String, Tag[_])*) extends StaticAnnotation
  final class Tag[T]()
}

package object rt {
  val unsafe: Unsafe = {
    val f = classOf[Unsafe].getDeclaredField("theUnsafe");
    f.setAccessible(true);
    f.get(null).asInstanceOf[Unsafe]
  }

  val PAGE_SIZE         = unsafe.pageSize()
  val NODE_PAYLOAD_SIZE = PAGE_SIZE * 10
  val ARENA_NODE_COUNT  = 32
  val ARENA_SIZE        = NODE_PAYLOAD_SIZE * ARENA_NODE_COUNT

  assert(NODE_PAYLOAD_SIZE % PAGE_SIZE == 0)
  assert(ARENA_SIZE % NODE_PAYLOAD_SIZE == 0)

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
    val arena = unsafe.allocateMemory(ARENA_SIZE)
    var i = 0
    while (i < ARENA_NODE_COUNT) {
      free = Node(arena + i * NODE_PAYLOAD_SIZE, free)
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

  def allocRegion(): Region[_] = {
    new Region[Int](retainNode(), 0)
  }

  def disposeRegion(region: Region[_]): Unit = {
    releaseNode(region.node)
    region.node = null
  }

  def allocMemory(region: Region[_], size: Long): Long = {
    val old = region.offset
    val offset =
      if (old + size < NODE_PAYLOAD_SIZE) {
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
