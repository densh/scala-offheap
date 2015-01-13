package regions.internal

import sun.misc.Unsafe
import scala.collection.immutable.IntMap
import scala.annotation.StaticAnnotation
import scala.language.experimental.{macros => CanMacro}
import regions.{Region, Ref}

package runtime {
  class offheap extends StaticAnnotation
  case class Node(loc: Long, var next: Node)
}

package object runtime {
  val unsafe: Unsafe = {
    val f = classOf[Unsafe].getDeclaredField("theUnsafe");
    f.setAccessible(true);
    f.get(null).asInstanceOf[Unsafe]
  }

  val PAGE_SIZE         = unsafe.pageSize()
  val NODE_PAYLOAD_SIZE = 409600
  val ARENA_NODE_COUNT  = 32
  val ARENA_SIZE        = NODE_PAYLOAD_SIZE * ARENA_NODE_COUNT

  assert(NODE_PAYLOAD_SIZE % PAGE_SIZE == 0)
  assert(ARENA_SIZE % NODE_PAYLOAD_SIZE == 0)

  var free: Node = null
  var regions: Array[Region] = (1 to 16).map { _ => new Region(null, 0) }.toArray
  var regionNext: Int = 0

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

  def allocClass[T](r: Region, args: Any*): _root_.regions.Ref[T] =
    macro macros.Runtime.allocClass[T]
}
