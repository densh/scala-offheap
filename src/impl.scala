package offheap
package internal

import scala.language.experimental.{macros => CanMacro}
import Const._, Unsafe._, Alias._

object Region {
  import PagePool.currentPool

  final class Handle(var page: Addr, var offset: Short, val dirty: AddrStack)

  def open(): Region =
    new Region(new Handle(currentPool.get.claim, 0, new AddrStack(8)))

  def close(r: Region): Unit = {
    val h    = r.handle.asInstanceOf[Handle]
    val pool = currentPool.get
    pool.reclaim(h.page)
    pool.reclaim(h.dirty)
  }

  def allocate(r: Region, size: Size): Addr = {
    assert(size < PAGE_SIZE)
    val pool   = currentPool.get
    val h      = r.handle.asInstanceOf[Handle]
    val offset = h.offset
    val resOffset =
      if (offset + size < PAGE_SIZE) {
        h.offset = (offset + size).toShort
        offset
      } else {
        h.dirty.push(h.page)
        h.page   = pool.claim
        h.offset = size.toShort
        0
      }
    h.page + resOffset
  }
}

object Unsafe {
  val unsafe: sun.misc.Unsafe =
    try {
      sun.misc.Unsafe.getUnsafe()
    } catch {
      case _: java.lang.SecurityException =>
        val f = classOf[sun.misc.Unsafe].getDeclaredField("theUnsafe");
        f.setAccessible(true);
        f.get(null).asInstanceOf[sun.misc.Unsafe]
    }
}

object Method {
  def accessor[C, T](addr: Addr, name: String): T =
    macro macros.Method.accessor[C, T]

  def assigner[C, T](addr: Addr, name: String, value: T): Unit =
    macro macros.Method.assigner[C, T]

  def allocator[C](r: offheap.Region, args: Any*): C =
    macro macros.Method.allocator[C]

  def method[T](body: T): T =
    macro macros.Method.method[T]

  def copy[C](r: Region, args: Any*): C =
    macro macros.Method.copy[C]

  def toString[C]: String =
    macro macros.Method.toString[C]
}

class AddrStack(startingSize: Int, growthFactor: Double = 1.5) {
  private var arr = new Array[Addr](startingSize)
  private var idx = 0

  def size: Int         = idx
  def isEmpty: Boolean  = idx == 0
  def nonEmpty: Boolean = idx != 0

  def push(value: Addr): Unit =
    if (idx < arr.length) {
      arr(idx) = value
      idx += 1
    } else {
      val newarr = new Array[Long]((arr.size * growthFactor).toInt)
      System.arraycopy(arr, 0, newarr, 0, arr.size)
      arr = newarr
      push(value)
    }

  def pop: Addr = {
    assert(nonEmpty)
    idx -= 1
    arr(idx)
  }

  def merge(other: AddrStack): Unit = {
    if (idx + other.size < arr.length) {
      System.arraycopy(other.arr, 0, arr, idx, other.size)
    } else {
      val newarr = new Array[Long](((size + other.size) * growthFactor).toInt)
      System.arraycopy(arr, 0, newarr, 0, arr.size)
      System.arraycopy(other.arr, 0, newarr, arr.size, other.size)
      arr = newarr
    }
    idx += other.size
  }
}

object Alias {
  type Addr = Long
  type Size = Long
}

object Const {
  val PAGE_SIZE     = unsafe.pageSize()
  val ADDRESS_SIZE  = unsafe.addressSize()
  val CHUNK_SIZE    = PAGE_SIZE * 1024  // approx 4MB per chunk

  assert(PAGE_SIZE == 4096,
    "offheap memory is only support on systems with 4096 bytes long pages")
  assert(ADDRESS_SIZE == 8,
    "offheap memory is only supported on 64-bit systems")
  assert(CHUNK_SIZE % PAGE_SIZE == 0)
}

class PagePool {
  private val chunks = new AddrStack(startingSize = 4, growthFactor = 2)
  private val pages  = new AddrStack(startingSize = 1024, growthFactor = 2)
  private def claimChunks: Unit = {
    val chunk = unsafe.allocateMemory(CHUNK_SIZE)
    chunks.push(chunk)
    var i = 0
    while (i < CHUNK_SIZE / PAGE_SIZE) {
      pages.push(chunk + i * PAGE_SIZE)
      i += 1
    }
  }
  override protected def finalize: Unit = {
    while (chunks.nonEmpty)
      unsafe.freeMemory(chunks.pop)
  }
  def claim: Addr = {
    if (pages.isEmpty) claimChunks
    pages.pop
  }
  def reclaim(page: Addr): Unit =
    pages.push(page)
  def reclaim(pages: AddrStack) =
    pages.merge(pages)
}
object PagePool {
  val currentPool = new ThreadLocal[PagePool] {
    override protected def initialValue(): PagePool = new PagePool
  }
}
