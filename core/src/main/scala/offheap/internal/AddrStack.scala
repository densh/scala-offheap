package offheap
package internal

import C._
import Unsafe.unsafe

@struct class AddrStack(arrSize: Long, arr: Ptr[Long], size: Long)
object AddrStack {
  final val GROWTH_FACTOR = 2

  def alloc(startingSize: Long): Ptr[AddrStack] = {
    val ptr = Ptr.alloc[AddrStack](1)
    ptr.arrSize = startingSize
    ptr.arr = Ptr.alloc[Long](startingSize)
    ptr.size = 0L
    ptr
  }

  def isEmpty(stack: Ptr[AddrStack]): Boolean = stack.size == 0

  def nonEmpty(stack: Ptr[AddrStack]): Boolean = stack.size != 0

  def push(stack: Ptr[AddrStack], value: Addr): Unit = {
    if (stack.size >= stack.arrSize) {
      stack.arrSize = (stack.arrSize * GROWTH_FACTOR).toLong
      stack.arr     = stack.arr.resize(stack.arrSize)
    }
    stack.arr(stack.size) = value
    stack.size = stack.size + 1
  }

  def pop(stack: Ptr[AddrStack]): Addr = {
    val newsize = stack.size - 1
    stack.size = newsize
    stack.arr(newsize)
  }

  def merge(stack: Ptr[AddrStack], other: Ptr[AddrStack]): Unit = {
    if (stack.size + other.size >= stack.arrSize) {
      stack.arrSize = ((stack.size + other.size) * GROWTH_FACTOR).toLong
      stack.arr     = stack.arr.resize(stack.arrSize)
    }
    Ptr.copy(other.arr, 0, stack.arr, stack.size, other.size)
    stack.size = stack.size + other.size
  }

  def free(stack: Ptr[AddrStack]): Unit = {
    stack.arr.free
    stack.free
  }
}

final class AddrStackPagePool {
  protected val chunks = AddrStack.alloc(startingSize = 4)
  protected val pages  = AddrStack.alloc(startingSize = 1024)

  override protected def finalize: Unit = {
    while (AddrStack.nonEmpty(chunks))
      unsafe.freeMemory(AddrStack.pop(chunks))
    AddrStack.free(chunks)
    AddrStack.free(pages)
  }

  protected def claimChunks: Unit = {
    val chunk = unsafe.allocateMemory(CHUNK_SIZE)
    AddrStack.push(chunks, chunk)
    var i = 0
    while (i < CHUNK_SIZE / PAGE_SIZE) {
      AddrStack.push(pages, chunk + i * PAGE_SIZE)
      i += 1
    }
  }

  def claim: Addr = {
    if (AddrStack.isEmpty(pages)) claimChunks
    AddrStack.pop(pages)
  }

  def reclaim(page: Addr): Unit = {
    AddrStack.push(pages, page)
  }

  def reclaimStack(otherPages: Ptr[AddrStack]) = this.synchronized {
    AddrStack.merge(pages, otherPages)
  }
}
object AddrStackPagePool {
  val currentPool = new ThreadLocal[AddrStackPagePool] {
    override protected def initialValue(): AddrStackPagePool = new AddrStackPagePool
  }
}

final class AddrStackRegion extends offheap.Region {
  private var pool   = AddrStackPagePool.currentPool.get
  private var dirty  = AddrStack.alloc(8)
  private var page   = pool.claim
  private var offset = 0

  protected[internal] def isOpen = pool != null

  protected[internal] def close(): Unit = {
    assert(isOpen)
    pool.reclaim(page)
    pool.reclaimStack(dirty)
    AddrStack.free(dirty)
    pool = null
  }

  protected[internal] def allocate(size: Size): Addr = {
    assert(isOpen)
    assert(size < PAGE_SIZE)
    val currentOffset = offset
    val resOffset =
      if (currentOffset + size < PAGE_SIZE) {
        offset = (currentOffset + size).toShort
        currentOffset
      } else {
        AddrStack.push(dirty, page)
        page   = pool.claim
        offset = size.toShort
        0
      }
    page + resOffset
  }
}
