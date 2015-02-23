package offheap
package internal

import C._
import Unsafer.unsafe

@struct class SynchronizedAddrStack(arrSize: Long, arr: Ptr[Long], size: Long)
object SynchronizedAddrStack {
  final val GROWTH_FACTOR = 2

  def alloc(startingSize: Long): Ptr[SynchronizedAddrStack] = {
    val ptr = Ptr.alloc[SynchronizedAddrStack](1)
    ptr.arrSize = startingSize
    ptr.arr = Ptr.alloc[Long](startingSize)
    ptr.size = 0L
    ptr
  }

  def isEmpty(stack: Ptr[SynchronizedAddrStack]): Boolean = stack.size == 0

  def nonEmpty(stack: Ptr[SynchronizedAddrStack]): Boolean = stack.size != 0

  def push(stack: Ptr[SynchronizedAddrStack], value: Addr): Unit = {
    if (stack.size >= stack.arrSize) {
      stack.arrSize = (stack.arrSize * GROWTH_FACTOR).toLong
      stack.arr     = stack.arr.resize(stack.arrSize)
    }
    stack.arr(stack.size) = value
    stack.size = stack.size + 1
  }

  def pop(stack: Ptr[SynchronizedAddrStack]): Addr = {
    val newsize = stack.size - 1
    stack.size = newsize
    stack.arr(newsize)
  }

  def merge(stack: Ptr[SynchronizedAddrStack], other: Ptr[SynchronizedAddrStack]): Unit = {
    if (stack.size + other.size >= stack.arrSize) {
      stack.arrSize = ((stack.size + other.size) * GROWTH_FACTOR).toLong
      stack.arr     = stack.arr.resize(stack.arrSize)
    }
    Ptr.copy(other.arr, 0, stack.arr, stack.size, other.size)
    stack.size = stack.size + other.size
  }

  def free(stack: Ptr[SynchronizedAddrStack]): Unit = {
    stack.arr.free
    stack.free
  }
}

final class SynchronizedAddrStackPagePool {
  protected val chunks = SynchronizedAddrStack.alloc(startingSize = 4)
  protected val pages  = SynchronizedAddrStack.alloc(startingSize = 1024)

  override protected def finalize: Unit = {
    while (SynchronizedAddrStack.nonEmpty(chunks))
      unsafe.freeMemory(SynchronizedAddrStack.pop(chunks))
    SynchronizedAddrStack.free(chunks)
    SynchronizedAddrStack.free(pages)
  }

  protected def claimChunks: Unit = {
    val chunk = unsafe.allocateMemory(CHUNK_SIZE)
    SynchronizedAddrStack.push(chunks, chunk)
    var i = 0
    while (i < CHUNK_SIZE / PAGE_SIZE) {
      SynchronizedAddrStack.push(pages, chunk + i * PAGE_SIZE)
      i += 1
    }
  }

  def claim: Addr = this.synchronized {
    if (SynchronizedAddrStack.isEmpty(pages)) claimChunks
    SynchronizedAddrStack.pop(pages)
  }

  def reclaim(page: Addr): Unit = this.synchronized {
    SynchronizedAddrStack.push(pages, page)
  }

  def reclaimStack(otherPages: Ptr[SynchronizedAddrStack]) = this.synchronized {
    SynchronizedAddrStack.merge(pages, otherPages)
  }
}
object SynchronizedAddrStackPagePool {
  val currentPool = new ThreadLocal[SynchronizedAddrStackPagePool] {
    override protected def initialValue(): SynchronizedAddrStackPagePool = new SynchronizedAddrStackPagePool
  }
}

final class SynchronizedAddrStackRegion extends offheap.Region {
  private var pool   = SynchronizedAddrStackPagePool.currentPool.get
  private var dirty  = SynchronizedAddrStack.alloc(8)
  private var page   = pool.claim
  private var offset = 0

  def isOpen = pool != null

  def close(): Unit = this.synchronized {
    assert(isOpen)
    pool.reclaim(page)
    pool.reclaimStack(dirty)
    SynchronizedAddrStack.free(dirty)
    pool = null
  }

  def allocate(size: Size): Addr = this.synchronized {
    assert(isOpen)
    assert(size <= PAGE_SIZE)
    val currentOffset = offset
    val resOffset =
      if (currentOffset + size <= PAGE_SIZE) {
        offset = (currentOffset + size).toShort
        currentOffset
      } else {
        SynchronizedAddrStack.push(dirty, page)
        page   = pool.claim
        offset = size.toShort
        0
      }
    page + resOffset
  }
}
