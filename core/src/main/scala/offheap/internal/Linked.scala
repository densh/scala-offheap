package offheap
package internal

import Memory.memory

class LinkedPagePool {
  private var chunk: LinkedChunk = null
  private var page: LinkedPage = null
  private def allocateChunk(): Unit = {
    println(s"pool: trying to allocate ${CHUNK_SIZE} bytes of memory")
    val start = memory.allocateMemory(CHUNK_SIZE)
    println(s"pool: allocated memory at $start")
    chunk = new LinkedChunk(start, chunk)
    var i = 0
    while (i < CHUNK_SIZE / PAGE_SIZE) {
      page = new LinkedPage(start + i * PAGE_SIZE, chunk, page)
      println(s"pool: created page $i starting at ${page.start}")
      i += 1
    }
  }
  def claim(): LinkedPage = this.synchronized {
    if (page == null) allocateChunk()
    val res = page
    page = res.next
    res.next = null
    println(s"pool: giving away page starting at ${res.start}")
    res
  }
  def reclaim(head: LinkedPage): Unit = this.synchronized {
    var tail = head
    while (tail.next != null)  tail = tail.next
    tail.next = page
    page = head
  }
}
object LinkedPagePool extends LinkedPagePool

final class LinkedChunk(val start: Long, var next: LinkedChunk)

final class LinkedPage(val start: Long, val chunk: LinkedChunk, var next: LinkedPage) {
  assert(start + PAGE_SIZE - 1 <= chunk.start + CHUNK_SIZE - 1)
}

final class LinkedRegion extends offheap.Region {
  private var page = LinkedPagePool.claim
  private var offset = 0L
  println(s"region: got page starting at ${page.start}")
  def isOpen: Boolean = page != null
  def close(): Unit = this.synchronized {
    LinkedPagePool.reclaim(page)
    page = null
  }
  def allocate(size: Size): Addr = this.synchronized {
    if (!isOpen) throw InaccessibleRegionException
    if (size > PAGE_SIZE) throw new IllegalArgumentException
    val currentOffset = this.offset
    if (currentOffset + size <= PAGE_SIZE) {
      this.offset = currentOffset + size
      val res = this.page.start + currentOffset
      assert(res + size - 1 < this.page.chunk.start + CHUNK_SIZE - 1)
      res

    } else {
      val newpage = LinkedPagePool.claim
      newpage.next = page
      this.offset = size
      this.page = newpage
      newpage.start
    }
  }
}
