package offheap
package internal

import offheap.internal.Setting._

class PagePool64(memory: Memory64) {
  private var chunk: Chunk64 = null
  private var page: Page64 = null
  newChunk()
  private def newChunk(): Unit = {
    val start = memory.allocate(chunkSize)
    if (start == 0) throw OutOfMemoryException
    chunk = new Chunk64(start, 0, chunk)
  }
  private def newPage(): Unit = {
    if (chunk.offset == chunk.start + chunkSize) newChunk()
    page = new Page64(chunk.start + chunk.offset, 0, page)
    chunk.offset += pageSize
  }
  def claim(): Page64 = this.synchronized {
    if (page == null) newPage()
    val res = page
    page = res.next
    res.next = null
    res
  }
  def reclaim(head: Page64): Unit = this.synchronized {
    var tail = head
    while (tail.next != null)  tail = tail.next
    tail.next = page
    page = head
  }
}
object PagePool64 {
  lazy val multiByteBufferPool: PagePool64 = new PagePool64(MultiByteBufferMemory64)
  lazy val unsafePool: PagePool64 = new PagePool64(UnsafeMemory64)
  def apply(memory: Memory64): PagePool64 = memory match {
    case MultiByteBufferMemory64 => multiByteBufferPool
    case UnsafeMemory64          => unsafePool
  }
}

final class Chunk64(val start: Long, var offset: Long, var next: Chunk64)

final class Page64(val start: Long, var offset: Long, var next: Page64)


