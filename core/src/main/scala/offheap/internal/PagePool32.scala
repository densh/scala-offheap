package offheap
package internal

import offheap.internal.Setting._

class PagePool32(val memory: Memory32) {
  private var chunk: Chunk32 = null
  private var page: Page32 = null
  newChunk()
  private def newChunk(): Unit = {
    val start = memory.allocate(chunkSize)
    if (start == 0) throw OutOfMemoryException
    chunk = new Chunk32(start, 0, chunk)
  }
  private def newPage(): Unit = {
    if (chunk.offset + pageSize >= chunkSize) newChunk()
    page = new Page32(chunk.start + chunk.offset, 0, page)
    chunk.offset += pageSize
  }
  def claim(): Page32 = this.synchronized {
    if (page == null) newPage()
    val res = page
    page = res.next
    res.next = null
    res
  }
  def reclaim(head: Page32): Unit = this.synchronized {
    var tail = head
    while (tail.next != null)  tail = tail.next
    tail.next = page
    page = head
  }
}
object PagePool32 {
  lazy val byteBufferPool = new PagePool32(ByteBufferMemory32)
}

final class Chunk32(val start: Int, var offset: Int, var next: Chunk32)

final class Page32(val start: Int, var offset: Int, var next: Page32)


