package offheap

import offheap.internal.Sanitizer

final class Pool(
  val alloc: Allocator,
  val pageSize: Size,
  val chunkSize: Size
) {
  private[this] var chunk: Chunk = null
  private[this] var page: Page = null
  newChunk()
  private def newChunk(): Unit = {
    val start = Sanitizer.validate(alloc.allocate(chunkSize))
    chunk = new Chunk(start, 0, chunk)
  }
  private def newPage(): Unit = {
    if (chunk.offset + pageSize >= chunkSize) newChunk()
    page = new Page(chunk.start + chunk.offset, 0, page)
    chunk.offset += pageSize
  }
  def claim(): Page = this.synchronized {
    if (page == null) newPage()
    val res = page
    page = res.next
    res.next = null
    res
  }
  def reclaim(head: Page): Unit = {
    var tail = head
    while (tail.next != null)  tail = tail.next
    reclaim(head, tail)
  }
  def reclaim(head: Page, tail: Page): Unit = this.synchronized {
    tail.next = page
    page = head
  }
}
object Pool {
  def apply(alloc: Allocator, pageSize: Size = 4096, chunkSize: Size = 1024 * 4096) =
    new Pool(alloc, pageSize, chunkSize)
}

final class Chunk(val start: Addr, var offset: Size, var next: Chunk)

final class Page(val start: Addr, var offset: Size, var next: Page)
