package offheap
package x64

final class Pool(
  val memory: Memory,
  val pageSize: Size = 4096,
  val chunkSize: Size = 1024 * 4096
) {
  private var chunk: Chunk = null
  private var page: Page = null
  newChunk()
  private def newChunk(): Unit = {
    val start = memory.allocate(chunkSize)
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
  def apply(memory: Memory): Pool = new Pool(memory)
}

final class Chunk(val start: Addr, var offset: Size, var next: Chunk)

final class Page(val start: Addr, var offset: Size, var next: Page)


