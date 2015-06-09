package scala.offheap

import offheap.internal.Sanitizer
import offheap.internal.SunMisc.UNSAFE

/** Efficient pool of fixed-size memory pages.
 *  Allocations from underlying allocator are performed
 *  in big chunks of memory that are sliced into
 *  pages of requested size.
 *
 *  Pages and chunks are organized in a intrusive linked list
 *  wayt to minimise memory overhead and re-use the same nodes
 *  for the whole lifetime of the pool.
 *
 *  Memory is reclaimed back to underlying alocator once
 *  the pool is finalized.
 */
final class Pool(
  val alloc: Allocator,
  val pageSize: Size,
  val chunkSize: Size
) {
  if (chunkSize % pageSize != 0)
    throw new IllegalArgumentException("chunkSize must be a multiple of pageSize")
  if (pageSize > chunkSize)
    throw new IllegalArgumentException("pageSize may not be bigger than chunkSize")
  if (chunkSize < UNSAFE.pageSize())
    throw new IllegalArgumentException(
      s"chunkSize may not be smaller than underlying OS virtual page size (${UNSAFE.pageSize()})")

  private[this] var chunk: Pool.Chunk = null
  private[this] var page: Pool.Page = null
  newChunk()
  private def newChunk(): Unit = {
    val start = Sanitizer.validate(alloc.allocate(chunkSize))
    chunk = new Pool.Chunk(start, 0, chunk)
  }
  private def newPage(): Unit = {
    if (chunk.offset + pageSize >= chunkSize) newChunk()
    page = new Pool.Page(chunk.start + chunk.offset, 0, page)
    chunk.offset += pageSize
  }
  protected override def finalize: Unit = {
    while (chunk != null) {
      try {
        alloc.free(chunk.start)
        chunk = chunk.next
      } catch {
        case _: UnsupportedOperationException =>
          chunk = null
      }
    }
  }
  def claim(): Pool.Page = this.synchronized {
    if (page == null) newPage()
    val res = page
    page = res.next
    res.next = null
    res
  }
  def reclaim(head: Pool.Page): Unit = {
    var tail = head
    while (tail.next != null)  tail = tail.next
    reclaim(head, tail)
  }
  def reclaim(head: Pool.Page, tail: Pool.Page): Unit = this.synchronized {
    tail.next = page
    page = head
  }
}
object Pool {
  private final class Chunk(val start: Addr, var offset: Size, var next: Chunk)
  final class Page(val start: Addr, var offset: Size, var next: Page)
  def apply(alloc: Allocator = malloc,
            pageSize: Size = UNSAFE.pageSize(),
            chunkSize: Size = 256 * UNSAFE.pageSize()) =
    new Pool(alloc, pageSize, chunkSize)
}


