package regions.internal

import sun.misc.Unsafe
import scala.collection.mutable.LongMap
import scala.annotation.StaticAnnotation
import scala.language.experimental.{macros => CanMacro}
import regions.{Region, Ref, InaccessiblePageException}

package rt {
  final class offheap(layout: Layout) extends StaticAnnotation
  final case class Node(loc: Long, var next: Node)
  final case class Layout(fields: (String, Tag[_])*)
  final case class Tag[T]()
}

package object rt {
  // --- Internal runtime API as used in implementation of the Public API ---

  val unsafe: Unsafe = {
    val f = classOf[Unsafe].getDeclaredField("theUnsafe");
    f.setAccessible(true);
    f.get(null).asInstanceOf[Unsafe]
  }

  type Addr       = Long
  type PackedAddr = Long
  type Size       = Long
  type Offset     = Long

  def regionOpen(): regions.Region =
    new regions.Region(claimRegion())

  def regionClose(r: regions.Region): Unit =
    releaseRegion(r.region)

  def regionAllocate(r: regions.Region, size: Size): PackedAddr =
    claimMemoryInRegion(r.region, size)

  def unpack(paddr: PackedAddr): Addr =
    registeredPageAddr(packedPageId(paddr)) + packedOffset(paddr)

  // --- Implementation Details ---

  type PageId = Long

  val PAGE_SIZE    = unsafe.pageSize()
  val ADDRESS_SIZE = unsafe.addressSize()
  val SHORT_SIZE   = 2
  val CHUNK_SIZE   = PAGE_SIZE * 1024  // approx 4MB per chunk
  val MAX_PAGE_ID  = 4503599627370496L // 2^52

  assert(PAGE_SIZE == 4096,
    "region based memory is only support on systems with 4096 bytes long pages")
  assert(ADDRESS_SIZE == 8,
    "region based memory is only supported on 64-bit systems")
  assert(CHUNK_SIZE % PAGE_SIZE == 0)

  /*def validPackedAddr(paddr: PackedAddr): Boolean = {
    val pageId = packedPageId(paddr)
    val offset = packedOffset(paddr)
    pageId < MAX_PAGE_ID && pageId >= 0 &&
    (pageMap.contains(pageId) || paddr == 0) &&
    offset < PAGE_SIZE && offset >= 0
  }*/

  def pack(pageId: PageId, offset: Offset): PackedAddr = (pageId << 12) + offset
  def packedPageId(paddr: PackedAddr): PageId = (paddr & 0xffffffffff000L) >> 12
  def packedOffset(paddr: PackedAddr): PageId = (paddr & 0x0000000000fffL).toShort

  final class Region(var pageId:       PageId,
                     var pageAddr:     Addr,
                     var pageOffset:   Short,
                     var dirtyPageIds: LongStack)

  final class LongStack(startingSize: Int) {
    private var arr: Array[Long] = new Array[Long](startingSize)
    private var idx: Int = 0

    def isEmpty: Boolean  = idx == 0
    def nonEmpty: Boolean = idx != 0

    def push(value: Long): Unit =
      if (idx < arr.length) {
        arr(idx) = value
        idx += 1
      } else {
        val newarr = new Array[Long](arr.size * 2)
        System.arraycopy(arr, 0, newarr, 0, arr.size)
        arr = newarr
        push(value)
      }

    def pop(): Long = {
      assert(idx >= 1)
      idx -= 1
      arr(idx)
    }
  }

  val freePageStack: LongStack =
    new LongStack(CHUNK_SIZE / PAGE_SIZE)
  def claimPage(): Addr = {
    if (freePageStack.isEmpty) {
      val chunk = unsafe.allocateMemory(CHUNK_SIZE)
      var i = 0
      while (i < CHUNK_SIZE / PAGE_SIZE) {
        freePageStack.push(chunk + i * PAGE_SIZE)
        i += 1
      }
    }
    freePageStack.pop()
  }
  def releasePage(page: Addr) =
    freePageStack.push(page)

  val pageMap: LongMap[Addr] = LongMap.empty[Addr]
  var nextPageId: Long = 0
  def registerPage(page: Addr): PageId = {
    nextPageId += 1
    assert(nextPageId < MAX_PAGE_ID)
    val id = nextPageId
    pageMap(id) = page
    id
  }
  def registeredPageAddr(pageId: PageId): Addr =
    if (!pageMap.contains(pageId)) {
      if (pageId == 0)
        0
      else {
        throw InaccessiblePageException
      }
    } else {
      pageMap(pageId)
    }
  def deregisterPage(pageId: PageId): Unit = {
    val pageAddr = pageMap(pageId)
    pageMap -= pageId
    releasePage(pageAddr)
  }

  def claimRegion(): Region = {
    val pageAddr = claimPage()
    val pageId   = registerPage(pageAddr)
    new Region(pageId, pageAddr, 0, new LongStack(64))
  }
  def releaseRegion(r: Region): Unit = {
    deregisterPage(r.pageId)
    while (r.dirtyPageIds.nonEmpty)
      deregisterPage(r.dirtyPageIds.pop())
  }
  def claimMemoryInRegion(r: Region, size: Size): PackedAddr = {
    assert(size < PAGE_SIZE)
    val offset = r.pageOffset
    val resOffset =
      if (offset + size < PAGE_SIZE) {
        r.pageOffset = (offset + size).toShort
        offset
      } else {
        val pageAddr = claimPage()
        val pageId   = registerPage(pageAddr)
        r.dirtyPageIds.push(r.pageId)
        r.pageId = pageId
        r.pageOffset = size.toShort
        0
      }
    pack(r.pageId, resOffset)
  }
}
