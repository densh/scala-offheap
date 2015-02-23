package offheap

package object internal {
  import Unsafer.unsafe

  type Addr = Long
  type Size = Long

  val PAGE_POOLS    = Runtime.getRuntime().availableProcessors()
  val PAGE_SIZE     = unsafe.pageSize()
  val ADDRESS_SIZE  = unsafe.addressSize()
  val CHUNK_SIZE    = PAGE_SIZE * 256

  assert(ADDRESS_SIZE == 8, "offheap memory is only supported on 64-bit systems")
  assert(CHUNK_SIZE % PAGE_SIZE == 0)
  assert(PAGE_POOLS > 0)
}
