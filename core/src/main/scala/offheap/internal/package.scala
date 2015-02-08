package offheap

package object internal {
  import Unsafe.unsafe

  type Addr = Long
  type Size = Long

  val PAGE_SIZE     = unsafe.pageSize()
  val ADDRESS_SIZE  = unsafe.addressSize()
  val CHUNK_SIZE    = PAGE_SIZE * 1024  // approx 4MB per chunk

  assert(PAGE_SIZE == 4096,
    "offheap memory is only support on systems with 4096 bytes long pages")
  assert(ADDRESS_SIZE == 8,
    "offheap memory is only supported on 64-bit systems")
  assert(CHUNK_SIZE % PAGE_SIZE == 0)
}
