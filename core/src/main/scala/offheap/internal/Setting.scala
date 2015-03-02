package offheap
package internal

import Unsafer.unsafe

object Setting {
  val pageSize: Long  =
    System.getProperty("offheap.pageSize", unsafe.pageSize().toString).toLong
  val chunkSize: Long =
    System.getProperty("offheap.chunkSize", math.max(pageSize, 4194304).toString).toLong

  assert(chunkSize % pageSize == 0,
    "chunkSize must be divisible by pageSize")
  assert(pageSize <= chunkSize,
    "pageSize must not be larger than chunkSize")
  assert(pageSize >= unsafe.pageSize(),
    s"pageSize must not be smaller than underlying system's page (${unsafe.pageSize()})")
  assert((pageSize & (pageSize - 1)) == 0,
    "pageSize must be a power of 2")
}
