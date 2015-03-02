package offheap
package internal

import offheap.internal.Unsafer.unsafe

object Setting {
  val pageSize: Int  = 4096
    // System.getProperty("offheap.pageSize", unsafe.pageSize().toString).toInt
  val chunkSize: Int = Integer.MAX_VALUE - 1
    // System.getProperty("offheap.chunkSize", math.max(pageSize, 4194304).toString).toInt

  // val maxMemory: Int = ???
  // val minMemory: Int = ???

  assert(pageSize <= chunkSize,
    "pageSize must not be larger than chunkSize")
  assert(pageSize >= unsafe.pageSize(),
    s"pageSize must not be smaller than underlying system's page (${unsafe.pageSize()})")
  assert((pageSize & (pageSize - 1)) == 0,
    "pageSize must be a power of 2")
}
