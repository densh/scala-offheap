package offheap
package internal

import Unsafe.unsafe

object LinkedPagePool {
  var page: LinkedPage = null
  def claimChunk(): Unit = ???
  def claim(): LinkedPage = ???
  def reclaim(p: LinkedPage): Unit = ???
}

final case class LinkedPage(val start: Long, var offset: Long, var next: LinkedPage)

final class LinkedRegion extends offheap.Region {
  var page = LinkedPagePool.claim
  protected[internal] def isOpen: Boolean = page != null
  protected[internal] def close(): Unit = {
    LinkedPagePool.reclaim(page)
    page = null
  }
  protected[internal] def allocate(size: Size): Addr = ???
}
