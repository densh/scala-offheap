package offheap
package x64

final class Region(pool: Pool) extends Memory {
  private val tail = pool.claim
  private var page = tail
  val id = Region.atomicFresh.next
  val memory = pool.memory

  def isOpen = page != null

  private def checkOpen(): Unit =
    if (page == null) throw new InaccessibleRegionException

  def close(): Unit = this.synchronized {
    checkOpen
    pool.reclaim(page, tail)
    page = null
  }

  def allocate(size: Size): Addr = this.synchronized {
    checkOpen
    if (size > pool.pageSize)
      throw new IllegalArgumentException("can't allocate object larger than the virtual page")
    val currentOffset = page.offset
    val resOffset =
      if (currentOffset + size <= pool.pageSize) {
        page.offset = (currentOffset + size).toShort
        currentOffset
      } else {
        val newpage = pool.claim
        newpage.next = page
        newpage.offset = size.toShort
        page = newpage
        0L
      }
    page.start + resOffset
  }

  override def sizeOfRef: Size = 8 + 4

  override def getRef(addr: Addr): Ref = {
    checkOpen
    val refAddr = memory.getLong(addr)
    if (refAddr == 0L) null
    else {
      val refRegionId = memory.getInt(addr + 8L)
      if (refRegionId != this.id)
        throw new InaccessibleRegionException
      Ref(refAddr, this)
    }
  }

  override def putRef(addr: Addr, value: Ref): Unit = {
    checkOpen
    if (value == null)
      memory.putLong(addr, 0L)
    else {
      memory.putLong(addr, value.addr)
      memory.putInt(addr + 8L, this.id)
    }
  }

  def getChar(addr: Addr): Char                  = { checkOpen; memory.getChar(addr)          }
  def getByte(addr: Addr): Byte                  = { checkOpen; memory.getByte(addr)          }
  def getShort(addr: Addr): Short                = { checkOpen; memory.getShort(addr)         }
  def getInt(addr: Addr): Int                    = { checkOpen; memory.getInt(addr)           }
  def getLong(addr: Addr): Long                  = { checkOpen; memory.getLong(addr)          }
  def getFloat(addr: Addr): Float                = { checkOpen; memory.getFloat(addr)         }
  def getDouble(addr: Addr): Double              = { checkOpen; memory.getDouble(addr)        }

  def putChar(addr: Addr, value: Char): Unit     = { checkOpen; memory.putChar(addr, value)   }
  def putByte(addr: Addr, value: Byte): Unit     = { checkOpen; memory.putByte(addr, value)   }
  def putShort(addr: Addr, value: Short): Unit   = { checkOpen; memory.putShort(addr, value)  }
  def putInt(addr: Addr, value: Int): Unit       = { checkOpen; memory.putInt(addr, value)    }
  def putLong(addr: Addr, value: Long): Unit     = { checkOpen; memory.putLong(addr, value)   }
  def putFloat(addr: Addr, value: Float): Unit   = { checkOpen; memory.putFloat(addr, value)  }
  def putDouble(addr: Addr, value: Double): Unit = { checkOpen; memory.putDouble(addr, value) }
}
object Region {
  private val atomicFresh = new offheap.internal.AtomicFresh
  def open(implicit pool: Pool): Region = new Region(pool)
  def apply[T](f: Region => T)(implicit pool: Pool): T = {
    val region = Region.open(pool)
    try f(region)
    finally if (region.isOpen) region.close
  }
}
