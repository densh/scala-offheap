package offheap
package internal

final class Region64(pool: Pool64) extends Region with Memory64 {
  private var page = pool.claim
  val id: Long = Region64.freshId.next
  val memory = pool.memory

  def isOpen = page != null

  def close(): Unit = this.synchronized {
    assert(isOpen, "can't close region which is already closed")
    pool.reclaim(page)
    page = null
  }

  def allocate(size: Int): Addr = this.synchronized {
    assert(isOpen, "can't allocate in closed region")
    assert(size <= pool.pageSize, "can't allocate object larger than the virtual page")
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

  override def sizeOfRef: Int = 16

  override def getRef(addr: Addr): Ref64 = {
    assert(isOpen)
    val refAddr = memory.getLong(addr)
    if (refAddr == 0L) null
    else {
      val refRegionId = memory.getLong(addr + 8L)
      assert(refRegionId == this.id)
      Ref64(refAddr, this)
    }
  }

  override def putRef(addr: Addr, value: Ref64): Unit = {
    assert(isOpen)
    if (value != null) {
      memory.putLong(addr, value.addr)
      memory.putLong(addr + 8L, this.id)
    } else {
      memory.putLong(addr, 0L)
    }
  }

  def getChar(addr: Addr): Char                  = { assert(isOpen); memory.getChar(addr)          }
  def getByte(addr: Addr): Byte                  = { assert(isOpen); memory.getByte(addr)          }
  def getShort(addr: Addr): Short                = { assert(isOpen); memory.getShort(addr)         }
  def getInt(addr: Addr): Int                    = { assert(isOpen); memory.getInt(addr)           }
  def getLong(addr: Addr): Long                  = { assert(isOpen); memory.getLong(addr)          }
  def getFloat(addr: Addr): Float                = { assert(isOpen); memory.getFloat(addr)         }
  def getDouble(addr: Addr): Double              = { assert(isOpen); memory.getDouble(addr)        }

  def putChar(addr: Addr, value: Char): Unit     = { assert(isOpen); memory.putChar(addr, value)   }
  def putByte(addr: Addr, value: Byte): Unit     = { assert(isOpen); memory.putByte(addr, value)   }
  def putShort(addr: Addr, value: Short): Unit   = { assert(isOpen); memory.putShort(addr, value)  }
  def putInt(addr: Addr, value: Int): Unit       = { assert(isOpen); memory.putInt(addr, value)    }
  def putLong(addr: Addr, value: Long): Unit     = { assert(isOpen); memory.putLong(addr, value)   }
  def putFloat(addr: Addr, value: Float): Unit   = { assert(isOpen); memory.putFloat(addr, value)  }
  def putDouble(addr: Addr, value: Double): Unit = { assert(isOpen); memory.putDouble(addr, value) }
}
object Region64 {
  private val freshId = new AtomicFreshLong
  def open(implicit pool: Pool64): Region64 = new Region64(pool)
  def apply[T](f: Region64 => T)(implicit pool: Pool64): T = {
    val region = Region64.open(pool)
    try f(region)
    finally if (region.isOpen) region.close
  }
}
