package offheap
package x64

sealed class Region(private[this] val pool: Pool) extends Memory {
  private[this] val tail = pool.claim
  private[this] var page = tail
  val id = Region.atomicFresh.next
  val memory = pool.memory

  def isOpen   = page != null
  def isClosed = page == null

  protected override def finalize(): Unit =
    if (isOpen) close

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


  override def getRef(addr: Addr): Ref = {
    checkOpen
    val refAddr = memory.getLong(addr)
    if (refAddr == 0L) null
    else {
      val refRegionId = memory.getInt(addr + 8L)
      if (refRegionId != this.id)
        throw new InaccessibleRegionException(
          "can't read a reference from different region")
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

  def copy(from: Addr, to: Addr, size: Size)     = { checkOpen; memory.copy(from, to, size)   }
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
  def isNative: Boolean                          = memory.isNative
}
object Region {
  private val atomicFresh = new offheap.internal.AtomicFresh
  def open(implicit pool: Pool) = new Region(pool)
  def apply[T](f: Region => T)(implicit pool: Pool): T = {
    val region = open(pool)
    try f(region)
    finally if (region.isOpen) region.close
  }
}
