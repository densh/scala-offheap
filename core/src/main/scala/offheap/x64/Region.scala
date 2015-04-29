package offheap
package x64

import scala.language.experimental.{macros => canMacro}
import offheap.internal.macros

final class Region(private[this] val pool: Pool) extends Memory {
  private[this] val tail = pool.claim
  private[this] var page = tail
  val id = Region.atomicFresh.next
  val memory = pool.memory

  def isOpen   = page != null
  def isClosed = page == null

  private def checkOpen(): Unit =
    if (page == null) throw new InaccessibleRegionException

  private def pad(addr: Addr) = {
    val alignment = sizeOf[Long]
    val padding =
      if (addr % alignment == 0) 0
      else alignment - addr % alignment
    addr + padding
  }

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
    val paddedOffset = pad(currentOffset)
    val resOffset =
      if (paddedOffset + size <= pool.pageSize) {
        page.offset = paddedOffset + size
        paddedOffset
      } else {
        val newpage = pool.claim
        newpage.next = page
        newpage.offset = size
        page = newpage
        0L
      }
    page.start + resOffset
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
  def apply[T](f: Region => T)(implicit pool: Pool): T = macro macros.Region.apply
}
