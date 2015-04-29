package offheap

import offheap.internal.UnsafeHolder.UNSAFE

class UnsafeMemory extends Memory {
  protected class Alloc(val addr: Addr, val next: Alloc)
  protected var alloc: Alloc = null
  protected override def finalize(): Unit =
    while (alloc != null) {
      UNSAFE.freeMemory(alloc.addr)
      alloc = alloc.next
    }

  def allocate(size: Size): Addr = this.synchronized {
    val addr = UNSAFE.allocateMemory(size)
    alloc = new Alloc(addr, alloc)
    addr
  }
  def copy(from: Addr, to: Addr, size: Size): Unit = UNSAFE.copyMemory(from, to, size)
  def getChar(addr: Addr): Char                    = UNSAFE.getChar(addr)
  def getByte(addr: Addr): Byte                    = UNSAFE.getByte(addr)
  def getShort(addr: Addr): Short                  = UNSAFE.getShort(addr)
  def getInt(addr: Addr): Int                      = UNSAFE.getInt(addr)
  def getLong(addr: Addr): Long                    = UNSAFE.getLong(addr)
  def getFloat(addr: Addr): Float                  = UNSAFE.getFloat(addr)
  def getDouble(addr: Addr): Double                = UNSAFE.getDouble(addr)
  def putChar(addr: Addr, value: Char): Unit       = UNSAFE.putChar(addr, value)
  def putByte(addr: Addr, value: Byte): Unit       = UNSAFE.putByte(addr, value)
  def putShort(addr: Addr, value: Short): Unit     = UNSAFE.putShort(addr, value)
  def putInt(addr: Addr, value: Int): Unit         = UNSAFE.putInt(addr, value)
  def putLong(addr: Addr, value: Long): Unit       = UNSAFE.putLong(addr, value)
  def putFloat(addr: Addr, value: Float): Unit     = UNSAFE.putFloat(addr, value)
  def putDouble(addr: Addr, value: Double): Unit   = UNSAFE.putDouble(addr, value)
  def isNative: Boolean                            = true
}
object UnsafeMemory {
  def apply() = new UnsafeMemory()
}
