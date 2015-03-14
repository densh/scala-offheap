package offheap
package x64

import offheap.internal.Unsafer.unsafe

class UnsafeMemory extends NativeMemory {
  assert(unsafe.addressSize() == 8,
    "unsafe memory is only supported 64-bit systems")

  protected class Alloc(val addr: Addr, val next: Alloc)
  protected var alloc: Alloc = null
  protected override def finalize(): Unit =
    while (alloc != null) {
      unsafe.freeMemory(alloc.addr)
      alloc = alloc.next
    }

  def allocate(size: Size): Addr = this.synchronized {
    val addr = unsafe.allocateMemory(size)
    alloc = new Alloc(addr, alloc)
    addr
  }
  def copy(from: Addr, to: Addr, size: Size): Unit = unsafe.copyMemory(from, to, size)
  def getChar(addr: Addr): Char                    = unsafe.getChar(addr)
  def getByte(addr: Addr): Byte                    = unsafe.getByte(addr)
  def getShort(addr: Addr): Short                  = unsafe.getShort(addr)
  def getInt(addr: Addr): Int                      = unsafe.getInt(addr)
  def getLong(addr: Addr): Long                    = unsafe.getLong(addr)
  def getFloat(addr: Addr): Float                  = unsafe.getFloat(addr)
  def getDouble(addr: Addr): Double                = unsafe.getDouble(addr)
  def putChar(addr: Addr, value: Char): Unit       = unsafe.putChar(addr, value)
  def putByte(addr: Addr, value: Byte): Unit       = unsafe.putByte(addr, value)
  def putShort(addr: Addr, value: Short): Unit     = unsafe.putShort(addr, value)
  def putInt(addr: Addr, value: Int): Unit         = unsafe.putInt(addr, value)
  def putLong(addr: Addr, value: Long): Unit       = unsafe.putLong(addr, value)
  def putFloat(addr: Addr, value: Float): Unit     = unsafe.putFloat(addr, value)
  def putDouble(addr: Addr, value: Double): Unit   = unsafe.putDouble(addr, value)
}
object UnsafeMemory {
  def apply() = new UnsafeMemory()
}
