package offheap
package internal

import java.nio.ByteBuffer
import offheap.internal.Memory32._

trait Memory32 {
  def allocate(size: Size): Addr
  def getChar(addr: Addr): Char
  def getByte(addr: Addr): Byte
  def getShort(addr: Addr): Short
  def getInt(addr: Addr): Int
  def getLong(addr: Addr): Long
  def getFloat(addr: Addr): Float
  def getDouble(addr: Addr): Double
  def putChar(addr: Addr, value: Char): Unit
  def putByte(addr: Addr, value: Byte): Unit
  def putShort(addr: Addr, value: Short): Unit
  def putInt(addr: Addr, value: Int): Unit
  def putLong(addr: Addr, value: Long): Unit
  def putFloat(addr: Addr, value: Float): Unit
  def putDouble(addr: Addr, value: Double): Unit

  lazy val pool = new PagePool32(this)
}
object Memory32 {
  type Addr = Int
  type Size = Int
}

object ByteBufferMemory32 extends Memory32 {
  private var buffer: ByteBuffer = _

  def allocate(size: Size): Addr = this.synchronized {
    if (buffer != null) throw OutOfMemoryException
    else {
      assert(size > 0 && size < Integer.MAX_VALUE)
      buffer = ByteBuffer.allocateDirect(size + 1)
      1
    }
  }

  def getByte(addr: Addr): Byte                  = buffer.get(addr)
  def getChar(addr: Addr): Char                  = buffer.getChar(addr)
  def getShort(addr: Addr): Short                = buffer.getShort(addr)
  def getInt(addr: Addr): Int                    = buffer.getInt(addr)
  def getLong(addr: Addr): Long                  = buffer.getLong(addr)
  def getFloat(addr: Addr): Float                = buffer.getFloat(addr)
  def getDouble(addr: Addr): Double              = buffer.getDouble(addr)
  def putByte(addr: Addr, value: Byte): Unit     = buffer.put(addr, value)
  def putChar(addr: Addr, value: Char): Unit     = buffer.putChar(addr, value)
  def putShort(addr: Addr, value: Short): Unit   = buffer.putShort(addr, value)
  def putInt(addr: Addr, value: Int): Unit       = buffer.putInt(addr, value)
  def putLong(addr: Addr, value: Long): Unit     = buffer.putLong(addr, value)
  def putFloat(addr: Addr, value: Float): Unit   = buffer.putFloat(addr, value)
  def putDouble(addr: Addr, value: Double): Unit = buffer.putDouble(addr, value)
}
