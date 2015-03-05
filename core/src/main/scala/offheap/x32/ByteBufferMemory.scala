package offheap
package x32

import java.nio.ByteBuffer

class ByteBufferMemory(buffer: ByteBuffer) extends Memory {
  def allocate(size: Int): Addr                  = ???
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
