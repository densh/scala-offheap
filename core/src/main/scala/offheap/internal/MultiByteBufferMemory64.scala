package offheap
package internal

import java.nio.ByteBuffer

object MultiByteBufferMemory64 extends Memory64 {
  private final val MASK = 0xFFFFFFFFL
  private val lock = new Object
  private var idx = 1
  private val buffers = new Array[ByteBuffer](1024)

  def allocate(size: Size): Addr = lock.synchronized {
    assert(size > 0)
    assert(size <= Integer.MAX_VALUE)
    val buffer = ByteBuffer.allocateDirect(size.toInt)
    buffers(idx) = buffer
    val res = idx.toLong << 32
    idx += 1
    res
  }

  def unpack(addr: Addr): (Int, Int)             = ((addr >> 32).toInt, (addr & MASK).toInt)

  def getByte(addr: Addr): Byte                  = buffers((addr >> 32).toInt).get((addr & MASK).toInt)
  def getChar(addr: Addr): Char                  = buffers((addr >> 32).toInt).getChar((addr & MASK).toInt)
  def getShort(addr: Addr): Short                = buffers((addr >> 32).toInt).getShort((addr & MASK).toInt)
  def getInt(addr: Addr): Int                    = buffers((addr >> 32).toInt).getInt((addr & MASK).toInt)
  def getLong(addr: Addr): Long                  = buffers((addr >> 32).toInt).getLong((addr & MASK).toInt)
  def getFloat(addr: Addr): Float                = buffers((addr >> 32).toInt).getFloat((addr & MASK).toInt)
  def getDouble(addr: Addr): Double              = buffers((addr >> 32).toInt).getDouble((addr & MASK).toInt)
  def putByte(addr: Addr, value: Byte): Unit     = buffers((addr >> 32).toInt).put((addr & MASK).toInt, value)
  def putChar(addr: Addr, value: Char): Unit     = buffers((addr >> 32).toInt).putChar((addr & MASK).toInt, value)
  def putShort(addr: Addr, value: Short): Unit   = buffers((addr >> 32).toInt).putShort((addr & MASK).toInt, value)
  def putInt(addr: Addr, value: Int): Unit       = buffers((addr >> 32).toInt).putInt((addr & MASK).toInt, value)
  def putLong(addr: Addr, value: Long): Unit     = buffers((addr >> 32).toInt).putLong((addr & MASK).toInt, value)
  def putFloat(addr: Addr, value: Float): Unit   = buffers((addr >> 32).toInt).putFloat((addr & MASK).toInt, value)
  def putDouble(addr: Addr, value: Double): Unit = buffers((addr >> 32).toInt).putDouble((addr & MASK).toInt, value)
}
