package offheap
package internal

import java.nio.ByteBuffer
import offheap.internal.Unsafer.unsafe
import offheap.internal.Memory64._

trait Memory64 {
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

  def getRef(addr: Addr): Ref64 =
    new Ref(getLong(addr), this)
  def putRef(addr: Addr, ref: Ref64): Unit = {
    assert(ref.memory eq this)
    putLong(addr, ref)
  }
}
object Memory64 {
  type Addr = Long
  type Size = Long
}

object MultiByteBufferMemory64 extends Memory64 {
  private final val MASK = 0xFFFFFFFFL
  private var idx = 1
  // TODO: automatically resize this array
  private val buffers = new Array[ByteBuffer](1024)

  def allocate(size: Size): Addr = this.synchronized {
    assert(size > 0)
    assert(size <= Integer.MAX_VALUE)
    buffers(idx) = ByteBuffer.allocateDirect(size.toInt)
    val res = idx.toLong << 32
    idx += 1
    res
  }

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

object UnsafeMemory64 extends Memory64 {
  assert(unsafe.addressSize() == 8,
    "unsafe memory is only supported 64-bit systems")

  def allocate(size: Size): Addr                 = unsafe.allocateMemory(size)
  def getChar(addr: Addr): Char                  = unsafe.getChar(addr)
  def getByte(addr: Addr): Byte                  = unsafe.getByte(addr)
  def getShort(addr: Addr): Short                = unsafe.getShort(addr)
  def getInt(addr: Addr): Int                    = unsafe.getInt(addr)
  def getLong(addr: Addr): Long                  = unsafe.getLong(addr)
  def getFloat(addr: Addr): Float                = unsafe.getFloat(addr)
  def getDouble(addr: Addr): Double              = unsafe.getDouble(addr)
  def putChar(addr: Addr, value: Char): Unit     = unsafe.putChar(addr, value)
  def putByte(addr: Addr, value: Byte): Unit     = unsafe.putByte(addr, value)
  def putShort(addr: Addr, value: Short): Unit   = unsafe.putShort(addr, value)
  def putInt(addr: Addr, value: Int): Unit       = unsafe.putInt(addr, value)
  def putLong(addr: Addr, value: Long): Unit     = unsafe.putLong(addr, value)
  def putFloat(addr: Addr, value: Float): Unit   = unsafe.putFloat(addr, value)
  def putDouble(addr: Addr, value: Double): Unit = unsafe.putDouble(addr, value)
}
