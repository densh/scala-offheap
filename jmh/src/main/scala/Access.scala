package jmh

import org.openjdk.jmh.annotations._
import offheap._

class JByteCell(var v: Byte)
class JShortCell(var v: Short)
class JIntCell(var v: Int)
class JLongCell(var v: Long)

@data class ByteCell(var v: Byte)
@data class ShortCell(var v: Short)
@data class IntCell(var v: Int)
@data class LongCell(var v: Long)

@State(Scope.Thread)
class Access {
  implicit val alloc = malloc

  val byte  = ByteCell(0.toByte)
  val short = ShortCell(0.toShort)
  val int   = IntCell(0)
  val long  = LongCell(0L)

  val byte42 = 42.toByte
  val short42 = 42.toShort
  val int42 = 42
  val long42 = 42L

  @Benchmark
  def offheapReadByte = byte.v

  @Benchmark
  def offheapReadShort = short.v

  @Benchmark
  def offheapReadInt = int.v

  @Benchmark
  def offheapReadLong = long.v

  @Benchmark
  def offheapWriteByte = { byte.v = byte42; byte42 }

  @Benchmark
  def offheapWriteShort = { short.v = short42; short42 }

  @Benchmark
  def offheapWriteInt = { int.v = int42; int42 }

  @Benchmark
  def offheapWriteLong = { long.v = long42; long42 }

  val jbyte  = new JByteCell(0.toByte)
  val jshort = new JShortCell(0.toShort)
  val jint   = new JIntCell(0)
  val jlong  = new JLongCell(0L)

  @Benchmark
  def onheapReadByte = jbyte.v

  @Benchmark
  def onheapReadShort = jshort.v

  @Benchmark
  def onheapReadInt = jint.v

  @Benchmark
  def onheapReadLong = jlong.v

  @Benchmark
  def onheapWriteByte = { jbyte.v = byte42; byte42 }

  @Benchmark
  def onheapWriteShort = { jshort.v = short42; short42 }

  @Benchmark
  def onheapWriteInt = { jint.v = int42; int42 }

  @Benchmark
  def onheapWriteLong = { jlong.v = long42; long42 }
}
