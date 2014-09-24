package regions
import scala.collection.immutable.IntMap
import sun.misc.Unsafe

package object internal {
  private[regions] final case class RegionInfo(start: Long, size: Long, cursor: Long)
  val unsafe: Unsafe = {
    val f = classOf[Unsafe].getDeclaredField("theUnsafe");
    f.setAccessible(true);
    f.get(null).asInstanceOf[Unsafe]
  }
  val CHUNK_SIZE = 4096000
  val regions: Array[RegionInfo] = Array.fill[RegionInfo](65536)(null)
  var last = 0

  def allocRegion(): Region = {
    val start = unsafe.allocateMemory(CHUNK_SIZE)
    val info = RegionInfo(start = start, size = CHUNK_SIZE, cursor = 0)
    val id = last
    last += 1
    regions(id) = info
    new Region(id.toShort)
  }

  def disposeRegion(region: Region): Unit = {
    assert(region.id == last - 1)
    last -= 1
    unsafe.freeMemory(regions(last).start)
    regions(last) = null
  }

  def allocMemory[T](region: Region, size: Long): Ref[T] = {
    val info = regions(region.id)
    val cursor = info.cursor
    if (cursor + size < info.size)
      regions(region.id) = info.copy(cursor = cursor + size)
    else {
      var newsize = info.size
      while (cursor + size > newsize) newsize += CHUNK_SIZE
      val newstart = unsafe.reallocateMemory(info.start, newsize)
      regions(region.id) = info.copy(start = newstart, size = newsize, cursor = cursor + size)
    }
    new Ref[T](region.id.toLong + (cursor << 8))
  }

  def refRegion[T](ref: Ref[T]): Region = new Region((ref.loc & 0x000000FF ).toShort)
  def refOffset[T](ref: Ref[T]): Long = ref.loc >> 8

  // IntRef

  /*@inline def allocIntRef(region: Region): IntRef =
    IntRef(region, allocMemory(region, 4))

  @inline def getIntRefValue(ref: IntRef): Int =
    unsafe.getInt(regions(ref.region.id).start + ref.offset)

  @inline def setIntRefValue(ref: IntRef, value: Int): Unit =
    unsafe.putInt(regions(ref.region.id).start + ref.offset, value)*/

  // IntArrayRef

  /*@inline def allocIntArrayRef(region: Region, size: Int): IntArrayRef =
    IntArrayRef(region, allocMemory(region, size * 4))

  @inline def getIntArrayRefValue(ref: IntArrayRef, i: Int): Int =
    unsafe.getInt(regions(ref.region.id).start + ref.offset + i * 4)

  @inline def setIntArrayRefValue(ref: IntArrayRef, i: Int, value: Int): Unit =
    unsafe.putInt(regions(ref.region.id).start + ref.offset + i * 4, value)*/
}
