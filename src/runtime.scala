package regions
import scala.collection.immutable.IntMap
import sun.misc.Unsafe

package object internal {
  val unsafe: Unsafe = {
    val f = classOf[Unsafe].getDeclaredField("theUnsafe");
    f.setAccessible(true);
    f.get(null).asInstanceOf[Unsafe]
  }
  val CHUNK_SIZE = 4096000

  final case class RegionInfo(start: Long, size: Long, cursor: Long)
  val infos: Array[RegionInfo] = new Array[RegionInfo](65536)
  var last = 0

  def allocRegion(): Region = {
    val start = unsafe.allocateMemory(CHUNK_SIZE)
    val info = RegionInfo(start = start, size = CHUNK_SIZE, cursor = 0)
    val id = last
    last += 1
    infos(id) = info
    new Region(id.toShort)
  }

  def disposeRegion(region: Region): Unit = {
    assert(region.id == last - 1)
    last -= 1
    unsafe.freeMemory(infos(last).start)
    infos(last) = null
  }

  def allocMemory[T](region: Region, size: Long): Ref[T] = {
    val info = infos(region.id)
    val cursor = info.cursor
    if (cursor + size < info.size)
      infos(region.id) = info.copy(cursor = cursor + size)
    else {
      var newsize = info.size
      while (cursor + size > newsize) newsize += CHUNK_SIZE
      val newstart = unsafe.reallocateMemory(info.start, newsize)
      infos(region.id) = info.copy(start = newstart, size = newsize, cursor = cursor + size)
    }
    new Ref[T](region.id.toLong + (cursor << 8))
  }
}
