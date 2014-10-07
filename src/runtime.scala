package regions
import sun.misc.Unsafe
import scala.collection.immutable.IntMap
import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

package internal {
  class struct extends StaticAnnotation
}

package object internal {
  val unsafe: Unsafe = {
    val f = classOf[Unsafe].getDeclaredField("theUnsafe");
    f.setAccessible(true);
    f.get(null).asInstanceOf[Unsafe]
  }
  // TODO: dynamically upscale size of the array
  // TODO: use thread-local
  val infos: Array[Long] = new Array[Long](64 * 3)
  var last = 1

  def allocRegion(size: Long = 40960): Region = {
    val start = unsafe.allocateMemory(size)
    val id = last
    last += 1
    infos(id * 3) = start
    infos(id * 3 + 1) = size
    infos(id * 3 + 2) = 0
    new Region(id.toShort)
  }

  def disposeRegion(region: Region): Unit = {
    last -= 1
    unsafe.freeMemory(infos(last * 3))
  }

  def allocMemory[T](region: Region, size: Long): Ref[T] = {
    val rsize = infos(region.id * 3 + 1)
    val cursor = infos(region.id * 3 + 2)
    if (cursor + size < rsize)
      infos(region.id * 3 + 2) = cursor + size
    else {
      val newsize = rsize * 2
      val newstart = unsafe.reallocateMemory(infos(region.id * 3), newsize)
      infos(region.id * 3) = newstart
      infos(region.id * 3 + 1) = newsize
      infos(region.id * 3 + 2) = cursor + size
      //println(s"resized region size = $newsize, start = $newstart")
    }
    //println(s"allocated at ${region.id} and $cursor")
    new Ref[T](region.id.toLong + (cursor << 8))
  }

  def ensureFixedSizeAlloc[T]: Unit = macro internal.macros.ensureFixedSizeAlloc[T]
}
