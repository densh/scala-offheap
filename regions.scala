import scala.collection.immutable.IntMap

package regions {
  final class Region private[regions](val id: Short) extends AnyVal {
    @inline def allocInt(): IntRef = runtime.allocIntRef(this)
    @inline def allocIntArray(size: Int): IntArrayRef = runtime.allocIntArrayRef(this, size)
  }

  final class IntRef private(private[regions] val store: Long) extends AnyVal {
    @inline def apply(): Int = runtime.getIntRefValue(this)
    @inline def update(value: Int): Unit = runtime.setIntRefValue(this, value)
    private[regions] def region: Region = new Region((store & 0x000000FF ).toShort)
    private[regions] def offset: Long = store >> 8
  }

  object IntRef {
    @inline def apply(region: Region, offset: Long) = new IntRef(region.id.toLong + (offset << 8))
  }

  final class IntArrayRef private(private[regions] val store: Long) extends AnyVal {
    @inline def apply(i: Int): Int = runtime.getIntArrayRefValue(this, i)
    @inline def update(i: Int, value: Int): Unit = runtime.setIntArrayRefValue(this, i, value)
    private[regions] def region: Region = new Region((store & 0x000000FF ).toShort)
    private[regions] def offset: Long = store >> 8
  }

  object IntArrayRef {
    @inline def apply(region: Region, offset: Long) = new IntArrayRef(region.id.toLong + (offset << 8))
  }

  private[regions] final case class RegionInfo(start: Long, size: Long, cursor: Long)

  private[regions] object runtime {
    val unsafe = {
      import sun.misc.Unsafe
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

    def allocMemory(region: Region, size: Long): Long = {
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
      cursor
    }

    // IntRef

    @inline def allocIntRef(region: Region): IntRef =
      IntRef(region, allocMemory(region, 4))

    @inline def getIntRefValue(ref: IntRef): Int =
      unsafe.getInt(regions(ref.region.id).start + ref.offset)

    @inline def setIntRefValue(ref: IntRef, value: Int): Unit =
      unsafe.putInt(regions(ref.region.id).start + ref.offset, value)

    // IntArrayRef

    @inline def allocIntArrayRef(region: Region, size: Int): IntArrayRef =
      IntArrayRef(region, allocMemory(region, size * 4))

    @inline def getIntArrayRefValue(ref: IntArrayRef, i: Int): Int =
      unsafe.getInt(regions(ref.region.id).start + ref.offset + i * 4)

    @inline def setIntArrayRefValue(ref: IntArrayRef, i: Int, value: Int): Unit =
      unsafe.putInt(regions(ref.region.id).start + ref.offset + i * 4, value)
  }
}

package object regions {
  @inline def withRegion[T](f: Region => T): T = {
    val region = runtime.allocRegion()
    try f(region)
    finally runtime.disposeRegion(region)
  }
}

package test {
  import regions._

  object Test extends App {
    def recur(k: Int): Unit =
      withRegion { r =>
        val ref = r.allocIntArray(100000000)
        var i = 0
        while (i < 10000000) { ref(i) = 0; i += 1 }
        //var arr = Array.fill[Int](10000000)(0)
        if (k > 0) {
          recur(k - 1)
        }
      }
    recur(100)
    Thread.sleep(10000)
  }
}
