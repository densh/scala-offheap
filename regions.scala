// lib

package regions

final class Region(val id: Short) extends AnyVal {
  @inline final def alloc[T: Alloc](value: T): Ref[T] = runtime.allocRef[T](this)
}

object Region {
  @inline final def apply(): Region = runtime.allocRegion
}

final class Ref[T](val loc: Long) extends AnyVal {
  @inline final def apply(): T = runtime.getRefValue(this)
  @inline final def update(value: T): Unit = runtime.setRefValue(this, value)
  @inline private[regions] final def region: Region = ???
  @inline private[regions] final def offset: Long = ???
}

private[regions] object runtime {
  private[this] val unsafe = {
    import sun.misc.Unsafe
    val f = classOf[Unsafe].getDeclaredField("theUnsafe");
    f.setAccessible(true);
    f.get(null).asInstanceOf[Unsafe]
  }
  def getRefValue[T](ref: Ref[T]): T = ???
  def setRefValue[T](ref: Ref[T], value: T): Unit = ???
  def allocRegion(): Region = ???
  def allocRef[T](region: Region): Ref[T] = ???

  def size[T: Alloc]: Int = ???
}

sealed abstract class Alloc[T]
object Alloc{
  implicit object I1 extends Alloc[Boolean]
  implicit object I8 extends Alloc[Byte]
  implicit object I16 extends Alloc[Short]
  implicit object I32 extends Alloc[Int]
  implicit object I64 extends Alloc[Long]
  implicit object F32 extends Alloc[Float]
  implicit object F64 extends Alloc[Double]
}

