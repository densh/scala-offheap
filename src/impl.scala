package offheap
package internal

import scala.language.experimental.{macros => CanMacro}

object Region {
  def open(): Region = ???
  def close(r: Region): Unit = ???
  def allocate(r: Region, size: Long): Long = ???
}

object Unsafe {
  val unsafe: sun.misc.Unsafe =
    try {
      sun.misc.Unsafe.getUnsafe()
    } catch {
      case _: java.lang.SecurityException =>
        val f = classOf[sun.misc.Unsafe].getDeclaredField("theUnsafe");
        f.setAccessible(true);
        f.get(null).asInstanceOf[sun.misc.Unsafe]
    }
}

object Method {
  def accessor[C, T](addr: Long, name: String): T =
    macro macros.Method.accessor[C, T]

  def assigner[C, T](addr: Long, name: String, value: T): Unit =
    macro macros.Method.assigner[C, T]

  def allocator[C](r: offheap.Region, args: Any*): C =
    macro macros.Method.allocator[C]

  def method[T](body: T): T =
    macro macros.Method.method[T]

  def copy[C](r: Region, args: Any*): C =
    macro macros.Method.copy[C]

  def toString[C]: String =
    macro macros.Method.toString[C]
}

class PagePool {
  def claim: Long = ???
  def reclaim(addr: Long): Unit = ???
  override def finalize(): Unit = ???
}
object PagePool {
  val local = new ThreadLocal[PagePool] {
    override protected def initialValue(): PagePool = new PagePool
  }
}
