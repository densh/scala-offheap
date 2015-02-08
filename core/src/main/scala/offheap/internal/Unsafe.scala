package offheap
package internal

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
