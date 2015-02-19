package offheap

import scala.language.experimental.{macros => CanMacro}

class Ref private(private val addr: Long) extends AnyVal
object Ref {
  def toAddress(ref: Ref): Long = ref.addr
  def fromAddress(addr: Long): Ref = new Ref(addr)
}
