package offheap
package x64

final class Alignment private(val value: Size) extends AnyVal
object Alignment {
  def apply(value: Size) = {
    assert(value >= 1, "alignment must be a non-zero positive number")
    new Alignment(value)
  }

  implicit val default = Alignment(Memory.sizeOf[Long])
  val none = Alignment(1)
}
