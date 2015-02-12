package test
import offheap.internal.C._
  @struct class Data(size: Long, arr: Ptr[Long])
object Test extends App {
  val p = Ptr.alloc[Data]
  p.size = 4
  p.arr = Ptr.allocArray[Long](4)
  for (i <- 0 to 3) println(p.arr(i))
}
