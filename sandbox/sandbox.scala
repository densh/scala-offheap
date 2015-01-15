import regions._
object Test extends App {
  @offheap class Point(x: Int, y: Int)
  Region { implicit r =>
    val ref: Ref[Point] = Ref(Point(10, 20))
    println(ref.get(p => p.x + p.y))
  }
}
