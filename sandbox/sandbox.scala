import regions._
object Test extends App {
  @offheap class Point(x: Int, y: Int)
  Region { implicit r =>
    val p: Point = Point(10, 20)
    println(p.x + p.y)
  }
}
