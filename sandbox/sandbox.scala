import regions._
object Test extends App {
  @struct class Point(x: Int, y: Int)
  withRegion { r =>
    val p = r.alloc[Point](x = 10, y = 20)
    println(p.x)
    println(p.y)
  }
}
