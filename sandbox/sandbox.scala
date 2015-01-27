import regions._
object Test extends App {
  @region(R) class Point(x: Int, y: Int)

  {
    implicit val r = Region.open
    val p = Point(10, 20)
    val Point(x, y) = p
    println(x + y)
  }
}
