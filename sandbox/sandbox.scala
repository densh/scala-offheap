import regions._
object Test extends App {
  val tmp = {
    Region { r =>
      Ref(1)(r)

    }
  }.toString
  println(tmp)
}
