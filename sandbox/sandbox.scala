import regions._
object Test extends App {
  def test(name: String)(f: => Unit): Unit = {
    println(s"running $name")
    f
  }
}

