import regions._
object Test extends App {
    Region { implicit r =>
      var x = 0
      val ref = Ref(1)
      def getRef = if (x == 0) { x = 1; ref } else { Ref.empty[Int] }
      getRef.set(2)
      assert(ref.get == 2)
    }
}

