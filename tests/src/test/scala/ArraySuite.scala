import org.scalatest.FunSuite
import regions._

class ArraySuite extends FunSuite {
  test("read whole array") {
    Region { implicit r =>
      val array = Array(1, 2, 3)
      val ref = Ref(array)
      assert(ref.length == ref().length)
      assert(ref().zip(array).forall { case (a, b) => a == b })
    }
  }

  test("overwrite/read whole array") {
    Region { implicit r =>
      val array = Array(4, 5, 6)
      val ref = Ref(Array(1, 2, 3))
      ref() = array
      assert(ref().zip(array).forall { case (a, b) => a == b })
    }
  }

  // TODO: check failure in case rewritten array has different size

  test("read/write Byte element") {
    Region { implicit r =>
      val ref = Ref(Array(0.toByte))
      assert(ref(0) == 0.toByte)
      ref(0) = 1.toByte
      assert(ref(0) == 1.toByte)
    }
  }

  test("read/write Short element") {
    Region { implicit r =>
      val ref = Ref(Array(0.toShort))
      assert(ref(0) == 0.toShort)
      ref(0) = 1.toShort
      assert(ref(0) == 1.toShort)
    }
  }

  test("read/write Int element") {
    Region { implicit r =>
      val ref = Ref(Array(0))
      assert(ref.length == 1)
      assert(ref(0) == 0)
      ref(0) = 1
      assert(ref(0) == 1)
    }
  }

  test("read/write Long element") {
    Region { implicit r =>
      val ref = Ref(Array(0L))
      assert(ref(0) == 0L)
      ref(0) = 1L
      assert(ref(0) == 1L)
    }
  }
}
