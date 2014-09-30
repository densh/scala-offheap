import org.scalatest.FunSuite
import regions._

class PrimitiveSuite extends FunSuite {
  test("read/write Byte") {
    Region { implicit r =>
      val ref = Ref(0.toByte)
      assert(ref() == 0.toByte)
      ref() = 1.toByte
      assert(ref() == 1.toByte)
    }
  }

  test("read/write Short") {
    Region { implicit r =>
      val ref = Ref(0.toShort)
      assert(ref() == 0.toShort)
      ref() = 1.toShort
      assert(ref() == 1.toShort)
    }
  }

  test("read/write Int") {
    Region { implicit r =>
      val ref = Ref(0)
      assert(ref() == 0)
      ref() = 1
      assert(ref() == 1)
    }
  }

  test("read/write Long") {
    Region { implicit r =>
      val ref = Ref(0L)
      assert(ref() == 0L)
      ref() = 1L
      assert(ref() == 1L)
    }
  }

  test("read/write Float") {
    Region { implicit r =>
      val ref = Ref(0.0)
      assert(ref() == 0.0)
      ref() = 1.0
      assert(ref() == 1.0)
    }
  }

  test("read/write Double") {
    Region { implicit r =>
      val ref = Ref(0.0d)
      assert(ref() == 0.0d)
      ref() = 1.0d
      assert(ref() == 1.0d)
    }
  }

  test("read/write Boolean") {
    Region { implicit r =>
      val ref = Ref(false)
      assert(ref() == false)
      ref() = true
      assert(ref() == true)
    }
  }

  test("read/write Char") {
    Region { implicit r =>
      val ref = Ref('a')
      assert(ref() == 'a')
      ref() = 'b'
      assert(ref() == 'b')
    }
  }
}
