import org.scalatest.FunSuite
import regions._

class PrimitiveSuite extends FunSuite {
  test("read/write Byte") {
    Region { implicit r =>
      val ref = Ref(0.toByte)
      assert(ref.get == 0.toByte)
      ref.set(1.toByte)
      assert(ref.get == 1.toByte)
    }
  }

  test("read/write Short") {
    Region { implicit r =>
      val ref = Ref(0.toShort)
      assert(ref.get == 0.toShort)
      ref.set(1.toShort)
      assert(ref.get == 1.toShort)
    }
  }

  test("read/write Int") {
    Region { implicit r =>
      val ref = Ref(0)
      assert(ref.get == 0)
      ref.set(1)
      assert(ref.get == 1)
    }
  }

  test("read/write Long") {
    Region { implicit r =>
      val ref = Ref(0L)
      assert(ref.get == 0L)
      ref.set(1L)
      assert(ref.get == 1L)
    }
  }

  test("read/write Float") {
    Region { implicit r =>
      val ref = Ref(0.0)
      assert(ref.get == 0.0)
      ref.set(1.0)
      assert(ref.get == 1.0)
    }
  }

  test("read/write Double") {
    Region { implicit r =>
      val ref = Ref(0.0d)
      assert(ref.get == 0.0d)
      ref.set(1.0d)
      assert(ref.get == 1.0d)
    }
  }

  test("read/write Boolean") {
    Region { implicit r =>
      val ref = Ref(false)
      assert(ref.get == false)
      ref.set(true)
      assert(ref.get == true)
    }
  }

  test("read/write Char") {
    Region { implicit r =>
      val ref = Ref('a')
      assert(ref.get == 'a')
      ref.set('b')
      assert(ref.get == 'b')
    }
  }

  test("read/write Ref 1") {
    Region { implicit r =>
      val ref1 = Ref(0)
      val ref2 = Ref(ref1)
      assert(ref2.get == ref1)
      ref2.set(Ref.empty[Int])
      assert(ref2.get == Ref.empty[Int])
    }
  }

  test("read/write Ref 2") {
    Region { implicit r =>
      val ref1 = Ref(0)
      val ref2 = Ref(ref1)
      assert(ref2.get == ref1)
      ref2.get.set(1)
      assert(ref1.get == 1)
    }
  }
}
