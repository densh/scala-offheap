import org.scalatest.FunSuite
import regions._

class StructSuite extends FunSuite {
  @struct class Point(x: Int, y: Int)
  test("read/write point fields") {
    Region { implicit r =>
      val p = Ref[Point](1, 2)
      assert(p.x == 1)
      assert(p.y == 2)
      p.x = 10
      p.y = 20
      assert(p.x == 10)
      assert(p.y == 20)
    }
  }

  @struct class A(value: Int)
  @struct class B(value: Int, nested: Ref[A])
  test("read/write nested fields") {
    Region { implicit r =>
      val p = Ref[B](1, Ref[A](2))
      assert(p.value == 1)
      assert(p.nested.value == 2)
      p.value = 10
      p.nested.value = 20
      assert(p.value == 10)
      assert(p.nested.value == 20)
    }
  }

  @struct class A2(value: Int)
  @struct class B2(value: Int, nested: A2)
  test("read/write embedded fields") {
    Region { implicit r =>
      val p = Ref[B2](1, A2(2))
      Predef.assert(p.value == 1)
      Predef.assert(p.nested.value == 2)
    }
  }
}
