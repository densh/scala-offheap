import org.scalatest.FunSuite
import regions._

class StructSuite extends FunSuite {
  test("read/write point fields") {
    @struct class Point(x: Int, y: Int)
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

  test("read/write nested fields") {
    @struct class A(value: Int)
    @struct class B(value: Int, nested: Ref[A])
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

  // TODO: find a way to express this
  // test("read/write embedded fields") {
  //   @struct class A(value: Int)
  //   @struct class B(value: Int, nested: A)
  //   Region { implicit r =>
  //     val p = Ref[B](1, A(2))
  //   }
  // }
}
