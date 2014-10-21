import org.scalatest.FunSuite
import regions._

class CombinatorSuite extends FunSuite {
  test("nonempty get set") {
    Region { implicit r =>
      val ref = Ref(1)
      assert(ref.get == 1)
      ref.set(2)
      assert(ref.get == 2)
    }
  }

  test("nonempty nonstable get set") {
    Region { implicit r =>
      var x = 0
      val ref = Ref(1)
      def getRef = if (x == 0) { x = 1; ref } else { Ref.empty[Int] }
      getRef.set(2)
      assert(ref.get == 2)
    }
  }

  test("nonempty isEmpty") {
    Region { implicit r =>
      assert(!Ref(1).isEmpty)
    }
  }

  test("nonempty nonEmpty") {
    Region { implicit r =>
      assert(Ref(1).nonEmpty)
    }
  }

  test("nonempty contains") {
    Region { implicit r =>
      val ref = Ref(1)(r)
      assert(ref.contains(1))
      assert(!ref.contains(0))
    }
  }

  test("nonempty flatten #1") {
    Region { implicit r =>
      val ref = Ref(1)
      val refref = Ref(ref)
      assert(refref.flatten == ref)
    }
  }

  test("nonempty flatten #2") {
    Region { implicit r =>
      val refref = Ref(Ref.empty[Int])
      assert(refref.flatten == Ref.empty[Int])
    }
  }

  test("nonempty map #1") {
    Region { implicit r =>
      val ref = Ref(1)
      val ref2 = ref.map(_ + 1)
      assert(ref2.get == 2)
    }
  }

  test("nonempty map #2") {
    Region { implicit r =>
      val ref = Ref(1)
      val f: Int => Int = _ + 1
      val ref2 = ref.map(f)
      assert(ref2.get == 2)
    }
  }

  test("nonempty fold") {
    Region { implicit r =>
      val ref = Ref(1)
      assert(ref.fold(0)(_ + 1) == 2)
    }
  }

  test("empty isEmpty") {
    assert(Ref.empty[Int].isEmpty)
  }

  test("empty nonEmpty") {
    assert(!Ref.empty[Int].nonEmpty)
  }

  test("empty get") {
    intercept[EmptyRefException.type] {
      Ref.empty[Int].get
    }
  }

  test("empty getOrElse") {
    assert(Ref.empty[Int].getOrElse(0) == 0)
  }

  test("empty set") {
    intercept[EmptyRefException.type] {
      Ref.empty[Int].set(42)
    }
  }

  test("empty setOrElse") {
    case object MyException extends Exception
    intercept[MyException.type] {
      Ref.empty[Int].setOrElse(0) { throw MyException }
    }
  }

  test("empty contains") {
    assert(!Ref.empty[Int].contains(0))
  }

  test("empty flatten") {
    assert(Ref.empty[Ref[Int]].flatten == Ref.empty[Int])
  }

  test("empty map") {
    Region { implicit r =>
      assert(Ref.empty[Int].map(_ + 1) == Ref.empty[Int])
    }
  }

  test("empty fold") {
    assert(Ref.empty[Int].fold(0)(identity) == 0)
  }
}
