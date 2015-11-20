package test

import org.scalatest.FunSuite
import offheap._

trait AlignmentSuite extends FunSuite {
  val alloc: Allocator

  test("allocate aligned 1") {
    val addr1 = alloc.allocate(1, alignment = 1)
    val addr2 = alloc.allocate(1, alignment = 1)
    val addr3 = alloc.allocate(1, alignment = 1)
    val addr4 = alloc.allocate(1, alignment = 1)
    assert(addr1 % 1 == 0)
    assert(addr2 % 1 == 0)
    assert(addr3 % 1 == 0)
    assert(addr4 % 1 == 0)
  }

  test("allocate aligned 2") {
    val addr1 = alloc.allocate(1, alignment = 2)
    val addr2 = alloc.allocate(1, alignment = 2)
    val addr3 = alloc.allocate(1, alignment = 2)
    val addr4 = alloc.allocate(1, alignment = 2)
    assert(addr1 % 2 == 0)
    assert(addr2 % 2 == 0)
    assert(addr3 % 2 == 0)
    assert(addr4 % 2 == 0)
  }

  test("allocate aligned 4") {
    val addr1 = alloc.allocate(1, alignment = 4)
    val addr2 = alloc.allocate(1, alignment = 4)
    val addr3 = alloc.allocate(1, alignment = 4)
    val addr4 = alloc.allocate(1, alignment = 4)
    assert(addr1 % 4 == 0)
    assert(addr2 % 4 == 0)
    assert(addr3 % 4 == 0)
    assert(addr4 % 4 == 0)
  }

  test("allocate aligned 8") {
    val addr1 = alloc.allocate(1, alignment = 8)
    val addr2 = alloc.allocate(1, alignment = 8)
    val addr3 = alloc.allocate(1, alignment = 8)
    val addr4 = alloc.allocate(1, alignment = 8)
    assert(addr1 % 8 == 0)
    assert(addr2 % 8 == 0)
    assert(addr3 % 8 == 0)
    assert(addr4 % 8 == 0)
  }

  test("allocate aligned 16") {
    val addr1 = alloc.allocate(1, alignment = 16)
    val addr2 = alloc.allocate(1, alignment = 16)
    val addr3 = alloc.allocate(1, alignment = 16)
    val addr4 = alloc.allocate(1, alignment = 16)
    assert(addr1 % 16 == 0)
    assert(addr2 % 16 == 0)
    assert(addr3 % 16 == 0)
    assert(addr4 % 16 == 0)
  }

  test("allocate aligned 32") {
    val addr1 = alloc.allocate(1, alignment = 32)
    val addr2 = alloc.allocate(1, alignment = 32)
    val addr3 = alloc.allocate(1, alignment = 32)
    val addr4 = alloc.allocate(1, alignment = 32)
    assert(addr1 % 32 == 0)
    assert(addr2 % 32 == 0)
    assert(addr3 % 32 == 0)
    assert(addr4 % 32 == 0)
  }

  test("allocate aligned 64") {
    val addr1 = alloc.allocate(1, alignment = 64)
    val addr2 = alloc.allocate(1, alignment = 64)
    val addr3 = alloc.allocate(1, alignment = 64)
    val addr4 = alloc.allocate(1, alignment = 64)
    assert(addr1 % 64 == 0)
    assert(addr2 % 64 == 0)
    assert(addr3 % 64 == 0)
    assert(addr4 % 64 == 0)
  }

  test("allocate aligned 128") {
    val addr1 = alloc.allocate(1, alignment = 128)
    val addr2 = alloc.allocate(1, alignment = 128)
    val addr3 = alloc.allocate(1, alignment = 128)
    val addr4 = alloc.allocate(1, alignment = 128)
    assert(addr1 % 128 == 0)
    assert(addr2 % 128 == 0)
    assert(addr3 % 128 == 0)
    assert(addr4 % 128 == 0)
  }
}

class RegionAllignmentSuite extends AlignmentSuite {
  val alloc = Region.open(Region.Props(Pool()))
}

class MallocAlignmentSuite extends AlignmentSuite {
  val alloc = malloc
}
