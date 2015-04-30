package test

import java.{lang => jl}
import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import offheap.Sanitizer._

class PackingSuite extends Properties("Packing") {
  val ADDR_MASK = jl.Long.MAX_VALUE >> 24
  val ids = for (id <- Gen.choose(jl.Short.MIN_VALUE, jl.Short.MAX_VALUE)) yield id
  val addrs = for (l <- Gen.choose(jl.Long.MIN_VALUE, jl.Long.MAX_VALUE)) yield l & ADDR_MASK
  property("pack/unpackId") = forAll(ids, addrs) { (id, addr) =>
    unpackId(pack(id, addr)) == id
  }
  property("pack/unpackAddr") = forAll(ids, addrs) { (id: Short, addr: Long) =>
    unpackAddr(pack(id, addr)) == addr
  }
}
