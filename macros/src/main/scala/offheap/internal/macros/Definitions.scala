package scala.offheap
package internal
package macros

import scala.reflect.macros.blackbox

trait Definitions {
  val c: blackbox.Context

  import c.universe._
  import c.universe.definitions._
  import c.universe.rootMirror._

  val StringBuilderClass                 = staticClass("scala.collection.mutable.StringBuilder")
  val NullPointerExceptionClass          = staticClass("java.lang.NullPointerException")
  val IllegalArgumentExceptionClass      = staticClass("java.lang.IllegalArgumentException")
  val IndexOutOfBoundsExceptionClass     = staticClass("java.lang.IndexOutOfBoundsException")
  val UnsupportedOperationExceptionClass = staticClass("java.lang.UnsupportedOperationException")

  val RegionClass             = staticClass("scala.offheap.Region")
  val PoolRegionClass         = staticClass("scala.offheap.PoolRegion")
  val AllocatorClass          = staticClass("scala.offheap.Allocator")

  val RegionModule     = staticModule("scala.offheap.Region")
  val PoolRegionModule = staticModule("scala.offheap.PoolRegion")
  val PoolModule       = staticModule("scala.offheap.Pool")
  val MemoryModule     = staticModule("scala.offheap.Memory")

  val offheap  = staticPackage("scala.offheap")
  val internal = staticPackage("scala.offheap.internal")

  val AddrTpe      = LongTpe
  val SizeTpe      = LongTpe

}
