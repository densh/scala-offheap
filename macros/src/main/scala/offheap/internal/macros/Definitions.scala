package offheap
package internal
package macros

import scala.reflect.macros.blackbox

trait Definitions {
  val c: blackbox.Context

  import c.universe._
  import c.universe.definitions._
  import c.universe.rootMirror._

  val AddrTpe = LongClass.toType
  val SizeTpe = LongClass.toType

  val StringBuilderClass            = staticClass("scala.collection.mutable.StringBuilder")
  val NullPointerExceptionClass     = staticClass("java.lang.NullPointerException")
  val IllegalArgumentExceptionClass = staticClass("java.lang.IllegalArgumentException")

  val RegionClass             = staticClass("offheap.Region")
  val AllocatorClass          = staticClass("offheap.Allocator")
  val ArrayClass              = staticClass("offheap.Array")
  val LayoutClass             = staticClass("offheap.internal.Layout")
  val FieldsClass             = staticClass("offheap.internal.Fields")
  val DataClass               = staticClass("offheap.internal.Data")
  val EnumClass               = staticClass("offheap.internal.Enum")
  val ClassTagClass           = staticClass("offheap.internal.ClassTag")
  val ClassTagRangeClass      = staticClass("offheap.internal.ClassTagRange")
  val ParentClass             = staticClass("offheap.internal.Parent")
  val PrimaryExtractorClass   = staticClass("offheap.internal.PrimaryExtractor")
  val ParentExtractorClass    = staticClass("offheap.internal.ParentExractor")
  val UniversalExtractorClass = staticClass("offheap.internal.UniversalExtractor")

  val RegionModule    = staticModule("offheap.Region")
  val PoolModule      = staticModule("offheap.Pool")
  val ArrayModule     = staticModule("offheap.Array")
  val SanitizerModule = staticModule("offheap.internal.Sanitizer")
  val FieldsModule    = staticModule("offheap.internal.Fields")
  val MethodModule    = staticModule("offheap.internal.Method")

  val offheap  = staticPackage("offheap")
  val internal = staticPackage("offheap.internal")

  val initialize   = TermName("$initialize")
  val tag          = TermName("$tag")
  val addr         = TermName("$addr")
  val canUseMacros = TermName("$canUseMacros")

  val UNSAFE  = q"$internal.UnsafeHolder.UNSAFE"
  val CHECKED = q"$internal.CheckedHolder.CHECKED"
}
