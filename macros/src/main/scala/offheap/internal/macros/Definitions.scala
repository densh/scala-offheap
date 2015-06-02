package offheap
package internal
package macros

import scala.reflect.macros.blackbox

trait Definitions {
  val c: blackbox.Context

  import c.universe._
  import c.universe.definitions._
  import c.universe.rootMirror._

  val StringBuilderClass             = staticClass("scala.collection.mutable.StringBuilder")
  val NullPointerExceptionClass      = staticClass("java.lang.NullPointerException")
  val IllegalArgumentExceptionClass  = staticClass("java.lang.IllegalArgumentException")
  val IndexOutOfBoundsExceptionClass = staticClass("java.lang.IndexOutOfBoundsException")

  val RegionClass             = staticClass("offheap.Region")
  val PoolRegionClass         = staticClass("offheap.PoolRegion")
  val DirectRegionClass       = staticClass("offheap.DirectRegion")
  val AllocatorClass          = staticClass("offheap.Allocator")
  val ArrayClass              = staticClass("offheap.Array")
  val EmbedArrayClass         = staticClass("offheap.EmbedArray")
  val EmbedClass              = staticClass("offheap.embed")
  val DataClass               = staticClass("offheap.internal.Data")
  val EnumClass               = staticClass("offheap.internal.Enum")
  val ClassTagClass           = staticClass("offheap.internal.ClassTag")
  val ClassTagRangeClass      = staticClass("offheap.internal.ClassTagRange")
  val PotentialChildrenClass  = staticClass("offheap.internal.PotentialChildren")
  val ParentClass             = staticClass("offheap.internal.Parent")
  val PrimaryExtractorClass   = staticClass("offheap.internal.PrimaryExtractor")
  val ParentExtractorClass    = staticClass("offheap.internal.ParentExractor")
  val UniversalExtractorClass = staticClass("offheap.internal.UniversalExtractor")
  val FieldClass              = staticClass("offheap.internal.Field")
  val AnnotsClass             = staticClass("offheap.internal.Annots")
  val CompleteClass           = staticClass("offheap.internal.Complete")
  val CtorClass               = staticClass("offheap.internal.Ctor")

  val RegionModule       = staticModule("offheap.Region")
  val PoolRegionModule   = staticModule("offheap.PoolRegion")
  val DirectRegionModule = staticModule("offheap.DirectRegion")
  val PoolModule         = staticModule("offheap.Pool")
  val ArrayModule        = staticModule("offheap.Array")
  val EmbedArrayModule   = staticModule("offheap.EmbedArray")
  val SanitizerModule    = staticModule("offheap.internal.Sanitizer")
  val MethodModule       = staticModule("offheap.internal.Method")
  val LayoutModule       = staticModule("offheap.internal.Layout")
  val MemoryModule       = staticModule("offheap.internal.Memory")
  val CheckedModule      = staticModule("offheap.internal.Checked")

  val offheap  = staticPackage("offheap")
  val internal = staticPackage("offheap.internal")

  val AddrTpe      = LongTpe
  val SizeTpe      = LongTpe
  val ArrayTpe     = ArrayClass.toType
  val ArraySizeTpe = IntTpe

  val initializer  = TermName("$init")
  val layout       = TermName("$layout")
  val tag          = TermName("$tag")
  val canUseMacros = TermName("$canUseMacros")
  val complete     = TermName("$complete")
}
