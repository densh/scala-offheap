package scala.offheap
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

  val RegionClass             = staticClass("scala.offheap.Region")
  val PoolRegionClass         = staticClass("scala.offheap.PoolRegion")
  val DirectRegionClass       = staticClass("scala.offheap.DirectRegion")
  val AllocatorClass          = staticClass("scala.offheap.Allocator")
  val ArrayClass              = staticClass("scala.offheap.Array")
  val EmbedArrayClass         = staticClass("scala.offheap.EmbedArray")
  val EmbedClass              = staticClass("scala.offheap.embed")
  val DataClass               = staticClass("scala.offheap.internal.Data")
  val EnumClass               = staticClass("scala.offheap.internal.Enum")
  val ClassTagClass           = staticClass("scala.offheap.internal.ClassTag")
  val ClassTagRangeClass      = staticClass("scala.offheap.internal.ClassTagRange")
  val PotentialChildrenClass  = staticClass("scala.offheap.internal.PotentialChildren")
  val ParentClass             = staticClass("scala.offheap.internal.Parent")
  val PrimaryExtractorClass   = staticClass("scala.offheap.internal.PrimaryExtractor")
  val ParentExtractorClass    = staticClass("scala.offheap.internal.ParentExractor")
  val UniversalExtractorClass = staticClass("scala.offheap.internal.UniversalExtractor")
  val FieldClass              = staticClass("scala.offheap.internal.Field")
  val AnnotsClass             = staticClass("scala.offheap.internal.Annots")
  val CompleteClass           = staticClass("scala.offheap.internal.Complete")
  val CtorClass               = staticClass("scala.offheap.internal.Ctor")

  val RegionModule       = staticModule("scala.offheap.Region")
  val PoolRegionModule   = staticModule("scala.offheap.PoolRegion")
  val DirectRegionModule = staticModule("scala.offheap.DirectRegion")
  val PoolModule         = staticModule("scala.offheap.Pool")
  val ArrayModule        = staticModule("scala.offheap.Array")
  val EmbedArrayModule   = staticModule("scala.offheap.EmbedArray")
  val MemoryModule       = staticModule("scala.offheap.Memory")
  val SanitizerModule    = staticModule("scala.offheap.internal.Sanitizer")
  val MethodModule       = staticModule("scala.offheap.internal.Method")
  val LayoutModule       = staticModule("scala.offheap.internal.Layout")
  val CheckedModule      = staticModule("scala.offheap.internal.Checked")

  val offheap  = staticPackage("scala.offheap")
  val internal = staticPackage("scala.offheap.internal")

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
