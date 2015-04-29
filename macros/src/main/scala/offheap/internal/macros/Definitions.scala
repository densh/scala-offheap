package offheap
package internal
package macros

import scala.reflect.macros.blackbox

trait Definitions {
  val c: blackbox.Context

  import c.universe._
  import c.universe.definitions._
  import c.universe.rootMirror._

  val bitDepth: Int = 64
  private val prefix = s"offheap.x$bitDepth"

  val AddrTpe = LongClass.toType
  val SizeTpe = LongClass.toType

  val StringBuilderClass            = staticClass("scala.collection.mutable.StringBuilder")
  val NullPointerExceptionClass     = staticClass("java.lang.NullPointerException")
  val IllegalArgumentExceptionClass = staticClass("java.lang.IllegalArgumentException")

  val RegionClass             = staticClass(s"$prefix.Region")
  val MemoryClass             = staticClass(s"$prefix.Memory")
  val ArrayClass              = staticClass(s"$prefix.Array")
  val LayoutClass             = staticClass(s"$prefix.internal.Layout")
  val FieldsClass             = staticClass(s"$prefix.internal.Fields")
  val DataClass               = staticClass("offheap.internal.Data")
  val EnumClass               = staticClass("offheap.internal.Enum")
  val ClassTagClass           = staticClass("offheap.internal.ClassTag")
  val ClassTagRangeClass      = staticClass("offheap.internal.ClassTagRange")
  val ParentClass             = staticClass("offheap.internal.Parent")
  val PrimaryExtractorClass   = staticClass("offheap.internal.PrimaryExtractor")
  val ParentExtractorClass    = staticClass("offheap.internal.ParentExractor")
  val UniversalExtractorClass = staticClass("offheap.internal.UniversalExtractor")

  val PoolModule   = staticModule(s"$prefix.Pool")
  val ArrayModule  = staticModule(s"$prefix.Array")
  val MemoryModule = staticModule(s"$prefix.Memory")
  val FieldsModule = staticModule(s"$prefix.internal.Fields")
  val MethodModule = staticModule("offheap.internal.Method")

  val offheapx = staticPackage(prefix)
  val offheap  = staticPackage("offheap")
  val internal = staticPackage("offheap.internal")

  val initialize   = TermName("$initialize")
  val tag          = TermName("$tag")
  val addr         = TermName("$addr")
  val canUseMacros = TermName("$canUseMacros")
}
