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
  val checked = !System.getProperties.containsKey("offheap.unchecked")

  def addr(ref: Tree)    = if (checked) q"$ref.addr"    else ref
  def memory(ref: Tree)  = if (checked) q"$ref.memory"  else q"$internal.Unsafer.unsafe"
  def isNull(ref: Tree)  = if (checked) q"$ref == null" else q"$ref == 0L"
  def notNull(ref: Tree) = if (checked) q"$ref != null" else q"$ref != 0L"

  val StringBuilderClass            = staticClass("scala.collection.mutable.StringBuilder")
  val NullPointerExceptionClass     = staticClass("java.lang.NullPointerException")
  val IllegalArgumentExceptionClass = staticClass("java.lang.IllegalArgumentException")

  val RegionClass             = staticClass(s"$prefix.Region")
  val RefClass                = staticClass(s"$prefix.Ref")
  val MemoryClass             = staticClass(s"$prefix.Memory")
  val ArrayClass              = staticClass(s"$prefix.Array")
  val LayoutClass             = staticClass(s"$prefix.Layout")
  val LayoutAnnotationClass   = staticClass(s"$prefix.internal.Layout")
  val DataClass               = staticClass("offheap.internal.Data")
  val EnumClass               = staticClass("offheap.internal.Enum")
  val ClassTagClass           = staticClass("offheap.internal.ClassTag")
  val ClassTagRangeClass      = staticClass("offheap.internal.ClassTagRange")
  val ParentClass             = staticClass("offheap.internal.Parent")
  val PrimaryExtractorClass   = staticClass("offheap.internal.PrimaryExtractor")
  val ParentExtractorClass    = staticClass("offheap.internal.ParentExractor")
  val UniversalExtractorClass = staticClass("offheap.internal.UniversalExtractor")
  val UncheckedClass          = staticClass("offheap.internal.Unchecked")

  val PoolModule   = staticModule(s"$prefix.Pool")
  val ArrayModule  = staticModule(s"$prefix.Array")
  val MemoryModule = staticModule(s"$prefix.Memory")
  val LayoutModule = staticModule(s"$prefix.Layout")
  val MethodModule = staticModule("offheap.internal.Method")

  val offheapx = staticPackage(prefix)
  val offheap  = staticPackage("offheap")
  val internal = staticPackage("offheap.internal")

  val initialize   = TermName("$initialize")
  val tag          = TermName("$tag")
  val ref          = TermName("$ref")
  val canUseMacros = TermName("$canUseMacros")
}
