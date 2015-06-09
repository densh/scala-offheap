package scala.offheap
package internal
package macros

import scala.reflect.macros.whitebox

class WhiteboxMethod(val c: whitebox.Context) extends Common {
  import c.universe.{ weakTypeOf => wt, _ }

  def unapply[C: WeakTypeTag](scrutinee: Tree): Tree = {
    val q"$_(..$args)" = c.macroApplication
    val C = wt[C]
    val T = scrutinee.tpe
    val extractor =
      if (C =:= T) {
        val ExtractPrimaryExtractor(q"new $_($_.${extractor: TermName})" :: Nil) = C.typeSymbol
        extractor
      } else if (isParent(T, C)) {
        val ExtractParentExtractor(extractors) = C.typeSymbol
        extractors.collectFirst {
          case q"new $_(${tpe: Type}, $_.${extractor: TermName})"
            if tpe.typeSymbol == T.typeSymbol =>
            extractor
        }.get
      } else {
        val ExtractUniversalExtractor(q"new $_($_.${extractor: TermName})" :: Nil) = C.typeSymbol
        extractor
      }
    val companion = C.typeSymbol.companion
    q"$companion.$extractor.unapply(..$args)"
  }

  def coerce[C: WeakTypeTag, T: WeakTypeTag](t: Tree): Tree = {
    val T = wt[T]
    val C = wt[C]
    if (isParent(C, T)) cast(t, T, C)
    else abort(s"can't coerce $T to $C")
  }
}
