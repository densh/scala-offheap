package offheap
package internal
package macros

import scala.reflect.macros.whitebox

class Unsafe(val c: whitebox.Context) extends Common {
  import c.universe._
  import c.universe.definitions._
  import c.internal._

  def annotation(annottees: Tree*) = annottees match {
    case q"$mods def $name[..$targs](...$argss): $tpt = $body" :: Nil =>
      val unsafeName = fresh(name.toString + "$unsafe")
      val targNames  = targs.map { _.name }
      val argNamess  = argss.map { _.map { _.name } }
      val ctoMsg     = s"Method $name can only be called in unsafe context."
      val ctoMods    = mods.mapAnnotations { anns =>
        q"new $compileTimeOnlyClass($ctoMsg)" ::
        q"new $UnsafeClass(${unsafeName.toString})" :: anns
      }
      q"""
        def $unsafeName[..$targs](...$argss): $tpt =
          $unsafeModule.apply { $body }
        $ctoMods def $name[..$targs](...$argss): $tpt =
          $unsafeName[..$targNames](...$argNamess)
      """
    case _ =>
      abort("@unsafe annotation is only supported on methods.")
  }

  def scope(f: Tree) = typingTransform(f) {
    case (q"$method[..$tpes](...$values)", api)
      if method.symbol != null && ExtractUnsafe.has(method.symbol) =>
      val ExtractUnsafe(q"new $_(${nameStr: String})" :: Nil) = method.symbol
      val unsafeName = TermName(nameStr)
      val q"$pre.$_" = method
      api.typecheck(q"$pre.$unsafeName[..$tpes](...$values)")
    case (tree, api) =>
      api.default(tree)
  }
}
