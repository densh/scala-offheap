package offheap
package internal
package macros

import scala.reflect.macros.whitebox

class Unsafe(val c: whitebox.Context) extends Common {
  import c.universe._, Flag._
  import c.universe.definitions._
  import c.internal._

  def annotation(annottees: Tree*) = annottees match {
    case q"$mods def $name[..$targs](...$argss): $tpt = $body" :: Nil =>
      val targNames  = targs.map { _.name }
      val argNamess  = argss.map { _.map { _.name } }
      val unsafeName = TermName(name.toString + "$unsafe")
      val unsafeBody = if (body.isEmpty) q"" else q"$unsafeModule.apply { $body }"
      val unsafeMods = {
        val overrideFlag = if (mods.hasFlag(OVERRIDE)) OVERRIDE else NoFlags
        val deferredFlag = if (body.isEmpty)           DEFERRED else NoFlags
        Modifiers(overrideFlag | deferredFlag)
      }
      val ctoMsg     = s"Method $name can only be called in unsafe context."
      val ctoMods    = mods.mapAnnotations { anns =>
        q"new $compileTimeOnlyClass($ctoMsg)" ::
        q"new $UnsafeClass" :: anns
      }
      val ctoBody    = if (body.isEmpty) q"" else q"$unsafeName[..$targNames](...$argNamess)"
      q"""
        $unsafeMods def $unsafeName[..$targs](...$argss): $tpt = $unsafeBody
        $ctoMods def $name[..$targs](...$argss): $tpt = $ctoBody
      """
    case _ =>
      abort("@unsafe annotation is only supported on methods.")
  }

  def scope(f: Tree) = typingTransform(f) {
    case (q"$method[..$tpes](...$values)", api)
      if method.symbol != null && ExtractUnsafe.has(method.symbol) =>
      val q"$pre.$name" = method
      val unsafeName = TermName(name.toString + "$unsafe")
      api.typecheck(q"$pre.$unsafeName[..$tpes](...$values)")
    case (tree, api) =>
      api.default(tree)
  }
}
