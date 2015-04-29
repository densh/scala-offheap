package offheap
package internal

import scala.annotation.StaticAnnotation
import scala.language.experimental.{macros => CanMacro}

final class Data extends StaticAnnotation
final class Enum extends StaticAnnotation
final class Parent(tag: Class[_]) extends StaticAnnotation
final class Children(tags: Class[_]*) extends StaticAnnotation
final class ClassTag(tag: Any) extends StaticAnnotation
final class ClassTagRange(from: Any, to: Any) extends StaticAnnotation // > from <= to
final class ParentExractor(tag: Class[_], value: Any) extends StaticAnnotation
final class PrimaryExtractor(value: Any) extends StaticAnnotation
final class UniversalExtractor(value: Any) extends StaticAnnotation
final class Layout(val fields: Fields) extends StaticAnnotation
final class Fields(val fields: (String, Class[_], Size)*)
object Fields {
  def layout[C](pairs: (String, Class[_])*): Fields = macro macros.Layout.perform[C]
}
