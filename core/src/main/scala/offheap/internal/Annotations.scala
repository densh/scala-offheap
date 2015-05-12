package offheap
package internal

import scala.annotation.StaticAnnotation
import scala.language.experimental.{macros => CanMacro}

final class Data extends StaticAnnotation
final class Enum extends StaticAnnotation
final class Parent(tag: Class[_]) extends StaticAnnotation
final class PotentialChildren(tags: Class[_]*) extends StaticAnnotation
final class ClassTag(tag: Any) extends StaticAnnotation
final class ClassTagRange(from: Any, to: Any) extends StaticAnnotation // > from <= to
final class ParentExractor(tag: Class[_], value: Any) extends StaticAnnotation
final class PrimaryExtractor(value: Any) extends StaticAnnotation
final class UniversalExtractor(value: Any) extends StaticAnnotation
final class Field(name: String, after: Any, tag: Class[_],
                  annots: Class[_], offset: Size) extends StaticAnnotation
object Field {
  def offset(after: Any, tag: Class[_], annots: Class[_]): Size =
    macro macros.Layout.fieldOffset
}
