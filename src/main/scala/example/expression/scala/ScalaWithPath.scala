package example.expression.scala   /*DI:LD:AI*/

import java.nio.file.Path

import org.combinators.templating.persistable.Persistable
import scala.meta.Source

/** A Scala fragment to be stored at `persistTo`. */
case class ScalaWithPath(code: Source, persistTo: Path)

trait ScalaWithPathPersistableInstances {
  /** Persistable instance for [ScalaWithPath]. */
  implicit def gjWithPathPersistable:ScalaWithPathPersistable.Aux[ScalaWithPath] = new Persistable {
    type T = ScalaWithPath
    def rawText(elem: ScalaWithPath): Array[Byte] = elem.code.syntax.getBytes
    def path(elem: ScalaWithPath): Path = elem.persistTo
  }
}

object ScalaWithPathPersistable extends ScalaWithPathPersistableInstances {
  type Aux[TT] = Persistable { type T = TT }
  def apply[T](implicit persistable: Aux[T]): Aux[T] = persistable
}
