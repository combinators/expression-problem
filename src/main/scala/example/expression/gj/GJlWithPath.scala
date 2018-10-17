package example.expression.gj    /*DI:LD:AI*/

import java.nio.file.Path

import org.combinators.templating.persistable.Persistable

/** A GJ fragment to be stored at `persistTo`. */
case class GJWithPath(code: GJ, persistTo: Path)

trait GJWithPathPersistableInstances {
  /** Persistable instance for [HaskellWithPath]. */
  implicit def gjWithPathPersistable: GJWithPathPersistable.Aux[GJWithPath] = new Persistable {
    type T = GJWithPath
    def rawText(elem: GJWithPath): Array[Byte] = elem.code.getCode.getBytes
    def path(elem: GJWithPath): Path = elem.persistTo
  }
}

object GJWithPathPersistable extends GJWithPathPersistableInstances {
  type Aux[TT] = Persistable { type T = TT }
  def apply[T](implicit persistable: Aux[T]): Aux[T] = persistable
}
