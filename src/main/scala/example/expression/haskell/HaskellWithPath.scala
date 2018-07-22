package example.expression.haskell     /*DI:LD:AI*/

import java.nio.file.Path

import org.combinators.templating.persistable.Persistable


/** A python fragment to be stored at `persistTo`. */
case class HaskellWithPath(code: Haskell, persistTo: Path)

trait HaskellWithPathPersistableInstances {
  /** Persistable instance for [HaskellWithPath]. */
  implicit def pythonWithPathPersistable: HaskellWithPathPersistable.Aux[HaskellWithPath] = new Persistable {
    type T = HaskellWithPath
    def rawText(elem: HaskellWithPath): Array[Byte] = elem.code.getCode.getBytes
    def path(elem: HaskellWithPath): Path = elem.persistTo
  }
}

object HaskellWithPathPersistable extends HaskellWithPathPersistableInstances {
  type Aux[TT] = Persistable { type T = TT }
  def apply[T](implicit persistable: Aux[T]): Aux[T] = persistable
}
