package example.expression.scala   /*DI:LD:AI*/

import java.nio.file.Path
import org.combinators.templating.persistable.Persistable
import scala.meta.Source
import java.nio.file.Paths

/** A Scala fragment to be stored at `persistTo`. */
trait ScalaWithPath {
  val code:Source
  val persistTo: Path
  val destination: String
}
case class ScalaMainWithPath(code: Source, persistTo: Path) extends ScalaWithPath {
  val destination = "main"
}
case class ScalaTestWithPath(code: Source, persistTo: Path) extends ScalaWithPath {
  val destination = "test"
}

trait ScalaWithPathPersistableInstances {
  /** Persistable instance for [ScalaWithPath]. */
  implicit def gjWithPathPersistable:ScalaWithPathPersistable.Aux[ScalaWithPath] = new Persistable {
    type T = ScalaWithPath
    def rawText(elem: ScalaWithPath): Array[Byte] = elem.code.syntax.getBytes
    def path(elem: ScalaWithPath): Path = {

      val fullPath = "src" +: elem.destination +: "scala" +: elem.persistTo.toString +: Seq.empty
      Paths.get(fullPath.head, fullPath.tail : _*)
    }
  }
}

object ScalaWithPathPersistable extends ScalaWithPathPersistableInstances {
  type Aux[TT] = Persistable { type T = TT }
  def apply[T](implicit persistable: Aux[T]): Aux[T] = persistable
}
