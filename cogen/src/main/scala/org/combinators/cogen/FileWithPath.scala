package org.combinators.cogen

import org.combinators.templating.persistable.Persistable

import java.nio.file.Path

/**
  * Contents of file to be stored at `persistTo`.
  *
  * Used for any general-purpose resource (like build.sbt file or compile.sh script).
  *
  * When data is BINARY, must load up using rawBytes.
  */
case class FileWithPath(rawBytes: Array[Byte], persistTo: Path, charset: java.nio.charset.Charset = java.nio.charset.Charset.defaultCharset()) {
  override def toString: String = s"FileWithPath(${new String(rawBytes, charset)}, $persistTo, $charset)"
}

object FileWithPath {
  def apply(code: String, persistTo: Path, charset: java.nio.charset.Charset): FileWithPath =
    FileWithPath(code.getBytes(charset), persistTo)
  def apply(code: String, persistTo: Path): FileWithPath =
    FileWithPath(code, persistTo, java.nio.charset.Charset.defaultCharset())
}

trait FileWithPathPersistableInstances {
  /** Persistable instance for [FileWithPath]. */
  implicit def fileWithPathPersistable: FileWithPathPersistable.Aux[FileWithPath] = new Persistable {
    type T = FileWithPath
    def rawText(elem: FileWithPath): Array[Byte] = {
      elem.rawBytes
    }
    def path(elem: FileWithPath): Path = elem.persistTo
  }
}

object FileWithPathPersistable extends FileWithPathPersistableInstances {
  type Aux[TT] = Persistable { type T = TT }
  def apply[T](implicit persistable: Aux[T]): Aux[T] = persistable
}
