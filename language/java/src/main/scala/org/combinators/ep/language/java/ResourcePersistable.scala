package org.combinators.ep.language.java     /*DI:LD:AI*/

import java.nio.file.Path
import org.combinators.templating.persistable.{BundledResource, Persistable}

trait ResourcePersistableInstances {
  def bundledResourceInstance: ResourcePersistable.Aux = new Persistable {
    override type T = BundledResource
    override def path(elem: BundledResource): Path = elem.persistTo
    override def rawText(elem: BundledResource): Array[Byte] = {
      val contents:java.io.InputStream = elem.classToLoadResource.getResourceAsStream(elem.name)
      // TODO: This method is JDK9; consider replacing with JDK-8 equivalent?
      contents.readAllBytes()     // requires JDK 9 or higher

      // could be replaced with:
      //      contents.reset()
      //      val bytes:Array[Byte] = new Array[Byte](contents.available())
      //      new java.io.DataInputStream(contents).readFully(bytes)
      //      bytes

    }
  }
}

object ResourcePersistable extends ResourcePersistableInstances {
  type Aux = Persistable { type T = BundledResource }
  implicit def apply: Aux = bundledResourceInstance
}
