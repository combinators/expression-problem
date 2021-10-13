package org.combinators.ep.language.java     /*DI:LD:AI*/

import java.nio.file.{FileSystems, Files, Path, Paths}

import org.combinators.templating.persistable.{BundledResource, Persistable}

trait ResourcePersistableInstances {
  def bundledResourceInstance: ResourcePersistable.Aux = new Persistable {
    override type T = BundledResource
    override def path(elem: BundledResource): Path = elem.persistTo
    override def rawText(elem: BundledResource): Array[Byte] = {
      val contents = elem.classToLoadResource.getResourceAsStream(elem.name)
      contents.readAllBytes()
    }
  }
}

object ResourcePersistable extends ResourcePersistableInstances {
  type Aux = Persistable { type T = BundledResource }
  implicit def apply: Aux = bundledResourceInstance
}
