/*
 * Copyright 2017-2019 Jan Bessai
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.combinators.jgitserv

import java.nio.file.{FileSystems, Files, Path, Paths}

import org.combinators.templating.persistable.{BundledResource, Persistable}

trait ResourcePersistableInstances {
  def bundledResourceInstance: ResourcePersistable.Aux = new Persistable {
    override type T = BundledResource
    override def path(elem: BundledResource): Path = elem.persistTo
    override def rawText(elem: BundledResource): Array[Byte] = {
      val uri = elem.classToLoadResource.getResource(elem.name).toURI
      val env = new java.util.HashMap[String, String]()
      env.put("create", "true")
      val zipfs = FileSystems.newFileSystem(uri, env)
      val content = Files.readAllBytes(Paths.get(uri))
      zipfs.close()
      content
    }
  }
}

object ResourcePersistable extends ResourcePersistableInstances {
  type Aux = Persistable { type T = BundledResource }
  implicit def apply: Aux = bundledResourceInstance
}
