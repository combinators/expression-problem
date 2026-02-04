package org.combinators.ep.language.scala   /*DI:LD:AI*/

import scala.meta.Pkg

case class Config(
  targetPackage: Pkg,
  projectName: Option[String]
)