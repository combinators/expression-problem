package org.combinators.ep.language.scala

import scala.meta.Pkg

case class Config(
  targetPackage: Pkg,
  projectName: Option[String]
)