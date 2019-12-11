package org.combinators.ep.language.java

import com.github.javaparser.ast.PackageDeclaration

sealed abstract class BoxLevel(val inMethods: Boolean, val inClasses: Boolean, val inConstructors: Boolean) {
  val isConsistent: Boolean =
    (inMethods == inClasses) && (inClasses == inConstructors)
}
case object FullyBoxed extends BoxLevel(inMethods = true, inClasses = true, inConstructors = true)
case object PartiallyBoxed extends BoxLevel(inMethods = true, inClasses = false, inConstructors = false)
case object Unboxed extends BoxLevel(inMethods = false, inClasses = false, inConstructors = false)

case class Config(
  targetPackage: PackageDeclaration,
  projectName: Option[String],
  boxLevel: BoxLevel
)