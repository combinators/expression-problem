package org.combinators.ep.language.java     /*DI:LD:AI*/

import com.github.javaparser.ast.`type`.TypeParameter
import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, ConstructorDeclaration, MethodDeclaration}
import org.combinators.ep.language.java.Syntax.default._

case class ProjectCtxt(
  resolver: ContextSpecificResolver,
  units: Seq[com.github.javaparser.ast.CompilationUnit],
  testUnits: Seq[com.github.javaparser.ast.CompilationUnit],
  extraDependencies: Seq[String]
)
case class CompilationUnitCtxt(
  resolver: ContextSpecificResolver,
  unit: com.github.javaparser.ast.CompilationUnit,
  isTest: Boolean
)
case class ClassCtxt(
  resolver: ContextSpecificResolver,
  cls: ClassOrInterfaceDeclaration,
  extraImports: Seq[Import]
)
case class TestCtxt(
  resolver: ContextSpecificResolver,
  extraImports: Seq[Import],
  testClass: ClassOrInterfaceDeclaration
)
case class MethodBodyCtxt(
  resolver: ContextSpecificResolver,
  extraImports: Seq[Import],
  method: MethodDeclaration
)
case class CtorCtxt(
  resolver: ContextSpecificResolver,
  extraImports: Seq[Import],
  ctor: ConstructorDeclaration
)
case class TypeParamCtxt(
  param: TypeParameter
)