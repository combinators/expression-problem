package org.combinators.ep.generator.paradigm

import org.combinators.ep.generator.{AbstractSyntax, Command, Understands}
import org.combinators.ep.generator.Command.Generator

case class AddType[TypeContext](name: String, tpeGen: Generator[TypeContext, Unit]) extends Command {
  type Result = Unit
}

case class AddTypeConstructor[Type](name: String, parameters: Seq[Type]) extends Command {
  type Result = Unit
}

case class PatternMatch[MethodBodyContext, Expression](
    onValue: Expression,
    options: (String, Seq[Expression]) => Generator[MethodBodyContext, Expression]
  ) extends Command {
  type Result = Expression
}

case class InstantiateType[Type, Expression](
    tpe: Type,
    constructor: String,
    arguments: Seq[Expression]
  ) extends Command {
  type Result = Expression
}


trait Functional {
  val base: AnyParadigm
  import base._
  import syntax._

  type TypeContext

  implicit val canAddTypeInCompilationUnit: Understands[CompilationUnitContext, AddType[TypeContext]]
  implicit val canAddMethodInCompilationUnit: Understands[CompilationUnit, AddMethod[MethodBodyContext, Expression]]
  implicit val canResolveMethodImportInCompilationUnit: Understands[CompilationUnit, ResolveImport[Import, Expression]]
  implicit val canResolveTypeImportInCompilationUnit: Understands[CompilationUnit, ResolveImport[Import, Type]]

  implicit val canAddTypeConstructorInType: Understands[TypeContext, AddTypeConstructor[Type]]

  implicit val canPatternMatchInMethod: Understands[MethodBodyContext, PatternMatch[MethodBodyContext, Expression]]
  implicit val canInstantiateTypeInMethod: Understands[MethodBodyContext, InstantiateType[Type, Expression]]
}

object Functional {
  type WithBase[B <: AnyParadigm] = Functional { val base: B }
}