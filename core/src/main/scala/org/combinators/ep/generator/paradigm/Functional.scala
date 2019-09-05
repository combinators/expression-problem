package org.combinators.ep.generator.paradigm

import org.combinators.ep.generator.{AbstractSyntax, Command, Understands}
import org.combinators.ep.generator.Command.Generator

case class AddType[TypeContext](name: String, tpeGen: Generator[TypeContext, Unit]) extends Command {
  type Result = Unit
}

case class AddTypeConstructor[Type](name: String, parameters: Seq[(String, Type)]) extends Command {
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

case class FindMethod[Expression](name: String) extends Command {
  type Result = Expression
}


trait Functional {
  val base: AnyParadigm
  import base._
  import syntax._

  type TypeContext

  trait CompilationUnitCapabilities {
    implicit val canAddTypeInCompilationUnit: Understands[CompilationUnitContext, AddType[TypeContext]]
    implicit val canAddMethodInCompilationUnit: Understands[CompilationUnitContext, AddMethod[MethodBodyContext, Expression]]
    implicit val canResolveMethodImportInCompilationUnit: Understands[CompilationUnitContext, ResolveImport[Import, Expression]]
    implicit val canResolveTypeImportInCompilationUnit: Understands[CompilationUnitContext, ResolveImport[Import, Type]]
  }
  val compilationUnitCapabilities: CompilationUnitCapabilities

  trait TypeCapabilities {
    implicit val canAddTypeConstructorInType: Understands[TypeContext, AddTypeConstructor[Type]]
    implicit val canTranslateTypeInType: Understands[TypeContext, ToTargetLanguageType[Type]]
    implicit val canAddImportInType: Understands[TypeContext, AddImport[Import]]
    implicit val canResolveTypeImportInType: Understands[TypeContext, ResolveImport[Import, Type]]
    implicit val canResolveMethodImportInType: Understands[TypeContext, ResolveImport[Import, Expression]]
  }
  val typeCapabilities: TypeCapabilities

  trait MethodBodyCapabilities {
    implicit val canPatternMatchInMethod: Understands[MethodBodyContext, PatternMatch[MethodBodyContext, Expression]]
    implicit val canInstantiateTypeInMethod: Understands[MethodBodyContext, InstantiateType[Type, Expression]]
    implicit val canResolveMethodImportInMethod: Understands[MethodBodyContext, ResolveImport[Import, Expression]]
    implicit val canFindMethodInMethod: Understands[MethodBodyContext, FindMethod[Expression]]
  }
  val methodBodyCapabilities: MethodBodyCapabilities
}

object Functional {
  type WithBase[B <: AnyParadigm] = Functional { val base: B }
}