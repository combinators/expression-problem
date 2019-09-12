package org.combinators.ep.generator.paradigm

import org.combinators.ep.domain.abstractions.{DataType, TypeRep}
import org.combinators.ep.generator.{AbstractSyntax, Command, Understands}
import org.combinators.ep.generator.Command.Generator

case class AddType[TypeContext](name: String, tpeGen: Generator[TypeContext, Unit]) extends Command {
  type Result = Unit
}

case class AddTypeConstructor[Type](name: String, parameters: Seq[(String, Type)]) extends Command {
  type Result = Unit
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

case class FindType[Type](name: String) extends Command {
  type Result = Type
}


trait Functional {
  val base: AnyParadigm
  import base._
  import syntax._

  type TypeContext

  trait CompilationUnitCapabilities {
    implicit val canAddTypeInCompilationUnit: Understands[CompilationUnitContext, AddType[TypeContext]]
    def addType(name: String, spec: Generator[TypeContext, Unit]): Generator[CompilationUnitContext, Unit] =
      AnyParadigm.capabilitiy(AddType[TypeContext](name, spec))

    implicit val canAddMethodInCompilationUnit: Understands[CompilationUnitContext, AddMethod[MethodBodyContext, Expression]]
    def addMethod(
        name: String,
        spec: Generator[MethodBodyContext, Expression],
        isPublic: Boolean = true
      ): Generator[CompilationUnitContext, Unit] =
      AnyParadigm.capabilitiy(AddMethod(name, spec, isPublic))

    implicit val canResolveExpressionImportInCompilationUnit: Understands[CompilationUnitContext, ResolveImport[Import, Expression]]
    def resolveExpressionImport(expr: Expression): Generator[CompilationUnitContext, Option[Import]] =
      AnyParadigm.capabilitiy(ResolveImport[Import, Expression](expr))

    implicit val canResolveTypeImportInCompilationUnit: Understands[CompilationUnitContext, ResolveImport[Import, Type]]
    def resolveTypeImport(tpe: Type): Generator[CompilationUnitContext, Option[Import]] =
      AnyParadigm.capabilitiy(ResolveImport[Import, Type](tpe))
  }
  val compilationUnitCapabilities: CompilationUnitCapabilities

  trait TypeCapabilities {
    implicit val canAddTypeConstructorInType: Understands[TypeContext, AddTypeConstructor[Type]]
    def addTypeConstructor(name: String, parameters: Seq[(String, Type)]): Generator[TypeContext, Unit] =
      AnyParadigm.capabilitiy(AddTypeConstructor(name, parameters))

    implicit val canTranslateTypeInType: Understands[TypeContext, ToTargetLanguageType[Type]]
    def toTargetLanguageType(tpe: TypeRep): Generator[TypeContext, Type] =
      AnyParadigm.capabilitiy(ToTargetLanguageType[Type](tpe))

    implicit val canAddImportInType: Understands[TypeContext, AddImport[Import]]
    def addImport(imp: Import): Generator[TypeContext, Unit] =
      AnyParadigm.capabilitiy(AddImport(imp))

    implicit val canResolveTypeImportInType: Understands[TypeContext, ResolveImport[Import, Type]]
    def resolveTypeImport(tpe: Type): Generator[TypeContext, Option[Import]] =
      AnyParadigm.capabilitiy(ResolveImport[Import, Type](tpe))

    implicit val canResolveExpressionImportInType: Understands[TypeContext, ResolveImport[Import, Expression]]
    def resolveExpressionImport(expr: Expression): Generator[TypeContext, Option[Import]] =
      AnyParadigm.capabilitiy(ResolveImport[Import, Expression](expr))

    implicit val canFindTypeInType: Understands[TypeContext, FindType[Type]]
    def findType(name: String): Generator[TypeContext, Type] =
      AnyParadigm.capabilitiy(FindType[Type](name))
  }
  val typeCapabilities: TypeCapabilities

  trait MethodBodyCapabilities {
    implicit val canInstantiateTypeInMethod: Understands[MethodBodyContext, InstantiateType[Type, Expression]]
    def instantiateType(
        tpe: Type,
        constructor: String,
        arguments: Seq[Expression]): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capabilitiy(InstantiateType(tpe, constructor, arguments))

    implicit val canResolveExpressionImportInMethod: Understands[MethodBodyContext, ResolveImport[Import, Expression]]
    def resolveExpressionImport(expr: Expression): Generator[MethodBodyContext, Option[Import]] =
      AnyParadigm.capabilitiy(ResolveImport[Import, Expression](expr))

    implicit val canFindMethodInMethod: Understands[MethodBodyContext, FindMethod[Expression]]
    def findMethod(name: String): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capabilitiy(FindMethod[Expression](name))

    implicit val canFindTypeInMethod: Understands[MethodBodyContext, FindType[Type]]
    def findType(name: String): Generator[MethodBodyContext, Type] =
      AnyParadigm.capabilitiy(FindType[Type](name))
  }
  val methodBodyCapabilities: MethodBodyCapabilities

  trait ProjectContextCapabilities {
    implicit val canAddTypeLookupForTypesInProject: Understands[ProjectContext, AddTypeLookup[TypeContext, Type]]
    def addTypeLookupForTypes(tpe: TypeRep, lookup: Generator[TypeContext, Type]): Generator[ProjectContext, Unit] =
      AnyParadigm.capabilitiy(AddTypeLookup[TypeContext, Type](tpe, lookup))
  }
  val projectContextCapabilities: ProjectContextCapabilities
}

object Functional {
  type WithBase[B <: AnyParadigm] = Functional { val base: B }
}