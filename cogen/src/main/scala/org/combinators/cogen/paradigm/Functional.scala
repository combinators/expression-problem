package org.combinators.cogen.paradigm

import org.combinators.cogen.{TypeRep, Command, Understands}
import Command.Generator

case class AddType[Name, TypeContext](name: Name, tpeGen: Generator[TypeContext, Unit]) extends Command {
  type Result = Unit
}

case class AddTypeConstructor[Name, Type](name: Name, parameters: Seq[(Name, Type)]) extends Command {
  type Result = Unit
}

case class InstantiateType[Type, Name, Expression](
    tpe: Type,
    constructor: Name,
    arguments: Seq[Expression]
  ) extends Command {
  type Result = Expression
}

case class FindMethod[Name, Expression](name: Seq[Name]) extends Command {
  type Result = Expression
}

case class FindType[Name, Type](name: Seq[Name]) extends Command {
  type Result = Type
}


trait Functional {
  val base: AnyParadigm
  import base._
  import syntax._

  type TypeContext

  trait CompilationUnitCapabilities {
    implicit val canAddTypeInCompilationUnit: Understands[CompilationUnitContext, AddType[Name, TypeContext]]
    def addType(name: Name, spec: Generator[TypeContext, Unit]): Generator[CompilationUnitContext, Unit] =
      AnyParadigm.capability(AddType[Name, TypeContext](name, spec))

    implicit val canAddMethodInCompilationUnit: Understands[CompilationUnitContext, AddMethod[MethodBodyContext, Name, Expression]]
    def addMethod(
        name: Name,
        spec: Generator[MethodBodyContext, Expression],
        isPublic: Boolean = true
      ): Generator[CompilationUnitContext, Unit] =
      AnyParadigm.capability(AddMethod(name, spec, isPublic))

    implicit val canResolveExpressionImportInCompilationUnit: Understands[CompilationUnitContext, ResolveImport[Import, Expression]]
    def resolveExpressionImport(expr: Expression): Generator[CompilationUnitContext, Option[Import]] =
      AnyParadigm.capability(ResolveImport[Import, Expression](expr))

    implicit val canResolveTypeImportInCompilationUnit: Understands[CompilationUnitContext, ResolveImport[Import, Type]]
    def resolveTypeImport(tpe: Type): Generator[CompilationUnitContext, Option[Import]] =
      AnyParadigm.capability(ResolveImport[Import, Type](tpe))
  }
  val compilationUnitCapabilities: CompilationUnitCapabilities

  trait TypeCapabilities {
    implicit val canAddTypeConstructorInType: Understands[TypeContext, AddTypeConstructor[Name, Type]]
    def addTypeConstructor(name: Name, parameters: Seq[(Name, Type)]): Generator[TypeContext, Unit] =
      AnyParadigm.capability(AddTypeConstructor(name, parameters))

    implicit val canTranslateTypeInType: Understands[TypeContext, ToTargetLanguageType[Type]]
    def toTargetLanguageType(tpe: TypeRep): Generator[TypeContext, Type] =
      AnyParadigm.capability(ToTargetLanguageType[Type](tpe))

    implicit val canAddImportInType: Understands[TypeContext, AddImport[Import]]
    def addImport(imp: Import): Generator[TypeContext, Unit] =
      AnyParadigm.capability(AddImport(imp))

    implicit val canResolveTypeImportInType: Understands[TypeContext, ResolveImport[Import, Type]]
    def resolveTypeImport(tpe: Type): Generator[TypeContext, Option[Import]] =
      AnyParadigm.capability(ResolveImport[Import, Type](tpe))

    implicit val canResolveExpressionImportInType: Understands[TypeContext, ResolveImport[Import, Expression]]
    def resolveExpressionImport(expr: Expression): Generator[TypeContext, Option[Import]] =
      AnyParadigm.capability(ResolveImport[Import, Expression](expr))

    implicit val canFindTypeInType: Understands[TypeContext, FindType[Name, Type]]
    def findType(name: Seq[Name]): Generator[TypeContext, Type] =
      AnyParadigm.capability(FindType[Name, Type](name))
  }
  val typeCapabilities: TypeCapabilities

  trait MethodBodyCapabilities {
    implicit val canInstantiateTypeInMethod: Understands[MethodBodyContext, InstantiateType[Type, Name, Expression]]
    def instantiateType(
        tpe: Type,
        constructor: Name,
        arguments: Seq[Expression]): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capability(InstantiateType(tpe, constructor, arguments))

    implicit val canResolveExpressionImportInMethod: Understands[MethodBodyContext, ResolveImport[Import, Expression]]
    def resolveExpressionImport(expr: Expression): Generator[MethodBodyContext, Option[Import]] =
      AnyParadigm.capability(ResolveImport[Import, Expression](expr))

    implicit val canFindMethodInMethod: Understands[MethodBodyContext, FindMethod[Name, Expression]]
    def findMethod(name: Seq[Name]): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capability(FindMethod[Name, Expression](name))

    implicit val canFindTypeInMethod: Understands[MethodBodyContext, FindType[Name, Type]]
    def findType(name: Seq[Name]): Generator[MethodBodyContext, Type] =
      AnyParadigm.capability(FindType[Name, Type](name))
  }
  val methodBodyCapabilities: MethodBodyCapabilities

  trait ProjectCapabilities {
    implicit val canAddTypeLookupForTypesInProject: Understands[ProjectContext, AddTypeLookup[TypeContext, Type]]
    def addTypeLookupForTypes(tpe: TypeRep, lookup: Generator[TypeContext, Type]): Generator[ProjectContext, Unit] =
      AnyParadigm.capability(AddTypeLookup[TypeContext, Type](tpe, lookup))
  }
  val projectCapabilities: ProjectCapabilities
}

object Functional {
  type WithBase[B <: AnyParadigm] = Functional { val base: B }
}