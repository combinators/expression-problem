package org.combinators.ep.language.inbetween.functional   /*DI:LI:AI*/

import org.combinators.cogen.TypeRep
import org.combinators.cogen.paradigm.{AddImport, AddMethod, AddType, AddTypeConstructor, AddTypeLookup, FindMethod, FindType, InstantiateType, ResolveImport, ToTargetLanguageType, Functional as FP}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, FileWithPath, Understands, paradigm}
import org.combinators.ep.language.inbetween.any.{AnyParadigm, AnyParadigm2}
import org.combinators.ep.language.inbetween.any

trait FunctionalParadigm2(val base: AnyParadigm2.WithAST[FunctionalAST]) extends FP {
  import base.ast.any
  import base.ast.factory
  import base.ast.functionalFactory
  import base.ast.functional.*
  import base.ast.any.*
  import base.ProjectContext
  import base.CompilationUnitContext
  import base.MethodBodyContext
  
  type TypeContext = AlgebraicDataType

  val compilationUnitCapabilities: CompilationUnitCapabilities = new CompilationUnitCapabilities {
    implicit val canAddTypeInCompilationUnit: Understands[CompilationUnitContext, AddType[Name, TypeContext]] = new Understands[CompilationUnitContext, AddType[Name, TypeContext]] {
      def perform(context: CompilationUnit, command: AddType[Name, TypeContext]): (CompilationUnit, Unit) = {
        val converted = factory.convert(context)
        val emptyType = functionalFactory.adt(
          name = command.name,
          typeLookupMap = converted.adtTypeLookupMap)
        var (generatedType, result) = Command.runGenerator(command.tpeGen, emptyType)
        (converted.copyAsFunctionalCompilationUnit(adts = converted.adts :+ generatedType), ())
      }
    }

    implicit val canAddMethodInCompilationUnit: Understands[CompilationUnitContext, AddMethod[MethodBodyContext, Name, Expression]] = new Understands[CompilationUnitContext, AddMethod[MethodBodyContext, Name, Expression]] {
      def perform(context: CompilationUnit, command: AddMethod[MethodBodyContext, Name, Expression]): (CompilationUnit, Unit) = {
        val converted = factory.convert(context)
        val emptyMethod = factory.method(
          name = command.name,
          typeLookupMap = converted.functionTypeLookupMap)
        val (generatedMethod, result) = Command.runGenerator(command.spec, emptyMethod)
        val methodWithResult = generatedMethod.copy(statements = generatedMethod.statements :+ factory.returnExpression(result))
        (converted.copyAsFunctionalCompilationUnit(functions = converted.functions :+ methodWithResult), ())
      }
    }

    implicit val canResolveExpressionImportInCompilationUnit: Understands[CompilationUnitContext, ResolveImport[Import, Expression]] = new Understands[CompilationUnitContext, ResolveImport[Import, Expression]] {
      override def perform(context: any.CompilationUnit, command: ResolveImport[any.Import, any.Expression]): (any.CompilationUnit, Option[any.Import]) = {
        (context, factory.convert(context).resolveImport(command.forElem).headOption)
      }
    }

    implicit val canResolveTypeImportInCompilationUnit: Understands[CompilationUnitContext, ResolveImport[Import, Type]] = new Understands[CompilationUnitContext, ResolveImport[Import, Type]] {
      override def perform(context: any.CompilationUnit, command: ResolveImport[any.Import, any.Type]): (any.CompilationUnit, Option[any.Import]) = {
        (context, factory.convert(context).resolveImport(command.forElem).headOption)
      }
    }
  }

  val typeCapabilities: TypeCapabilities = new TypeCapabilities {
    implicit val canAddTypeConstructorInType: Understands[TypeContext, AddTypeConstructor[Name, Type]] = new Understands[TypeContext, AddTypeConstructor[Name, Type]] {
      override def perform(context: AlgebraicDataType, command: AddTypeConstructor[any.Name, any.Type]): (AlgebraicDataType, Unit) = {
        (context.copy(typeConstructors = context.typeConstructors :+ functionalFactory.typeConstructor(command.name, command.parameters)), ())
      }
    }
    implicit val canTranslateTypeInType: Understands[TypeContext, ToTargetLanguageType[Type]] = new Understands[TypeContext, ToTargetLanguageType[Type]] {
      override def perform(context: AlgebraicDataType, command: ToTargetLanguageType[Type]): (AlgebraicDataType, command.Result) = {
        Command.runGenerator(context.toTargetLanguageType(command.tpe), context)
      }
    }

    implicit val canAddImportInType: Understands[TypeContext, AddImport[Import]] = new Understands[TypeContext, AddImport[Import]] {
      override def perform(context: AlgebraicDataType, command: AddImport[any.Import]): (AlgebraicDataType, Unit) = {
        (context.copy(imports = context.imports :+ command.imp), ())
      }
    }

    implicit val canResolveTypeImportInType: Understands[TypeContext, ResolveImport[Import, Type]] = new Understands[TypeContext, ResolveImport[Import, Type]] {
      override def perform(context: AlgebraicDataType, command: ResolveImport[any.Import, any.Type]): (AlgebraicDataType, Option[any.Import]) = {
        (context, context.resolveImport(command.forElem).headOption)
      }
    }

    implicit val canResolveExpressionImportInType: Understands[TypeContext, ResolveImport[Import, Expression]] = new Understands[TypeContext, ResolveImport[Import, Expression]] {
      override def perform(context: AlgebraicDataType, command: ResolveImport[any.Import, any.Expression]): (AlgebraicDataType, Option[any.Import]) = {
        (context, context.resolveImport(command.forElem).headOption)
      }
    }

    implicit val canFindTypeInType: Understands[TypeContext, FindType[Name, Type]] = new Understands[TypeContext, FindType[Name, Type]] {
      override def perform(context: AlgebraicDataType, command: FindType[any.Name, any.Type]): (AlgebraicDataType, any.Type) = {
        (context, context.findType(command.name))
      }
    }
  }

  val methodBodyCapabilities: MethodBodyCapabilities = new MethodBodyCapabilities {
    implicit val canInstantiateTypeInMethod: Understands[MethodBodyContext, InstantiateType[Type, Name, Expression]] = new Understands[MethodBodyContext, InstantiateType[Type, Name, Expression]] {
      override def perform(context: any.Method, command: InstantiateType[any.Type, any.Name, any.Expression]): (any.Method, any.Expression) = {
        (context, functionalFactory.typeInstantiationExpression(command.tpe, Seq(command.constructor), command.arguments))
      }
    }

    implicit val canResolveExpressionImportInMethod: Understands[MethodBodyContext, ResolveImport[Import, Expression]] = new Understands[MethodBodyContext, ResolveImport[Import, Expression]] {
      override def perform(context: any.Method, command: ResolveImport[any.Import, any.Expression]): (any.Method, Option[any.Import]) = {
        (context, factory.convert(context).resolveImport(command.forElem).headOption)
      }
    }

    implicit val canFindMethodInMethod: Understands[MethodBodyContext, FindMethod[Name, Expression]] = new Understands[MethodBodyContext, FindMethod[Name, Expression]] {
      override def perform(context: any.Method, command: FindMethod[any.Name, any.Expression]): (any.Method, any.Expression) = {
        (context, factory.convert(context).findMethod(command.name))
      }
    }

    implicit val canFindTypeInMethod: Understands[MethodBodyContext, FindType[Name, Type]] = new Understands[MethodBodyContext, FindType[Name, Type]] {
      override def perform(context: any.Method, command: FindType[any.Name, any.Type]): (any.Method, any.Type) = {
        (context, factory.convert(context).findType(command.name))
      }
    }
  }

  val projectCapabilities: ProjectCapabilities = new ProjectCapabilities {
    implicit val canAddTypeLookupForTypesInProject: Understands[ProjectContext, AddTypeLookup[TypeContext, Type]] = new Understands[ProjectContext, AddTypeLookup[TypeContext, Type]] {
      override def perform(context: any.Project, command: AddTypeLookup[AlgebraicDataType, any.Type]): (any.Project, Unit) = {
        (factory.convert(context).addTypeLookupsForAlgebraicDataTypes((tpeRep: TypeRep) => if (tpeRep == command.tpe) Some(command.lookup) else None), ())
      }
    }
  }
}

object FunctionalParadigm2 {
  type WithBase[AST <: FunctionalAST, B <: AnyParadigm2.WithAST[AST]] = FunctionalParadigm2 {val base: B}
  trait WB[AST <: FunctionalAST, B <: AnyParadigm2.WithAST[AST]](override val base: B) extends FunctionalParadigm2 {}

  def apply[AST <: FunctionalAST, B <: AnyParadigm2.WithAST[AST]](_base: B): WithBase[AST, _base.type] = new WB[AST, _base.type](_base) with FunctionalParadigm2(_base) {}
}