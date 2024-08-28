package org.combinators.ep.language.inbetween.functional   /*DI:LI:AI*/

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, FileWithPath, Understands, paradigm}
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.generator.paradigm.{AddImport, AddMethod, AddType, AddTypeConstructor, AddTypeLookup, FindMethod, FindType, InstantiateType, ResolveImport, ToTargetLanguageType, Functional => FP}
import org.combinators.ep.language.inbetween.any

trait FunctionalParadigm[FT <: FinalTypes, FactoryType <: Factory[FT]] extends FP {
  val base: AnyParadigm.WithFT[FT, FactoryType]

  import base.{FT => _, _}
  import syntax._

  lazy val factory: base.factory.type = base.factory
  type TypeContext = AlgebraicDataType[FT]

  val compilationUnitCapabilities: CompilationUnitCapabilities = new CompilationUnitCapabilities {
    implicit val canAddTypeInCompilationUnit: Understands[CompilationUnitContext, AddType[Name, TypeContext]] = new Understands[CompilationUnitContext, AddType[Name, TypeContext]] {
      def perform(context: CompilationUnit, command: AddType[Name, TypeContext]): (CompilationUnit, Unit) = {
        val converted = factory.convert(context)
        val emptyType = factory.adt(
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
      override def perform(context: any.CompilationUnit[FT], command: ResolveImport[any.Import[FT], any.Expression[FT]]): (any.CompilationUnit[FT], Option[any.Import[FT]]) = {
        (context, factory.convert(context).resolveImport(command.forElem).headOption)
      }
    }

    implicit val canResolveTypeImportInCompilationUnit: Understands[CompilationUnitContext, ResolveImport[Import, Type]] = new Understands[CompilationUnitContext, ResolveImport[Import, Type]] {
      override def perform(context: any.CompilationUnit[FT], command: ResolveImport[any.Import[FT], any.Type[FT]]): (any.CompilationUnit[FT], Option[any.Import[FT]]) = {
        (context, factory.convert(context).resolveImport(command.forElem).headOption)
      }
    }
  }

  val typeCapabilities: TypeCapabilities = new TypeCapabilities {
    implicit val canAddTypeConstructorInType: Understands[TypeContext, AddTypeConstructor[Name, Type]] = new Understands[TypeContext, AddTypeConstructor[Name, Type]] {
      override def perform(context: AlgebraicDataType[FT], command: AddTypeConstructor[any.Name[FT], any.Type[FT]]): (AlgebraicDataType[FT], Unit) = {
        (context.copy(typeConstructors = context.typeConstructors :+ factory.typeConstructor(command.name, command.parameters)), ())
      }
    }
    implicit val canTranslateTypeInType: Understands[TypeContext, ToTargetLanguageType[Type]] = new Understands[TypeContext, ToTargetLanguageType[Type]] {
      override def perform(context: AlgebraicDataType[FT], command: ToTargetLanguageType[Type]): (AlgebraicDataType[FT], command.Result) = {
        Command.runGenerator(context.toTargetLanguageType(command.tpe), context)
      }
    }

    implicit val canAddImportInType: Understands[TypeContext, AddImport[Import]] = new Understands[TypeContext, AddImport[Import]] {
      override def perform(context: AlgebraicDataType[FT], command: AddImport[any.Import[FT]]): (AlgebraicDataType[FT], Unit) = {
        (context.copy(imports = context.imports :+ command.imp), ())
      }
    }

    implicit val canResolveTypeImportInType: Understands[TypeContext, ResolveImport[Import, Type]] = new Understands[TypeContext, ResolveImport[Import, Type]] {
      override def perform(context: AlgebraicDataType[FT], command: ResolveImport[any.Import[FT], any.Type[FT]]): (AlgebraicDataType[FT], Option[any.Import[FT]]) = {
        (context, context.resolveImport(command.forElem).headOption)
      }
    }

    implicit val canResolveExpressionImportInType: Understands[TypeContext, ResolveImport[Import, Expression]] = new Understands[TypeContext, ResolveImport[Import, Expression]] {
      override def perform(context: AlgebraicDataType[FT], command: ResolveImport[any.Import[FT], any.Expression[FT]]): (AlgebraicDataType[FT], Option[any.Import[FT]]) = {
        (context, context.resolveImport(command.forElem).headOption)
      }
    }

    implicit val canFindTypeInType: Understands[TypeContext, FindType[Name, Type]] = new Understands[TypeContext, FindType[Name, Type]] {
      override def perform(context: AlgebraicDataType[FT], command: FindType[any.Name[FT], any.Type[FT]]): (AlgebraicDataType[FT], any.Type[FT]) = {
        (context, context.findType(command.name))
      }
    }
  }

  val methodBodyCapabilities: MethodBodyCapabilities = new MethodBodyCapabilities {
    implicit val canInstantiateTypeInMethod: Understands[MethodBodyContext, InstantiateType[Type, Name, Expression]] = new Understands[MethodBodyContext, InstantiateType[Type, Name, Expression]] {
      override def perform(context: any.Method[FT], command: InstantiateType[any.Type[FT], any.Name[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
        (context, factory.typeInstantiationExpression(command.tpe, Seq(command.constructor), command.arguments))
      }
    }

    implicit val canResolveExpressionImportInMethod: Understands[MethodBodyContext, ResolveImport[Import, Expression]] = new Understands[MethodBodyContext, ResolveImport[Import, Expression]] {
      override def perform(context: any.Method[FT], command: ResolveImport[any.Import[FT], any.Expression[FT]]): (any.Method[FT], Option[any.Import[FT]]) = {
        (context, factory.convert(context).resolveImport(command.forElem).headOption)
      }
    }

    implicit val canFindMethodInMethod: Understands[MethodBodyContext, FindMethod[Name, Expression]] = new Understands[MethodBodyContext, FindMethod[Name, Expression]] {
      override def perform(context: any.Method[FT], command: FindMethod[any.Name[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
        (context, factory.convert(context).findMethod(command.name))
      }
    }

    implicit val canFindTypeInMethod: Understands[MethodBodyContext, FindType[Name, Type]] = new Understands[MethodBodyContext, FindType[Name, Type]] {
      override def perform(context: any.Method[FT], command: FindType[any.Name[FT], any.Type[FT]]): (any.Method[FT], any.Type[FT]) = {
        (context, factory.convert(context).findType(command.name))
      }
    }
  }

  val projectCapabilities: ProjectCapabilities = new ProjectCapabilities {
    implicit val canAddTypeLookupForTypesInProject: Understands[ProjectContext, AddTypeLookup[TypeContext, Type]] = new Understands[ProjectContext, AddTypeLookup[TypeContext, Type]] {
      override def perform(context: any.Project[FT], command: AddTypeLookup[AlgebraicDataType[FT], any.Type[FT]]): (any.Project[FT], Unit) = {
        (factory.convert(context).addTypeLookupsForAlgebraicDataTypes((tpeRep: TypeRep) => if (tpeRep == command.tpe) Some(command.lookup) else None), ())
      }
    }
  }
}

object FunctionalParadigm {
  type WithBase[FT <: FinalTypes, FactoryType <: Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]] = FunctionalParadigm[FT, FactoryType] { val base: B }
  def apply[FT <: FinalTypes, FactoryType <: Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]](_base: B): WithBase[FT, FactoryType, _base.type] = new FunctionalParadigm[FT, FactoryType] {
    val base: _base.type = _base
  }
}