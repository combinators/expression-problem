package org.combinators.ep.language.inbetween.oo

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, FileWithPath, Understands, paradigm}
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.generator.paradigm.{AddBlockDefinitions, AddClass, AddConstructor, AddField, AddImplemented, AddImport, AddMethod, AddParent, AddTypeLookup, Apply, CastObject, Debug, FindClass, FreshName, GetArguments, GetConstructor, GetField, GetMember, InitializeField, InitializeParent, InstanceOfType, InstantiateObject, Reify, RemoveMethod, ResolveImport, SelfReference, SetAbstract, SetInterface, SetParameters, SetStatic, SuperReference, ToTargetLanguageType, ObjectOriented => OOP}
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.ffi.arithmetic.SubOp

class OOParadigm[FT <: FinalTypes, FactoryType <: Factory[FT]](val base: AnyParadigm[FT, FactoryType]) extends OOP {
  import base._
  import syntax._
  lazy val factory: FactoryType = base.factory
  type ClassContext = Class[FT]
  type MethodBodyContext = any.Method[FT]
  type ConstructorContext = Constructor[FT]
  type CompilationUnitContext = any.CompilationUnit[FT]
  val classCapabilities: ClassCapabilities = new ClassCapabilities {
    implicit val canDebugInClass: Understands[Class[FT], Debug] = new Understands[Class[FT], Debug] {
      def perform(context: Class[FT], command: Debug): (Class[FT], Unit) = {
        println(command.tag)
        (context, ())
      }
    }
    implicit val canAddTypeLookupForMethodsInClass: Understands[Class[FT], AddTypeLookup[MethodBodyContext, Type]] = new Understands[Class[FT], AddTypeLookup[MethodBodyContext, Type]] {
      def perform(context: Class[FT], command: AddTypeLookup[MethodBodyContext, Type]): (Class[FT], Unit) = {
        (context.addTypeLookupForMethods(command.tpe, command.lookup), ())
      }
    }
    implicit val canAddTypeLookupForClassesInClass: Understands[Class[FT], AddTypeLookup[Class[FT], Type]] = new Understands[Class[FT], AddTypeLookup[Class[FT], Type]] {
      def perform(context: Class[FT], command: AddTypeLookup[Class[FT], Type]): (Class[FT], Unit) = {
        val (updated, lookup) = Command.runGenerator(command.lookup, context)
        (updated.addTypeLookupForClasses(command.tpe, lookup), ())
      }
    }
    implicit val canAddTypeLookupForConstructorsInClass: Understands[Class[FT], AddTypeLookup[ConstructorContext, Type]] = new Understands[Class[FT], AddTypeLookup[ConstructorContext, Type]] {
      def perform(context: Class[FT], command: AddTypeLookup[ConstructorContext, Type]): (Class[FT], Unit) = {
        (context.addTypeLookupForConstructors(command.tpe, command.lookup), ())
      }
    }
    implicit val canAddParentInClass: Understands[Class[FT], AddParent[Type]] = new Understands[Class[FT], AddParent[Type]] {
      def perform(context: Class[FT], command: AddParent[Type]): (Class[FT], Unit) = {
        (context.copy(parents = context.parents :+ command.parentClass), ())
      }
    }
    implicit val canAddImplementedInClass: Understands[Class[FT], AddImplemented[Type]] = new Understands[Class[FT], AddImplemented[Type]] {
      def perform(context: Class[FT], command: AddImplemented[Type]): (Class[FT], Unit) = {
        (context.copy(parents = context.implemented :+ command.interface), ())
      }
    }
    implicit val canRemoveMethodFromClass: Understands[Class[FT], RemoveMethod[Type, Name]] = new Understands[Class[FT], RemoveMethod[Type, Name]] {
      override def perform(context: Class[FT], command: RemoveMethod[Type, Name]): (Class[FT], Unit) = {
        ??? // TODO: Remove me
      }
    }
    implicit val canAddFieldInClass: Understands[Class[FT], AddField[Name, Type, Expression]] = new Understands[Class[FT], AddField[Name, Type, Expression]] {
      def perform(context: Class[FT], command: AddField[Name, Type, Expression]): (Class[FT], Unit) = {
        (context.copy(fields = context.fields :+ factory.field(command.name, command.tpe, command.initializer)), ())
      }
    }
    implicit val canGetFieldInClass: Understands[Class[FT], GetField[Name, Expression]] = new Understands[Class[FT], GetField[Name, Expression]] {
      def perform(context: Class[FT], command: GetField[Name, Expression]): (Class[FT], Expression) = {
        (context, factory.memberAccessExpression(factory.selfReferenceExpression, context.fields.find(f => f.name == command.name).get.name))
      }
    }
    implicit val canAddMethodInClass: Understands[Class[FT], AddMethod[MethodBodyContext, Name, Option[Expression]]] = new Understands[Class[FT], AddMethod[MethodBodyContext, Name, Option[Expression]]] {
      def perform(context: Class[FT], command: AddMethod[MethodBodyContext, Name, Option[Expression]]): (Class[FT], Unit) = {
        val emptyMethod = factory.clsMethod(
          name = command.name,
          isPublic = command.isPublic,
          isOverride = command.isOverride
        )
        var (generatedMethod, result) = Command.runGenerator(command.spec, emptyMethod)
        if (result.isDefined) {
          generatedMethod = generatedMethod.copy(statements = generatedMethod.statements :+ factory.returnExpression(result.get))
        }
        (context.copy(methods = context.methods :+ generatedMethod), ())
      }
    }
    implicit val canAddConstructorInClass: Understands[Class[FT], AddConstructor[ConstructorContext]] = new Understands[Class[FT], AddConstructor[ConstructorContext]] {
      def perform(context: Class[FT], command: AddConstructor[Constructor[FT]]): (Class[FT], Unit) = {
        val emptyConstructor = factory.constructor(constructedType = Some(factory.classReferenceType(context.name)))
        val (generatedConstructor, ()) = Command.runGenerator(command.ctor, emptyConstructor)
        (context.copy(constructors = context.constructors :+ generatedConstructor), ())
      }
    }
    implicit val canAddImportInClass: Understands[Class[FT], AddImport[Import]] = new Understands[Class[FT], AddImport[Import]] {
      def perform(context: Class[FT], command: AddImport[Import]): (Class[FT], Unit) = {
        (context.copy(imports = (context.imports :+ command.imp).distinct), ())
      }
    }
    implicit val canResolveImportInClass: Understands[Class[FT], ResolveImport[Import, Type]] = new Understands[Class[FT], ResolveImport[Import, Type]] {
      def perform(context: Class[FT], command: ResolveImport[Import, Type]): (Class[FT], Option[Import]) = {
        (context, context.resolveImport(command.forElem))
      }
    }
    implicit val canSetAbstractInClass: Understands[Class[FT], SetAbstract] = new Understands[Class[FT], SetAbstract] {
      def perform(context: Class[FT], command: SetAbstract): (Class[FT], Unit) = {
        (context.copy(isAbstract = true), ())
      }
    }
    implicit val canSetStaticInClass: Understands[Class[FT], SetStatic] = new Understands[Class[FT], SetStatic] {
      def perform(context: Class[FT], command: SetStatic): (Class[FT], Unit) = {
        (context.copy(isStatic = true), ())
      }
    }
    implicit val canSetInterfaceInClass: Understands[Class[FT], SetInterface] = new Understands[Class[FT], SetInterface] {
      def perform(context: Class[FT], command: SetInterface): (Class[FT], Unit) = {
        (context.copy(isInterface = true), ())
      }
    }
    implicit val canTranslateTypeInClass: Understands[Class[FT], ToTargetLanguageType[Type]] = new Understands[Class[FT], ToTargetLanguageType[Type]] {
      def perform(context: Class[FT], command: ToTargetLanguageType[Type]): (Class[FT], Type) = {
        (context, context.typeLookupMap(command.tpe))
      }
    }
    implicit val canSelfReferenceInClass: Understands[Class[FT], SelfReference[Expression]] = new Understands[Class[FT], SelfReference[Expression]] {
      def perform(context: Class[FT], command: SelfReference[Expression]): (Class[FT], Expression) = {
        (context, factory.selfReferenceExpression)
      }
    }
    implicit val canFindClassInClass: Understands[Class[FT], FindClass[Name, Type]] = new Understands[Class[FT], FindClass[Name, Type]] {
      def perform(context: Class[FT], command: FindClass[Name, Type]): (Class[FT], Type) = {
        (context, context.findClass(command.qualifiedName:_*))
      }
    }
    implicit val canGetFreshNameInClass: Understands[Class[FT], FreshName[Name]] = new Understands[Class[FT], FreshName[Name]] {
      def perform(context: Class[FT], command: FreshName[Name]): (Class[FT], Name) = {
        (context, context.getFreshName(command.basedOn))
      }
    }
  }
  val constructorCapabilities: ConstructorCapabilities = new ConstructorCapabilities {
    implicit val canInitializeParentInConstructor: Understands[Constructor[FT], InitializeParent[Type, Expression]] = new Understands[Constructor[FT], InitializeParent[Type, Expression]] {
      def perform(context: Constructor[FT], command: InitializeParent[Type, Expression]): (Constructor[FT], Unit) = {
        (context.copyAsConstructor(superInitialization = Some((command.parent, command.arguments))), ())
      }
    }
    implicit val canCastInConstructor: Understands[Constructor[FT], CastObject[Type, Expression]] = new Understands[Constructor[FT], CastObject[Type, Expression]] {
      def perform(context: Constructor[FT], command: CastObject[Type, Expression]): (Constructor[FT], Expression) = {
        (context, factory.castExpression(command.tpe, command.expr))
      }
    }
    implicit val canInitializeFieldInConstructor: Understands[Constructor[FT], InitializeField[Name, Expression]] = new Understands[Constructor[FT], InitializeField[Name, Expression]] {
      def perform(context: Constructor[FT], command: InitializeField[Name, Expression]): (Constructor[FT], Unit) = {
        (context.copyAsConstructor(fieldInitializers = context.fieldInitializers :+ (command.name, command.value)), ())
      }
    }
    implicit val canAddBlockDefinitionsInConstructor: Understands[Constructor[FT], AddBlockDefinitions[Statement]] = new Understands[Constructor[FT], AddBlockDefinitions[Statement]] {
      def perform(context: Constructor[FT], command: AddBlockDefinitions[Statement]): (Constructor[FT], Unit) = {
        (context.copyAsConstructor(statements = context.statements ++ command.definitions), ())
      }
    }
    implicit val canAddImportInConstructor: Understands[Constructor[FT], AddImport[Import]] = new Understands[Constructor[FT], AddImport[Import]] {
      def perform(context: Constructor[FT], command: AddImport[Import]): (Constructor[FT], Unit) = {
        (context.copyAsConstructor(imports = context.imports + command.imp), ())
      }
    }
    implicit val canResolveImportInConstructor: Understands[Constructor[FT], ResolveImport[Import, Type]] = new Understands[Constructor[FT], ResolveImport[Import, Type]] {
      def perform(context: Constructor[FT], command: ResolveImport[Import, Type]): (Constructor[FT], Option[Import]) = {
        (context, context.resolveImport(command.forElem))
      }
    }
    implicit val canInstantiateObjectInConstructor: Understands[Constructor[FT], InstantiateObject[Type, Expression, Class[FT]]] = new Understands[Constructor[FT], InstantiateObject[Type, Expression, Class[FT]]] {
      def perform(context: Constructor[FT], command: InstantiateObject[Type, Expression, Class[FT]]): (Constructor[FT], Expression) = {
        (context, factory.objectInstantiationExpression(command.tpe, command.constructorArguments, None)) // TODO: add anonymous inner class declarations
      }
    }
    implicit val canApplyInConstructor: Understands[Constructor[FT], Apply[Expression, Expression, Expression]] = new Understands[Constructor[FT], Apply[Expression, Expression, Expression]] {
      def perform(context: Constructor[FT], command: Apply[Expression, Expression, Expression]): (Constructor[FT], Expression) = {
        (context, factory.applyExpression(command.functional, command.arguments))
      }
    }
    implicit val canGetMemberInConstructor: Understands[Constructor[FT], GetMember[Expression, Name]] = new Understands[Constructor[FT], GetMember[Expression, Name]] {
      def perform(context: Constructor[FT], command: GetMember[Expression, Name]): (Constructor[FT], Expression) = {
        (context, factory.memberAccessExpression(command.instance, command.member))
      }
    }
    implicit val canSelfReferenceInConstructor: Understands[Constructor[FT], SelfReference[Expression]] = new Understands[Constructor[FT], SelfReference[Expression]] {
      def perform(context: Constructor[FT], command: SelfReference[Expression]): (Constructor[FT], Expression) = {
        (context, factory.selfReferenceExpression)
      }
    }
    implicit val canGetArgumentsInConstructor: Understands[Constructor[FT], GetArguments[Type, Name, Expression]] = new Understands[Constructor[FT], GetArguments[Type, Name, Expression]] {
      def perform(context: Constructor[FT], command: GetArguments[Type, Name, Expression]): (Constructor[FT], Seq[(Name, Type, Expression)]) = {
        (context, context.parameters.map(param => (param._1, param._2, factory.argumentExpression(param._1))))
      }
    }
    implicit val canTranslateTypeInConstructor: Understands[Constructor[FT], ToTargetLanguageType[Type]] = new Understands[Constructor[FT], ToTargetLanguageType[Type]] {
      def perform(context: Constructor[FT], command: ToTargetLanguageType[Type]): (Constructor[FT], Type) = {
        (context, context.toTargetLanguageType(command.tpe))
      }
    }
    implicit def canReifyInConstructor[T]: Understands[Constructor[FT], Reify[T, Expression]] = new Understands[Constructor[FT], Reify[T, Expression]] {
      def perform(context: Constructor[FT], command: Reify[T, Expression]): (Constructor[FT], Expression) = {
        (context, context.reify(command.tpe, command.value))
      }
    }
    implicit val canSetParametersInConstructor: Understands[Constructor[FT], SetParameters[Name, Type]] = new Understands[Constructor[FT], SetParameters[Name, Type]] {
      def perform(context: Constructor[FT], command: SetParameters[Name, Type]): (Constructor[FT], Unit) = {
        (context.copyAsConstructor(parameters = command.params), ())
      }
    }
    implicit val canGetConstructorInConstructor: Understands[Constructor[FT], GetConstructor[Type, Expression]] = new Understands[Constructor[FT], GetConstructor[Type, Expression]] {
      def perform(context: Constructor[FT], command: GetConstructor[Type, Expression]): (Constructor[FT], Expression) = {
        ???
      }
    }
    implicit val canFindClassInConstructor: Understands[Constructor[FT], FindClass[Name, Type]] = new Understands[Constructor[FT], FindClass[Name, Type]] {
      def perform(context: Constructor[FT], command: FindClass[Name, Type]): (Constructor[FT], Type) = {
        (context, context.findClass(command.qualifiedName: _*))
      }
    }
    implicit val canGetFreshNameInConstructor: Understands[Constructor[FT], FreshName[Name]] = new Understands[Constructor[FT], FreshName[Name]] {
      def perform(context: Constructor[FT], command: FreshName[Name]): (Constructor[FT], Name) = {
        (context, context.getFreshName(command.basedOn))
      }
    }
  }
  val methodBodyCapabilities: MethodBodyCapabilities = new MethodBodyCapabilities {
    implicit val canInstantiateObjectInMethod: Understands[MethodBodyContext, InstantiateObject[Type, Expression, ClassContext]] = new Understands[MethodBodyContext, InstantiateObject[Type, Expression, Class[FT]]] {
      def perform(context: any.Method[FT], command: InstantiateObject[Type, Expression, Class[FT]]): (any.Method[FT], Expression) = {
        (context, factory.objectInstantiationExpression(command.tpe, command.constructorArguments, None)) // TODO: add anonymous inner class declarations
      }
    }
    implicit val canGetMemberInMethod: Understands[any.Method[FT], GetMember[Expression, Name]] = new Understands[any.Method[FT], GetMember[Expression, Name]] {
      def perform(context: any.Method[FT], command: GetMember[Expression, Name]): (any.Method[FT], Expression) = {
        (context, factory.memberAccessExpression(command.instance, command.member))
      }
    }
    implicit val canCastInMethod: Understands[any.Method[FT], CastObject[Type, Expression]] = new Understands[any.Method[FT], CastObject[Type, Expression]] {
      def perform(context: any.Method[FT], command: CastObject[Type, Expression]): (any.Method[FT], Expression) = {
        (context, factory.castExpression(command.tpe, command.expr))
      }
    }
    implicit val canInstanceOfTypeInMethod: Understands[any.Method[FT], InstanceOfType[Type, Expression]] = new Understands[any.Method[FT], InstanceOfType[Type, Expression]] {
      def perform(context: any.Method[FT], command: InstanceOfType[Type, Expression]): (any.Method[FT], Expression) = {
        (context, factory.instanceOfExpression(command.tpe, command.expr))
      }
    }
    implicit val canSetAbstractInMethod: Understands[any.Method[FT], SetAbstract] = new Understands[any.Method[FT], SetAbstract] {
      def perform(context: any.Method[FT], command: SetAbstract): (any.Method[FT], Unit) = {
        (factory.convert(context).copyAsClsMethod(isAbstract = true), ())
      }
    }
    implicit val canSetStaticInMethod: Understands[any.Method[FT], SetStatic] = new Understands[any.Method[FT], SetStatic] {
      def perform(context: any.Method[FT], command: SetStatic): (any.Method[FT], Unit) = {
        (factory.convert(context).copyAsClsMethod(isStatic = true), ())
      }
    }
    implicit val canSelfReferenceInMethod: Understands[any.Method[FT], SelfReference[Expression]] = new Understands[any.Method[FT], SelfReference[Expression]] {
      def perform(context: any.Method[FT], command: SelfReference[Expression]): (any.Method[FT], Expression) = {
        (context, factory.selfReferenceExpression)
      }
    }
    implicit val canSuperReferenceInMethod: Understands[any.Method[FT], SuperReference[Name, Expression]] = new Understands[any.Method[FT], SuperReference[Name, Expression]] {
      def perform(context: any.Method[FT], command: SuperReference[Name, Expression]): (any.Method[FT], Expression) = {
        (context, factory.superReferenceExpression(factory.convert(context).findClass(command.qualifiedName: _*)))
      }
    }
    implicit val canGetConstructorInMethod: Understands[any.Method[FT], GetConstructor[Type, Expression]] = new Understands[any.Method[FT], GetConstructor[Type, Expression]] {
      def perform(context: any.Method[FT], command: GetConstructor[Type, Expression]): (any.Method[FT], Expression) = {
        ???
      }
    }
    implicit val canFindClassInMethod: Understands[any.Method[FT], FindClass[Name, Type]] = new Understands[any.Method[FT], FindClass[Name, Type]] {
      def perform(context: any.Method[FT], command: FindClass[Name, Type]): (any.Method[FT], Type) = {
        (context, factory.convert(context).findClass(command.qualifiedName: _*))
      }
    }
  }
  val compilationUnitCapabilities: CompilationUnitCapabilities = new CompilationUnitCapabilities {
    implicit val canAddClassInCompilationUnit: Understands[CompilationUnit, AddClass[Class[FT], Name]] = new Understands[CompilationUnit, AddClass[Class[FT], Name]] {
      def perform(context: CompilationUnit, command: AddClass[Class[FT], Name]): (CompilationUnit, Unit) = {
        val converted = factory.convert(context)
        val emptyCls = factory.cls(name = command.name)
        var (generatedClass, result) = Command.runGenerator(command.cls, emptyCls)
        (converted.copyAsCompilationUnitWithClasses(classes = converted.classes :+ generatedClass), ())
      }
    }
  }
  val projectCapabilities: ProjectCapabilities = new ProjectCapabilities {
    implicit val canAddTypeLookupForClassesInProject: Understands[ProjectContext, AddTypeLookup[Class[FT], Type]] = new Understands[ProjectContext, AddTypeLookup[Class[FT], Type]] {
      def perform(context: ProjectContext, command: AddTypeLookup[Class[FT], Type]): (ProjectContext, Unit) = {
        val converted = factory.convert(context)
        (converted.addTypeLookupForClasses(command.tpe, command.lookup), ())
      }
    }
    implicit val canAddTypeLookupForConstructorsInProject: Understands[ProjectContext, AddTypeLookup[Constructor[FT], Type]] = new Understands[ProjectContext, AddTypeLookup[Constructor[FT], Type]] {
      def perform(context: ProjectContext, command: AddTypeLookup[Constructor[FT], Type]): (ProjectContext, Unit) = {
        val converted = factory.convert(context)
        (converted.addTypeLookupForConstructors(command.tpe, command.lookup), ())
      }
    }
  }
  val testCapabilities: TestCapabilities = new TestCapabilities {
    implicit val canAddMethodInTest: Understands[TestContext, AddMethod[MethodBodyContext, Name, Option[Expression]]] = new Understands[TestContext, AddMethod[MethodBodyContext, Name, Option[Expression]]] {
        def perform(context: TestContext, command: AddMethod[MethodBodyContext, Name, Option[Expression]]): (TestContext, Unit) = {
          (context, ()) // TODO
        }
      }
    implicit val canAddBlockDefinitionsInTest: Understands[TestContext, AddBlockDefinitions[Statement]] = new Understands[TestContext, AddBlockDefinitions[Statement]] {
      def perform(context: TestContext, command: AddBlockDefinitions[Statement]): (TestContext, Unit) = {
        (context, ())
      }
    }
    implicit val canAddFieldInTest: Understands[TestContext, AddField[Name, Type, Expression]] = new Understands[TestContext, AddField[Name, Type, Expression]] {
      def perform(context: TestContext, command: AddField[Name, Type, Expression]): (TestContext, Unit) = {
        (context, ()) // TODO
      }
    }
    implicit val canInitializeFieldInTest: Understands[TestContext, InitializeField[Name, Expression]] = new Understands[TestContext, InitializeField[Name, Expression]] {
      def perform(context: TestContext, command: InitializeField[Name, Expression]): (TestContext, Unit) = {
        (context, ()) // TODO
      }
    }
    implicit val canInstantiateObjectInTest: Understands[TestContext, InstantiateObject[Type, Expression, TestContext]] = new Understands[TestContext, InstantiateObject[Type, Expression, TestContext]] {
      def perform(context: TestContext, command: InstantiateObject[Type, Expression, TestContext]): (TestContext, Expression) = {
        (context, command.constructorArguments(0)) // TODO
      }
    }
    implicit val canAddConstructorInTest: Understands[TestContext, AddConstructor[Constructor[FT]]] = new Understands[TestContext, AddConstructor[Constructor[FT]]] {
      def perform(context: TestContext, command: AddConstructor[Constructor[FT]]): (TestContext, Unit) = {
        (context, ()) // TODO
      }
    }
    implicit val canAddImportInTest: Understands[TestContext, AddImport[Import]] = new Understands[TestContext, AddImport[Import]] {
      def perform(context: TestContext, command: AddImport[Import]): (TestContext, Unit) = {
        (context, ()) // TODO
      }
    }
    implicit val canResolveImportInTest: Understands[TestContext, ResolveImport[Import, Type]] = new Understands[TestContext, ResolveImport[Import, Type]] {
      def perform(context: TestContext, command: ResolveImport[Import, Type]): (TestContext, Option[Import]) = {
        (context, None) // TODO
      }
    }
    implicit val canFindClassInTest: Understands[TestContext, FindClass[Name, Type]] = new Understands[TestContext, FindClass[Name, Type]] {
      def perform(context: TestContext, command: FindClass[Name, Type]): (TestContext, Type) = {
        (context, factory.classReferenceType(command.qualifiedName:_*)) // TODO
      }
    }
    implicit val canAddImplementedInTest: Understands[TestContext, AddImplemented[Type]] = new Understands[TestContext, AddImplemented[Type]] {
      def perform(context: TestContext, command: AddImplemented[Type]): (TestContext, Unit) = {
        (context, ()) // TODO
      }
    }
  }
}
