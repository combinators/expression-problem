package org.combinators.ep.language.inbetween.oo   /*DI:LI:AI*/

import org.combinators.cogen.TypeRep
import org.combinators.cogen.paradigm.{AddBlockDefinitions, AddClass, AddConstructor, AddField, AddImplemented, AddImport, AddMethod, AddParent, AddTypeLookup, Apply, CastObject, Debug, FindClass, FreshName, GetArguments, GetConstructor, GetField, GetMember, InitializeField, InitializeParent, InstanceOfType, InstantiateObject, Reify, RemoveMethod, ResolveImport, SelfReference, SetAbstract, SetInterface, SetOverride, SetParameters, SetStatic, SuperReference, ToTargetLanguageType, ObjectOriented as OOP}
import org.combinators.cogen.{Command, Understands}
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait OOParadigm[AST <: OOAST, B](val base: AnyParadigm.WithAST[AST] & B) extends OOP {
  import base.ast.any
  import base.ast.factory
  import base.ast.ooFactory
  import base.ast.oo.*
  import base.ast.any.*
  import base.ProjectContext
  import base.TestContext
  type ClassContext = Class
  type MethodBodyContext = Method
  type ConstructorContext = Constructor
  type CompilationUnitContext = CompilationUnit
  val classCapabilities: ClassCapabilities = new ClassCapabilities {
    implicit val canDebugInClass: Understands[Class, Debug] = new Understands[Class, Debug] {
      def perform(context: Class, command: Debug): (Class, Unit) = {
        println(command.tag)
        (context, ())
      }
    }
    implicit val canAddTypeLookupForMethodsInClass: Understands[Class, AddTypeLookup[MethodBodyContext, Type]] = new Understands[Class, AddTypeLookup[MethodBodyContext, Type]] {
      def perform(context: Class, command: AddTypeLookup[MethodBodyContext, Type]): (Class, Unit) = {
        (context.addTypeLookupsForMethods((tpeRep: TypeRep) => if (tpeRep == command.tpe) Some(command.lookup) else None), ())
      }
    }
    implicit val canAddTypeLookupForClassesInClass: Understands[Class, AddTypeLookup[Class, Type]] = new Understands[Class, AddTypeLookup[Class, Type]] {
      def perform(context: Class, command: AddTypeLookup[Class, Type]): (Class, Unit) = {
        (context.addTypeLookupsForClasses((tpeRep: TypeRep) => if (tpeRep == command.tpe) Some(command.lookup) else None), ())
      }
    }
    implicit val canAddTypeLookupForConstructorsInClass: Understands[Class, AddTypeLookup[ConstructorContext, Type]] = new Understands[Class, AddTypeLookup[ConstructorContext, Type]] {
      def perform(context: Class, command: AddTypeLookup[ConstructorContext, Type]): (Class, Unit) = {
        (context.addTypeLookupsForConstructors((tpeRep: TypeRep) => if (tpeRep == command.tpe) Some(command.lookup) else None), ())
      }
    }
    implicit val canAddParentInClass: Understands[Class, AddParent[Type]] = new Understands[Class, AddParent[Type]] {
      def perform(context: Class, command: AddParent[Type]): (Class, Unit) = {
        (context.copy(parents = context.parents :+ command.parentClass), ())
      }
    }
    implicit val canAddImplementedInClass: Understands[Class, AddImplemented[Type]] = new Understands[Class, AddImplemented[Type]] {
      def perform(context: Class, command: AddImplemented[Type]): (Class, Unit) = {
        (context.copy(implemented = context.implemented :+ command.interface), ())
      }
    }
    implicit val canRemoveMethodFromClass: Understands[Class, RemoveMethod[Type, Name]] = new Understands[Class, RemoveMethod[Type, Name]] {
      override def perform(context: Class, command: RemoveMethod[Type, Name]): (Class, Unit) = {
        ??? // TODO: Remove me
      }
    }
    implicit val canAddFieldInClass: Understands[Class, AddField[Name, Type, Expression]] = new Understands[Class, AddField[Name, Type, Expression]] {
      def perform(context: Class, command: AddField[Name, Type, Expression]): (Class, Unit) = {
        (context.copy(fields = context.fields :+ ooFactory.field(command.name, command.tpe, command.initializer)), ())
      }
    }
    implicit val canGetFieldInClass: Understands[Class, GetField[Name, Expression]] = new Understands[Class, GetField[Name, Expression]] {
      def perform(context: Class, command: GetField[Name, Expression]): (Class, Expression) = {
        (context, ooFactory.memberAccessExpression(ooFactory.selfReferenceExpression, context.fields.find(f => f.name == command.name).get.name))
      }
    }
    implicit val canAddMethodInClass: Understands[Class, AddMethod[MethodBodyContext, Name, Option[Expression]]] = new Understands[Class, AddMethod[MethodBodyContext, Name, Option[Expression]]] {
      def perform(context: Class, command: AddMethod[MethodBodyContext, Name, Option[Expression]]): (Class, Unit) = {
        val converted = ooFactory.convert(context)
        val emptyMethod = ooFactory.clsMethod(
          name = command.name,
          isPublic = command.isPublic,
          isOverride = command.isOverride,
          typeLookupMap = context.methodTypeLookupMap
        )
        var (generatedMethod, result) = Command.runGenerator(command.spec, emptyMethod)
        if (result.isDefined) {
          generatedMethod = generatedMethod.copy(statements = generatedMethod.statements :+ factory.returnExpression(result.get))
        }
        (context.copy(methods = context.methods :+ generatedMethod), ())
      }
    }
    implicit val canAddConstructorInClass: Understands[Class, AddConstructor[ConstructorContext]] = new Understands[Class, AddConstructor[ConstructorContext]] {
      def perform(context: Class, command: AddConstructor[Constructor]): (Class, Unit) = {
        val converted = ooFactory.convert(context)
        val emptyConstructor = ooFactory.constructor(
          constructedType = Some(ooFactory.classReferenceType(context.name)),
          constructorTypeLookupMap = context.constructorTypeLookupMap,
          typeLookupMap = context.methodTypeLookupMap)
        val (generatedConstructor, ()) = Command.runGenerator(command.ctor, emptyConstructor)
        (context.copy(constructors = context.constructors :+ generatedConstructor), ())
      }
    }
    implicit val canAddImportInClass: Understands[Class, AddImport[Import]] = new Understands[Class, AddImport[Import]] {
      def perform(context: Class, command: AddImport[Import]): (Class, Unit) = {
        (context.copy(imports = (context.imports :+ command.imp).distinct), ())
      }
    }
    implicit val canResolveImportInClass: Understands[Class, ResolveImport[Import, Type]] = new Understands[Class, ResolveImport[Import, Type]] {
      def perform(context: Class, command: ResolveImport[Import, Type]): (Class, Option[Import]) = {
        (context, context.resolveImport(command.forElem).headOption)
      }
    }
    implicit val canSetAbstractInClass: Understands[Class, SetAbstract] = new Understands[Class, SetAbstract] {
      def perform(context: Class, command: SetAbstract): (Class, Unit) = {
        (context.copy(isAbstract = true), ())
      }
    }
    implicit val canSetStaticInClass: Understands[Class, SetStatic] = new Understands[Class, SetStatic] {
      def perform(context: Class, command: SetStatic): (Class, Unit) = {
        (context.copy(isStatic = true), ())
      }
    }
    implicit val canSetInterfaceInClass: Understands[Class, SetInterface] = new Understands[Class, SetInterface] {
      def perform(context: Class, command: SetInterface): (Class, Unit) = {
        (context.copy(isInterface = true), ())
      }
    }
    implicit val canTranslateTypeInClass: Understands[Class, ToTargetLanguageType[Type]] = new Understands[Class, ToTargetLanguageType[Type]] {
      def perform(context: Class, command: ToTargetLanguageType[Type]): (Class, Type) = {
        Command.runGenerator(context.toTargetLanguageType(command.tpe), context)
      }
    }
    implicit val canSelfReferenceInClass: Understands[Class, SelfReference[Expression]] = new Understands[Class, SelfReference[Expression]] {
      def perform(context: Class, command: SelfReference[Expression]): (Class, Expression) = {
        (context, ooFactory.selfReferenceExpression)
      }
    }
    implicit val canFindClassInClass: Understands[Class, FindClass[Name, Type]] = new Understands[Class, FindClass[Name, Type]] {
      def perform(context: Class, command: FindClass[Name, Type]): (Class, Type) = {
        (context, context.findClass(command.qualifiedName*))
      }
    }
    implicit val canGetFreshNameInClass: Understands[Class, FreshName[Name]] = new Understands[Class, FreshName[Name]] {
      def perform(context: Class, command: FreshName[Name]): (Class, Name) = {
        (context, context.getFreshName(command.basedOn))
      }
    }
  }
  val constructorCapabilities: ConstructorCapabilities = new ConstructorCapabilities {
    implicit val canInitializeParentInConstructor: Understands[Constructor, InitializeParent[Type, Expression]] = new Understands[Constructor, InitializeParent[Type, Expression]] {
      def perform(context: Constructor, command: InitializeParent[Type, Expression]): (Constructor, Unit) = {
        (context.copyAsConstructor(superInitialization = Some((command.parent, command.arguments))), ())
      }
    }
    implicit val canCastInConstructor: Understands[Constructor, CastObject[Type, Expression]] = new Understands[Constructor, CastObject[Type, Expression]] {
      def perform(context: Constructor, command: CastObject[Type, Expression]): (Constructor, Expression) = {
        (context, ooFactory.castExpression(command.tpe, command.expr))
      }
    }
    implicit val canInitializeFieldInConstructor: Understands[Constructor, InitializeField[Name, Expression]] = new Understands[Constructor, InitializeField[Name, Expression]] {
      def perform(context: Constructor, command: InitializeField[Name, Expression]): (Constructor, Unit) = {
        (context.copyAsConstructor(fieldInitializers = context.fieldInitializers :+ (command.name, command.value)), ())
      }
    }
    implicit val canAddBlockDefinitionsInConstructor: Understands[Constructor, AddBlockDefinitions[Statement]] = new Understands[Constructor, AddBlockDefinitions[Statement]] {
      def perform(context: Constructor, command: AddBlockDefinitions[Statement]): (Constructor, Unit) = {
        (context.copyAsConstructor(statements = context.statements ++ command.definitions), ())
      }
    }
    implicit val canAddImportInConstructor: Understands[Constructor, AddImport[Import]] = new Understands[Constructor, AddImport[Import]] {
      def perform(context: Constructor, command: AddImport[Import]): (Constructor, Unit) = {
        (context.copyAsConstructor(imports = context.imports + command.imp), ())
      }
    }
    implicit val canResolveImportInConstructor: Understands[Constructor, ResolveImport[Import, Type]] = new Understands[Constructor, ResolveImport[Import, Type]] {
      def perform(context: Constructor, command: ResolveImport[Import, Type]): (Constructor, Option[Import]) = {
        (context, context.resolveImport(command.forElem).headOption)
      }
    }
    implicit val canInstantiateObjectInConstructor: Understands[Constructor, InstantiateObject[Type, Expression, Class]] = new Understands[Constructor, InstantiateObject[Type, Expression, Class]] {
      def perform(context: Constructor, command: InstantiateObject[Type, Expression, Class]): (Constructor, Expression) = {
        (context, ooFactory.objectInstantiationExpression(command.tpe, command.constructorArguments, None)) // TODO: add anonymous inner class declarations
      }
    }
    implicit val canApplyInConstructor: Understands[Constructor, Apply[Expression, Expression, Expression]] = new Understands[Constructor, Apply[Expression, Expression, Expression]] {
      def perform(context: Constructor, command: Apply[Expression, Expression, Expression]): (Constructor, Expression) = {
        (context, factory.applyExpression(command.functional, command.arguments))
      }
    }
    implicit val canGetMemberInConstructor: Understands[Constructor, GetMember[Expression, Name]] = new Understands[Constructor, GetMember[Expression, Name]] {
      def perform(context: Constructor, command: GetMember[Expression, Name]): (Constructor, Expression) = {
        (context, ooFactory.memberAccessExpression(command.instance, command.member))
      }
    }
    implicit val canSelfReferenceInConstructor: Understands[Constructor, SelfReference[Expression]] = new Understands[Constructor, SelfReference[Expression]] {
      def perform(context: Constructor, command: SelfReference[Expression]): (Constructor, Expression) = {
        (context, ooFactory.selfReferenceExpression)
      }
    }
    implicit val canGetArgumentsInConstructor: Understands[Constructor, GetArguments[Type, Name, Expression]] = new Understands[Constructor, GetArguments[Type, Name, Expression]] {
      def perform(context: Constructor, command: GetArguments[Type, Name, Expression]): (Constructor, Seq[(Name, Type, Expression)]) = {
        (context, context.parameters.map(param => (param._1, param._2, factory.argumentExpression(param._1))))
      }
    }
    implicit val canTranslateTypeInConstructor: Understands[Constructor, ToTargetLanguageType[Type]] = new Understands[Constructor, ToTargetLanguageType[Type]] {
      def perform(context: Constructor, command: ToTargetLanguageType[Type]): (Constructor, Type) = {
        Command.runGenerator(context.toTargetLanguageTypeInConstructor(command.tpe), context)
      }
    }
    implicit def canReifyInConstructor[T]: Understands[Constructor, Reify[T, Expression]] = new Understands[Constructor, Reify[T, Expression]] {
      def perform(context: Constructor, command: Reify[T, Expression]): (Constructor, Expression) = {
        (context, context.reify(command.tpe, command.value))
      }
    }
    implicit val canSetParametersInConstructor: Understands[Constructor, SetParameters[Name, Type]] = new Understands[Constructor, SetParameters[Name, Type]] {
      def perform(context: Constructor, command: SetParameters[Name, Type]): (Constructor, Unit) = {
        (context.copyAsConstructor(parameters = command.params), ())
      }
    }
    implicit val canGetConstructorInConstructor: Understands[Constructor, GetConstructor[Type, Expression]] = new Understands[Constructor, GetConstructor[Type, Expression]] {
      def perform(context: Constructor, command: GetConstructor[Type, Expression]): (Constructor, Expression) = {
        ???
      }
    }
    implicit val canFindClassInConstructor: Understands[Constructor, FindClass[Name, Type]] = new Understands[Constructor, FindClass[Name, Type]] {
      def perform(context: Constructor, command: FindClass[Name, Type]): (Constructor, Type) = {
        (context, context.findClass(command.qualifiedName*))
      }
    }
    implicit val canGetFreshNameInConstructor: Understands[Constructor, FreshName[Name]] = new Understands[Constructor, FreshName[Name]] {
      def perform(context: Constructor, command: FreshName[Name]): (Constructor, Name) = {
        (context, context.getFreshName(command.basedOn))
      }
    }
  }
  val methodBodyCapabilities: MethodBodyCapabilities = new MethodBodyCapabilities {
    implicit val canInstantiateObjectInMethod: Understands[MethodBodyContext, InstantiateObject[Type, Expression, ClassContext]] = new Understands[MethodBodyContext, InstantiateObject[Type, Expression, Class]] {
      def perform(context: any.Method, command: InstantiateObject[Type, Expression, Class]): (any.Method, Expression) = {
        (context, ooFactory.objectInstantiationExpression(command.tpe, command.constructorArguments, None)) // TODO: add anonymous inner class declarations
      }
    }
    implicit val canGetMemberInMethod: Understands[any.Method, GetMember[Expression, Name]] = new Understands[any.Method, GetMember[Expression, Name]] {
      def perform(context: any.Method, command: GetMember[Expression, Name]): (any.Method, Expression) = {
        (context, ooFactory.memberAccessExpression(command.instance, command.member))
      }
    }
    implicit val canCastInMethod: Understands[any.Method, CastObject[Type, Expression]] = new Understands[any.Method, CastObject[Type, Expression]] {
      def perform(context: any.Method, command: CastObject[Type, Expression]): (any.Method, Expression) = {
        (context, ooFactory.castExpression(command.tpe, command.expr))
      }
    }
    implicit val canInstanceOfTypeInMethod: Understands[any.Method, InstanceOfType[Type, Expression]] = new Understands[any.Method, InstanceOfType[Type, Expression]] {
      def perform(context: any.Method, command: InstanceOfType[Type, Expression]): (any.Method, Expression) = {
        (context, ooFactory.instanceOfExpression(command.tpe, command.expr))
      }
    }
    implicit val canSetAbstractInMethod: Understands[any.Method, SetAbstract] = new Understands[any.Method, SetAbstract] {
      def perform(context: any.Method, command: SetAbstract): (any.Method, Unit) = {
        (factory.convert(context).copyAsClsMethod(isAbstract = true), ())
      }
    }
    implicit val canSetStaticInMethod: Understands[any.Method, SetStatic] = new Understands[any.Method, SetStatic] {
      def perform(context: any.Method, command: SetStatic): (any.Method, Unit) = {
        (factory.convert(context).copyAsClsMethod(isStatic = true), ())
      }
    }
    implicit val canSetOverrideInMethod: Understands[any.Method, SetOverride] = new Understands[any.Method, SetOverride] {
      def perform(context: any.Method, command: SetOverride): (any.Method, Unit) = {
        (factory.convert(context).copyAsClsMethod(isOverride = true), ())
      }
    }
    implicit val canSelfReferenceInMethod: Understands[any.Method, SelfReference[Expression]] = new Understands[any.Method, SelfReference[Expression]] {
      def perform(context: any.Method, command: SelfReference[Expression]): (any.Method, Expression) = {
        (context, ooFactory.selfReferenceExpression)
      }
    }
    implicit val canSuperReferenceInMethod: Understands[any.Method, SuperReference[Name, Expression]] = new Understands[any.Method, SuperReference[Name, Expression]] {
      def perform(context: any.Method, command: SuperReference[Name, Expression]): (any.Method, Expression) = {
        (context, ooFactory.superReferenceExpression(factory.convert(context).findClass(command.qualifiedName*)))
      }
    }
    implicit val canGetConstructorInMethod: Understands[any.Method, GetConstructor[Type, Expression]] = new Understands[any.Method, GetConstructor[Type, Expression]] {
      def perform(context: any.Method, command: GetConstructor[Type, Expression]): (any.Method, Expression) = {
        ??? // TODO: Remove
      }
    }
    implicit val canFindClassInMethod: Understands[any.Method, FindClass[Name, Type]] = new Understands[any.Method, FindClass[Name, Type]] {
      def perform(context: any.Method, command: FindClass[Name, Type]): (any.Method, Type) = {
        (context, factory.convert(context).findClass(command.qualifiedName*))
      }
    }
  }
  val compilationUnitCapabilities: CompilationUnitCapabilities = new CompilationUnitCapabilities {
    implicit val canAddClassInCompilationUnit: Understands[CompilationUnit, AddClass[Class, Name]] = new Understands[CompilationUnit, AddClass[Class, Name]] {
      def perform(context: CompilationUnit, command: AddClass[Class, Name]): (CompilationUnit, Unit) = {
        val converted = factory.convert(context)
        val emptyCls = ooFactory.cls(
          name = command.name,
          methodTypeLookupMap = converted.methodTypeLookupMap,
          constructorTypeLookupMap = converted.constructorTypeLookupMap,
          typeLookupMap = converted.classTypeLookupMap)
        var (generatedClass, result) = Command.runGenerator(command.cls, emptyCls)
        (converted.copyAsCompilationUnitWithClasses(classes = converted.classes :+ generatedClass), ())
      }
    }
  }
  val projectCapabilities: ProjectCapabilities = new ProjectCapabilities {
    implicit val canAddTypeLookupForClassesInProject: Understands[ProjectContext, AddTypeLookup[Class, Type]] = new Understands[ProjectContext, AddTypeLookup[Class, Type]] {
      def perform(context: ProjectContext, command: AddTypeLookup[Class, Type]): (ProjectContext, Unit) = {
        (factory.convert(context).addTypeLookupsForClasses((tpeRep: TypeRep) => if (tpeRep == command.tpe) Some(command.lookup) else None), ())
      }
    }
    implicit val canAddTypeLookupForConstructorsInProject: Understands[ProjectContext, AddTypeLookup[Constructor, Type]] = new Understands[ProjectContext, AddTypeLookup[Constructor, Type]] {
      def perform(context: ProjectContext, command: AddTypeLookup[Constructor, Type]): (ProjectContext, Unit) = {
        (factory.convert(context).addTypeLookupsForConstructors((tpeRep: TypeRep) => if (tpeRep == command.tpe) Some(command.lookup) else None), ())
      }
    }
  }
  val testCapabilities: TestCapabilities = new TestCapabilities {
    implicit val canAddMethodInTest: Understands[TestContext, AddMethod[MethodBodyContext, Name, Option[Expression]]] = new Understands[TestContext, AddMethod[MethodBodyContext, Name, Option[Expression]]] {
      def perform(context: TestContext, command: AddMethod[MethodBodyContext, Name, Option[Expression]]): (TestContext, Unit) = {
        val clsBasedTestSuite = factory.convert(context)
        import classCapabilities.canAddMethodInClass
        val (updatedCls, ()) = Command.runGenerator(classCapabilities.addMethod(command.name, command.spec), clsBasedTestSuite.underlyingClass)
        (clsBasedTestSuite.copyAsClassBasedTestSuite(underlyingClass = updatedCls, testMarkers = clsBasedTestSuite.testMarkers :+ false), ())
      }
    }
    implicit val canAddBlockDefinitionsInTest: Understands[TestContext, AddBlockDefinitions[Statement]] = new Understands[TestContext, AddBlockDefinitions[Statement]] {
      def perform(context: TestContext, command: AddBlockDefinitions[Statement]): (TestContext, Unit) = {
        val clsBasedTestSuite = factory.convert(context)
        import classCapabilities.canAddConstructorInClass
        import constructorCapabilities.canAddBlockDefinitionsInConstructor
        val clsWithUpdatedPrimaryConstructor =
          if (clsBasedTestSuite.underlyingClass.constructors.isEmpty) {
            Command.runGenerator(
              classCapabilities.addConstructor(constructorCapabilities.addBlockDefinitions(command.definitions)),
              clsBasedTestSuite.underlyingClass
            )._1
          } else {
            val updatedFirstConstructor = Command.runGenerator(
              constructorCapabilities.addBlockDefinitions(command.definitions),
              clsBasedTestSuite.underlyingClass.constructors.head
            )._1
            clsBasedTestSuite.underlyingClass.copy(
              constructors = updatedFirstConstructor +: clsBasedTestSuite.underlyingClass.constructors.tail
            )
          }
        (clsBasedTestSuite.copyAsClassBasedTestSuite(underlyingClass = clsWithUpdatedPrimaryConstructor), ())
      }
    }
    implicit val canAddFieldInTest: Understands[TestContext, AddField[Name, Type, Expression]] = new Understands[TestContext, AddField[Name, Type, Expression]] {
      def perform(context: TestContext, command: AddField[Name, Type, Expression]): (TestContext, Unit) = {
        val clsBasedTestSuite = factory.convert(context)
        import classCapabilities.canAddFieldInClass
        val (updatedCls, ()) = Command.runGenerator(
          classCapabilities.addField(
            name = command.name,
            tpe = command.tpe,
            init = command.initializer
          ), clsBasedTestSuite.underlyingClass)
        (clsBasedTestSuite.copyAsClassBasedTestSuite(underlyingClass = updatedCls), ())
      }
    }
    implicit val canInitializeFieldInTest: Understands[TestContext, InitializeField[Name, Expression]] = new Understands[TestContext, InitializeField[Name, Expression]] {
      def perform(context: TestContext, command: InitializeField[Name, Expression]): (TestContext, Unit) = {
        val clsBasedTestSuite = factory.convert(context)
        import classCapabilities.canAddConstructorInClass
        import constructorCapabilities.canInitializeFieldInConstructor
        val clsWithUpdatedPrimaryConstructor =
          if (clsBasedTestSuite.underlyingClass.constructors.isEmpty) {
            Command.runGenerator(
              classCapabilities.addConstructor(constructorCapabilities.initializeField(command.name, command.value)),
              clsBasedTestSuite.underlyingClass
            )._1
          } else {
            val updatedFirstConstructor = Command.runGenerator(
              constructorCapabilities.initializeField(command.name, command.value),
              clsBasedTestSuite.underlyingClass.constructors.head
            )._1
            clsBasedTestSuite.underlyingClass.copy(
              constructors = updatedFirstConstructor +: clsBasedTestSuite.underlyingClass.constructors.tail
            )
          }
        (clsBasedTestSuite.copyAsClassBasedTestSuite(underlyingClass = clsWithUpdatedPrimaryConstructor), ())
      }
    }
    implicit val canInstantiateObjectInTest: Understands[TestContext, InstantiateObject[Type, Expression, TestContext]] = new Understands[TestContext, InstantiateObject[Type, Expression, TestContext]] {
      def perform(context: TestContext, command: InstantiateObject[Type, Expression, TestContext]): (TestContext, Expression) = {
        (context, ooFactory.objectInstantiationExpression(command.tpe, command.constructorArguments, None)) // TODO: Add anon inner class
      }
    }
    implicit val canAddConstructorInTest: Understands[TestContext, AddConstructor[Constructor]] = new Understands[TestContext, AddConstructor[Constructor]] {
      def perform(context: TestContext, command: AddConstructor[Constructor]): (TestContext, Unit) = {
        val clsBasedTestSuite = factory.convert(context)
        import classCapabilities.canAddConstructorInClass
        val (updatedCls, ()) = Command.runGenerator(classCapabilities.addConstructor(command.ctor), clsBasedTestSuite.underlyingClass)
        (clsBasedTestSuite.copyAsClassBasedTestSuite(underlyingClass = updatedCls), ())
      }
    }
    implicit val canAddImportInTest: Understands[TestContext, AddImport[Import]] = new Understands[TestContext, AddImport[Import]] {
      def perform(context: TestContext, command: AddImport[Import]): (TestContext, Unit) = {
        val clsBasedTestSuite = factory.convert(context)
        import classCapabilities.canAddImportInClass
        val (updatedCls, ()) = Command.runGenerator(classCapabilities.addImport(command.imp), clsBasedTestSuite.underlyingClass)
        (clsBasedTestSuite.copyAsClassBasedTestSuite(underlyingClass = updatedCls), ())
      }
    }
    implicit val canResolveImportInTest: Understands[TestContext, ResolveImport[Import, Type]] = new Understands[TestContext, ResolveImport[Import, Type]] {
      def perform(context: TestContext, command: ResolveImport[Import, Type]): (TestContext, Option[Import]) = {
        val clsBasedTestSuite = factory.convert(context)
        import classCapabilities.canResolveImportInClass
        val (updatedCls, result) = Command.runGenerator(classCapabilities.resolveImport(command.forElem), clsBasedTestSuite.underlyingClass)
        (clsBasedTestSuite.copyAsClassBasedTestSuite(underlyingClass = updatedCls), result)
      }
    }
    implicit val canFindClassInTest: Understands[TestContext, FindClass[Name, Type]] = new Understands[TestContext, FindClass[Name, Type]] {
      def perform(context: TestContext, command: FindClass[Name, Type]): (TestContext, Type) = {
        val clsBasedTestSuite = factory.convert(context)
        import classCapabilities.canFindClassInClass
        val (updatedCls, result) = Command.runGenerator(classCapabilities.findClass(command.qualifiedName*), clsBasedTestSuite.underlyingClass)
        (clsBasedTestSuite.copyAsClassBasedTestSuite(underlyingClass = updatedCls), result)
      }
    }
    implicit val canAddImplementedInTest: Understands[TestContext, AddImplemented[Type]] = new Understands[TestContext, AddImplemented[Type]] {
      def perform(context: TestContext, command: AddImplemented[Type]): (TestContext, Unit) = {
        val clsBasedTestSuite = factory.convert(context)
        import classCapabilities.canAddImplementedInClass
        val (updatedCls, ()) = Command.runGenerator(classCapabilities.addImplemented(command.interface), clsBasedTestSuite.underlyingClass)
        (clsBasedTestSuite.copyAsClassBasedTestSuite(underlyingClass = updatedCls), ())
      }
    }
  }
}

object OOParadigm {
  type WithBase[AST <: OOAST, B <: AnyParadigm.WithAST[AST]] = OOParadigm[AST, B] { }
  def apply[AST <: OOAST, B <: AnyParadigm.WithAST[AST]](_base: B): WithBase[AST, B] = new OOParadigm[AST, B](_base) {}
}
