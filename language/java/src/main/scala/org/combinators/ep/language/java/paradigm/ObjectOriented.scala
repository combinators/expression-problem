package org.combinators.ep.language.java.paradigm    /*DI:LD:AI*/

import java.util.UUID
import com.github.javaparser.ast.{ImportDeclaration, Modifier, NodeList}
import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, ConstructorDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.{AssignExpr, CastExpr, EnclosedExpr, Expression, FieldAccessExpr, InstanceOfExpr, MethodCallExpr, NameExpr, ObjectCreationExpr, ThisExpr, TypeExpr, Name => JName}
import com.github.javaparser.ast.stmt.{BlockStmt, ExplicitConstructorInvocationStmt, ExpressionStmt, ReturnStmt}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.{ObjectOriented => OO, AnyParadigm => _, _}
import org.combinators.ep.language.java.Syntax.MangledName
import org.combinators.ep.language.java.{ClassCtxt, CompilationUnitCtxt, ContextSpecificResolver, CtorCtxt, JavaNameProvider, MethodBodyCtxt, TestCtxt}

import scala.util.Try
import scala.jdk.CollectionConverters._

trait ObjectOriented[AP <: AnyParadigm] extends OO {
  val base: AP
  import base.syntax._
  import base.config

  type ClassContext = ClassCtxt
  type ConstructorContext = CtorCtxt

  val compilationUnitCapabilities: CompilationUnitCapabilities =
    new CompilationUnitCapabilities {
      implicit val canAddClassInCompilationUnit: Understands[base.CompilationUnitContext, AddClass[ClassContext, Name]] =
        new Understands[base.CompilationUnitContext, AddClass[ClassContext, Name]] {
          def perform(
            context: base.CompilationUnitContext,
            command: AddClass[ClassContext, Name]
          ): (base.CompilationUnitContext, Unit) = {
            val clsToAdd = new ClassOrInterfaceDeclaration()
            clsToAdd.setName(command.name.toAST)
            clsToAdd.setPublic(true)
            val (resultCtxt, _) =
              Command.runGenerator(
                command.cls,
                ClassCtxt(context.resolver, clsToAdd, context.unit.getImports().asScala)
              )
            val newUnit = context.unit.clone()
            newUnit.addType(resultCtxt.cls)
            resultCtxt.extraImports.foreach { imp =>
              if (!newUnit.getImports.contains(imp) && imp.getName.getIdentifier != command.name.toAST.getIdentifier) {
                newUnit.addImport(imp.clone())
              }
            }
            newUnit.getImports.sort((i1, i2) => i1.toString.compareTo(i2.toString))
            (context.copy(resolver = resultCtxt.resolver, unit = newUnit), ())
          }
        }
    }

  val classCapabilities: ClassCapabilities =
    new ClassCapabilities {
      import base.syntax._

      implicit val canDebugInClass: Understands[ClassContext, Debug] =
        new Understands[ClassContext, Debug] {
          def perform(
            context: ClassContext,
            command: Debug
          ): (ClassContext, Unit) = {

            System.err.println (command.tag + ": " + context.cls)
            (context,())
          }
        }

      implicit val canAddTypeLookupForClassesInClass: Understands[ClassContext, AddTypeLookup[ClassContext, Type]] =
        new Understands[ClassContext, AddTypeLookup[ClassContext, Type]] {
          def perform(
                       context: ClassContext,
                       command: AddTypeLookup[ClassContext, Type]
                     ): (ClassContext, Unit) = {
            def newLookup(k: ContextSpecificResolver)(tpe: TypeRep): Generator[ClassContext, Type] =
              if (tpe == command.tpe) {
                command.lookup
              } else {
                context.resolver._classTypeResolution(k)(tpe)
              }
            (context.copy(resolver = context.resolver.copy(_classTypeResolution = newLookup)), ())
          }
        }

      implicit val canAddTypeLookupForConstructorsInClass: Understands[ClassContext, AddTypeLookup[ConstructorContext, Type]] =
        new Understands[ClassContext, AddTypeLookup[ConstructorContext, Type]] {
          def perform(
                       context: ClassContext,
                       command: AddTypeLookup[ConstructorContext, Type]
                     ): (ClassContext, Unit) = {
            def newLookup(k: ContextSpecificResolver)(tpe: TypeRep): Generator[ConstructorContext, Type] =
              if (tpe == command.tpe) {
                command.lookup
              } else {
                context.resolver._constructorTypeResolution(k)(tpe)
              }
            (context.copy(resolver = context.resolver.copy(_constructorTypeResolution = newLookup)), ())
          }
        }

      import base.MethodBodyContext
      implicit val canAddTypeLookupForMethodsInClass: Understands[ClassContext, AddTypeLookup[MethodBodyContext, Type]] =
        new Understands[ClassContext, AddTypeLookup[MethodBodyContext, Type]] {
          def perform(
                       context: ClassContext,
                       command: AddTypeLookup[MethodBodyCtxt, Type]
                     ): (ClassContext, Unit) = {
            def newLookup(k: ContextSpecificResolver)(tpe: TypeRep): Generator[MethodBodyCtxt, Type] = {
              if (tpe == command.tpe) {
                command.lookup
              } else {
                context.resolver._methodTypeResolution(k)(tpe)
              }
            }
            (context.copy(resolver = context.resolver.copy(_methodTypeResolution = newLookup)), ())
          }
        }

      implicit val canAddParentInClass: Understands[ClassContext, AddParent[Type]] =
        new Understands[ClassContext, AddParent[Type]] {
          def perform(
            context: ClassContext,
            command: AddParent[Type]
          ): (ClassContext, Unit) = {
            val resultCls = context.cls.clone()
            resultCls.addExtendedType(command.parentClass.asClassOrInterfaceType())
            (context.copy(cls = resultCls), ())
          }
        }
      implicit val canAddImplementedInClass: Understands[ClassContext, AddImplemented[Type]] =
        new Understands[ClassContext, AddImplemented[Type]] {
          def perform(
            context: ClassContext,
            command: AddImplemented[Type]
          ): (ClassContext, Unit) = {
            val resultCls = context.cls.clone()
            resultCls.addImplementedType(command.interface.asClassOrInterfaceType())
            (context.copy(cls = resultCls), ())
          }
        }
      implicit val canRemoveMethodFromClass: Understands[ClassContext, RemoveMethod[Type, Name]] =
        new Understands[ClassContext, RemoveMethod[Type, Name]] {
          def perform(
            context: ClassContext,
            command: RemoveMethod[Type, Name]
          ): (ClassContext, Unit) = {
            val resultCls = context.cls.clone()

            // TODO: DO SOMETHING HERE (HEINEMAN)
            val method = resultCls.getMethodsByName(command.name.mangled)
            resultCls.remove(method.get(0))

            (context.copy(cls = resultCls), ())
          }
        }
      implicit val canAddFieldInClass: Understands[ClassContext, AddField[Name, Type, Expression]] =
        new Understands[ClassContext, AddField[Name, Type, Expression]] {
          def perform(
            context: ClassContext,
            command: AddField[Name, Type, Expression]
          ): (ClassContext, Unit) = {
            val resultCls = context.cls.clone()
            val modifiers: Seq[Modifier.Keyword] =
            (if (command.isVisibleToSubclasses) Seq(Modifier.protectedModifier().getKeyword) else Seq(Modifier.privateModifier().getKeyword)) ++ (if (command.isMutable) Seq.empty else Seq(Modifier.finalModifier().getKeyword))
            resultCls.addField(command.tpe, command.name.toAST.toString, modifiers:_*)
            (context.copy(cls = resultCls), ())
          }
        }

      implicit val canGetFieldInClass: Understands[ClassContext, GetField[Name,Expression]] =
        new Understands[ClassContext, GetField[Name, Expression]] {
          def perform(
            context: ClassContext,
            command: GetField[Name,Expression]
          ): (ClassContext, Expression) =
            (context, new NameExpr(command.name.mangled))
        }
      implicit val canAddMethodInClass: Understands[ClassContext, AddMethod[base.MethodBodyContext, Name, Option[Expression]]] =
        new Understands[ClassContext, AddMethod[base.MethodBodyContext, Name, Option[Expression]]] {
          def perform(
            context: ClassContext,
            command: AddMethod[base.MethodBodyContext, Name, Option[Expression]]
          ): (ClassContext, Unit) = {
            val methodToAdd = new MethodDeclaration()
            methodToAdd.setName(command.name.toAST)
            methodToAdd.setPublic(command.isPublic)
            val (methodCtxt, returnExp) =
              Command.runGenerator(
                command.spec,
                MethodBodyCtxt(context.resolver, context.extraImports, methodToAdd)
              )
            val resultingMethod = methodCtxt.method.clone()
            if (returnExp.isDefined && !resultingMethod.getType.isVoidType) {
              val body = resultingMethod.getBody.orElseGet(() => new BlockStmt())
              body.addStatement(new ReturnStmt(returnExp.get))
              resultingMethod.setBody(body)
            }
            val resultCls = context.cls.clone()
            resultCls.addMember(resultingMethod)
            val newImports = (context.extraImports ++ methodCtxt.extraImports).distinct.map(_.clone())
            (context.copy(resolver = methodCtxt.resolver, extraImports = newImports, cls = resultCls), ())
          }
        }

      implicit val canAddConstructorInClass: Understands[ClassContext, AddConstructor[ConstructorContext]] =
        new Understands[ClassContext, AddConstructor[ConstructorContext]] {
          def perform(
            context: ClassContext,
            command: AddConstructor[ConstructorContext]
          ): (ClassContext, Unit) = {
            val ctorToAdd = new ConstructorDeclaration()
            ctorToAdd.setPublic(true)
            ctorToAdd.setName(context.cls.getName.clone)
            val (ctorCtxt, _) =
              Command.runGenerator(
                command.ctor,
                CtorCtxt(context.resolver, context.extraImports, ctorToAdd)
              )
            val resultCls = context.cls.clone()
            resultCls.addMember(ctorCtxt.ctor.clone())
            val newImports = (context.extraImports ++ ctorCtxt.extraImports).distinct.map(_.clone())
            (context.copy(resolver = ctorCtxt.resolver, extraImports = newImports, cls = resultCls), ())
          }
        }

      implicit val canAddImportInClass: Understands[ClassContext, AddImport[Import]] =
        new Understands[ClassContext, AddImport[Import]] {
          def perform(
            context: ClassContext,
            command: AddImport[Import]
          ): (ClassContext, Unit) = {
            val newImports =
              if (context.extraImports.contains(command.imp)) {
                context.extraImports
              } else context.extraImports :+ command.imp.clone()
            (context.copy(extraImports = newImports), ())
          }
        }

      implicit val canResolveImportInClass: Understands[ClassContext, ResolveImport[Import, Type]] =
        new Understands[ClassContext, ResolveImport[Import, Type]] {
          def perform(
            context: ClassContext,
            command: ResolveImport[Import, Type]
          ): (ClassContext, Option[Import]) = {
            val stripped = AnyParadigm.stripGenerics(command.forElem)
            Try { (context, context.resolver.importResolution(stripped)) } getOrElse {
              val newImport =
                new ImportDeclaration(
                  ObjectOriented.typeToName(stripped.asClassOrInterfaceType()),
                  false,
                  false)
              if (context.extraImports.contains(newImport)) {
                (context, None)
              } else {
                (context, Some(newImport))
              }
            }
          }
        }
      implicit val canSetAbstractInClass: Understands[ClassContext, SetAbstract] =
        new Understands[ClassContext, SetAbstract] {
          def perform(
            context: ClassContext,
            command: SetAbstract
          ): (ClassContext, Unit) = {
            val newClass = context.cls.clone()
            newClass.setAbstract(true)
            (context.copy(cls = newClass), ())
          }
        }
      implicit val canSetStaticInClass: Understands[ClassContext, SetStatic] =
        new Understands[ClassContext, SetStatic] {
          def perform(
            context: ClassContext,
            command: SetStatic
          ): (ClassContext, Unit) = {
            val newClass = context.cls.clone()
            newClass.setStatic(true)
            newClass.setAbstract(false)
            (context.copy(cls = newClass), ())
          }
        }
      implicit val canSetInterfaceInClass: Understands[ClassContext, SetInterface] =
        new Understands[ClassContext, SetInterface] {
          def perform(
            context: ClassContext,
            command: SetInterface
          ): (ClassContext, Unit) = {
            val newClass = context.cls.clone()
            newClass.setInterface(true)
            newClass.getMethods.forEach { method =>
              if (method.getBody.isPresent) {
                method.setDefault(true)
              }
            }
            (context.copy(cls = newClass), ())
          }
        }

      implicit val canTranslateTypeInClass: Understands[ClassContext, ToTargetLanguageType[Type]] =
        new Understands[ClassContext, ToTargetLanguageType[Type]] {
          def perform(
            context: ClassContext,
            command: ToTargetLanguageType[Type]
          ): (ClassContext, Type) = {
            Command.runGenerator(context.resolver.classTypeResolution(command.tpe), context)
          }
        }
      implicit val canSelfReferenceInClass: Understands[ClassContext, SelfReference[Expression]] =
        new Understands[ClassContext, SelfReference[Expression]] {
          def perform(
            context: ClassContext,
            command: SelfReference[Expression]
          ): (ClassContext, Expression) = {
            (context, new com.github.javaparser.ast.expr.ThisExpr())
          }
        }
      implicit val canFindClassInClass: Understands[ClassContext, FindClass[Name, Type]] =
        new Understands[ClassContext, FindClass[Name, Type]] {
          def perform(
            context: ClassContext,
            command: FindClass[Name, Type]
          ): (ClassContext, Type) = {
            val start = new ClassOrInterfaceType()
            val fullName = ObjectOriented.components(config.targetPackage.getName).map(JavaNameProvider.mangle) ++ command.qualifiedName
            start.setName(fullName.head.toAST)
            val qualifiedName = fullName.tail.foldLeft(start){ case (scopes, suffix) =>
              new ClassOrInterfaceType(scopes, suffix.mangled)
            }
            (context, qualifiedName)
          }
        }
      implicit val canGetFreshNameInClass: Understands[ClassContext, FreshName[Name]] =
        new Understands[ClassContext, FreshName[Name]] {
          def perform(context: ClassContext, command: FreshName[Name]): (ClassContext, Name) = {
            val freshName = JavaNameProvider.mangle(s"$$$$generatedName_${UUID.randomUUID().toString.replace("-", "_")}$$$$")
            val updatedResolver = context.resolver.copy(
              generatedVariables = context.resolver.generatedVariables + (freshName.toAST.getIdentifier -> command.basedOn)
            )
            (context.copy(resolver = updatedResolver), freshName)
          }
        }
    }

  val constructorCapabilities: ConstructorCapabilities =
    new ConstructorCapabilities {
      implicit val canInitializeParentInConstructor: Understands[ConstructorContext, InitializeParent[Type, Expression]] =
        new Understands[ConstructorContext, InitializeParent[Type, Expression]] {
          def perform(
            context: ConstructorContext,
            command: InitializeParent[Type, Expression]
          ): (ConstructorContext, Unit) = {
            val newCtor = context.ctor.clone()
            val superCall =
              newCtor.getBody
                .findFirst(classOf[ExplicitConstructorInvocationStmt])
                .orElseGet(() => new ExplicitConstructorInvocationStmt())
            superCall.setThis(false)
            superCall.removeExpression()
            superCall.getArguments.clear()
            command.arguments.foreach { arg =>
              superCall.addArgument(arg.clone())
            }
            newCtor.getBody.addStatement(superCall)    // Necessary because of orElse above... [interpreter approach]
            (context.copy(ctor = newCtor), ())
          }
        }
      implicit val canInitializeFieldInConstructor: Understands[ConstructorContext, InitializeField[Name, Expression]] =
        new Understands[ConstructorContext, InitializeField[Name, Expression]] {
          def perform(
            context: ConstructorContext,
            command: InitializeField[Name, Expression]
          ): (ConstructorContext, Unit) = {
            val assignee = new FieldAccessExpr(new ThisExpr(), command.name.toAST.getIdentifier)
            val stmt = new ExpressionStmt(new AssignExpr(assignee, command.value, AssignExpr.Operator.ASSIGN))
            Command.runGenerator(
              addBlockDefinitions(Seq(stmt)),
              context
            )
          }
        }
      implicit val canAddBlockDefinitionsInConstructor: Understands[ConstructorContext, AddBlockDefinitions[Statement]] =
        new Understands[ConstructorContext, AddBlockDefinitions[Statement]] {
          def perform(
            context: CtorCtxt,
            command: AddBlockDefinitions[Statement]
          ): (CtorCtxt, Unit) = {
            val newCtor = context.ctor.clone()
            val body = newCtor.getBody
            command.definitions.foreach(stmt => body.addStatement(stmt))
            (context.copy(ctor = newCtor), ())
          }
        }
      implicit val canAddImportInConstructor: Understands[ConstructorContext, AddImport[Import]] =
        new Understands[ConstructorContext, AddImport[Import]] {
          def perform(
            context: ConstructorContext,
            command: AddImport[Import]
          ): (ConstructorContext, Unit) =
            (context.copy(extraImports = (context.extraImports :+ command.imp).distinct.map(_.clone())), ())
        }
      implicit val canResolveImportInConstructor: Understands[ConstructorContext, ResolveImport[Import, Type]] =
        new Understands[ConstructorContext, ResolveImport[ImportDeclaration, Type]] {
          def perform(
            context: ConstructorContext,
            command: ResolveImport[ImportDeclaration, Type]
          ): (ConstructorContext, Option[ImportDeclaration]) = {
            val stripped = AnyParadigm.stripGenerics(command.forElem)
            Try { (context, context.resolver.importResolution(stripped)) } getOrElse {
              val newImport =
                new ImportDeclaration(
                  ObjectOriented.typeToName(stripped.asClassOrInterfaceType()),
                  false,
                  false)
              if (context.extraImports.contains(newImport)) {
                (context, None)
              } else {
                (context, Some(newImport))
              }
            }
          }
        }
      implicit val canInstantiateObjectInConstructor: Understands[ConstructorContext, InstantiateObject[Type, Expression, ClassContext]] =
        new Understands[ConstructorContext, InstantiateObject[Type, Expression, ClassContext]] {
          def perform(
            context: ConstructorContext,
            command: InstantiateObject[Type, Expression, ClassContext]
          ): (ConstructorContext, Expression) = {
            val (tpe, args) = context.resolver.instantiationOverride(command.tpe, command.constructorArguments)
            /** Expand with instantiated body (if it exists). */
            val result = new ObjectCreationExpr()
            result.setType(tpe.asClassOrInterfaceType().clone())
            result.setArguments(new NodeList(args : _*))
            if (command.body.isDefined) {
              val ci = new ClassOrInterfaceDeclaration()
              val (newCtxt, classDef) = Command.runGenerator(command.body.get, ClassCtxt(context.resolver, ci, context.extraImports))
              result.setAnonymousClassBody(newCtxt.cls.getMembers)
              (context.copy(resolver = newCtxt.resolver, extraImports = newCtxt.extraImports), result)
             } else {
               (context, result)
             }
          }
        }

      implicit val canApplyInConstructor: Understands[ConstructorContext, Apply[Expression, Expression, Expression]] =
        new Understands[ConstructorContext, Apply[Expression, Expression, Expression]] {
          def perform(
            context: ConstructorContext,
            command: Apply[Expression, Expression, Expression]
          ): (ConstructorContext, Expression) = {
            if (command.functional.isMethodCallExpr) {
              val result = command.functional.asMethodCallExpr().clone()
              command.arguments.foreach(arg => result.addArgument(arg.clone()))
              (context, result)
            } else if (command.functional.isFieldAccessExpr) {
              val result = new MethodCallExpr()
              val functional = command.functional.asFieldAccessExpr()
              result.setScope(functional.getScope.clone())
              result.setName(functional.getName.clone())
              command.arguments.foreach(arg => result.addArgument(arg.clone()))
              (context, result)
            } else {
              val result = new MethodCallExpr(command.functional.toString, command.arguments:_*)
              (context, result)
            }
          }
        }

      implicit val canCastInConstructor: Understands[ConstructorContext, CastObject[Type, Expression]] =
        new Understands[ConstructorContext, CastObject[Type, Expression]] {
          def perform(
            context: ConstructorContext,
            command: CastObject[Type, Expression]
          ): (ConstructorContext, Expression) = {
            (context, new EnclosedExpr(new CastExpr(command.tpe.clone(), command.expr.clone())))
          }
        }

      implicit val canGetMemberInConstructor: Understands[ConstructorContext, GetMember[Expression, Name]] =
        new Understands[ConstructorContext, GetMember[Expression, Name]] {
          def perform(
            context: ConstructorContext,
            command: GetMember[Expression, Name]
          ): (ConstructorContext, Expression) = {
            (context, new FieldAccessExpr(command.instance.clone(), command.member.toAST.getIdentifier))
          }
        }
      implicit val canSelfReferenceInConstructor: Understands[ConstructorContext, SelfReference[Expression]] =
        new Understands[ConstructorContext, SelfReference[Expression]] {
          def perform(
            context: ConstructorContext,
            command: SelfReference[Expression]
          ): (ConstructorContext, Expression) = {
            (context, new com.github.javaparser.ast.expr.ThisExpr())
          }
        }

      implicit val canGetArgumentsInConstructor: Understands[ConstructorContext, GetArguments[Type, Name, Expression]] =
        new Understands[ConstructorContext, GetArguments[Type, Name, Expression]] {
          def perform(
            context: ConstructorContext,
            command: GetArguments[Type, Name, Expression]
          ): (ConstructorContext, Seq[(Name, Type, Expression)]) = {
            val args = context.ctor.getParameters.asScala.map { param =>
              (MangledName.fromAST(param.getName), param.getType.clone(), new NameExpr(param.getName.clone()))
            }
            (context, args)
          }
        }
      implicit val canTranslateTypeInConstructor: Understands[ConstructorContext, ToTargetLanguageType[Type]] =
        new Understands[ConstructorContext, ToTargetLanguageType[Type]] {
          def perform(
            context: ConstructorContext,
            command: ToTargetLanguageType[Type]
          ): (ConstructorContext, Type) = {
            Command.runGenerator(context.resolver.constructorTypeResolution(command.tpe), context)
          }
        }
      implicit def canReifyInConstructor[T]: Understands[ConstructorContext, Reify[T, Expression]] =
        new Understands[ConstructorContext, Reify[T, Expression]] {
          def perform(
            context: ConstructorContext,
            command: Reify[T, Expression]
          ): (ConstructorContext, Expression) = {
            Command.runGenerator(context.resolver.reificationInConstructor(InstanceRep(command.tpe)(command.value)), context)
          }
        }
      implicit val canSetParametersInConstructor: Understands[ConstructorContext, SetParameters[Name, Type]] =
        new Understands[ConstructorContext, SetParameters[Name, Type]] {
          def perform(
            context: ConstructorContext,
            command: SetParameters[Name, Type]
          ): (ConstructorContext, Unit) = {
            val newCtor = context.ctor.clone()
            newCtor.getParameters.clear()
            command.params.foreach { case (name, tpe) =>
              newCtor.addParameter(tpe, name.toAST.toString)
            }
            (context.copy(ctor = newCtor), ()) // second thing to be returned isn't optional, so make it () is like Unit
          }
        }

      implicit val canGetConstructorInConstructor: Understands[ConstructorContext, GetConstructor[Type, Expression]] =
        new Understands[ConstructorContext, GetConstructor[Type, Expression]] {
          def perform(
            context: ConstructorContext,
            command: GetConstructor[Type, Expression]
          ): (ConstructorContext, Expression) = {
            (context, new TypeExpr(command.tpe.clone()))
          }
        }
      implicit val canFindClassInConstructor: Understands[ConstructorContext, FindClass[Name, Type]] =
        new Understands[ConstructorContext, FindClass[Name, Type]] {
          def perform(
            context: ConstructorContext,
            command: FindClass[Name, Type]
          ): (ConstructorContext, Type) = {
            val start = new ClassOrInterfaceType()
            val fullName = ObjectOriented.components(config.targetPackage.getName).map(JavaNameProvider.mangle) ++ command.qualifiedName
            start.setName(fullName.head.toAST)
            val qualifiedName = fullName.tail.foldLeft(start){ case (scopes, suffix) =>
              new ClassOrInterfaceType(scopes, suffix.mangled)
            }

            (context, qualifiedName)
          }
        }
      implicit val canGetFreshNameInConstructor: Understands[ConstructorContext, FreshName[Name]] =
        new Understands[ConstructorContext, FreshName[Name]] {
          def perform(context: ConstructorContext, command: FreshName[MangledName]): (ConstructorContext, MangledName) = {
            val freshName = JavaNameProvider.mangle(s"$$$$generatedName_${UUID.randomUUID().toString.replace("-", "_")}$$$$")
            val updatedResolver = context.resolver.copy(
              generatedVariables = context.resolver.generatedVariables + (freshName.toAST.getIdentifier -> command.basedOn)
            )
            (context.copy(resolver = updatedResolver), freshName)
          }
        }
    }
  val methodBodyCapabilities: MethodBodyCapabilities =
    new MethodBodyCapabilities {
      import base._

      val canInstantiateObjectInMethod: Understands[MethodBodyContext, InstantiateObject[Type, Expression, ClassContext]] =
        new Understands[MethodBodyContext, InstantiateObject[Type, Expression, ClassContext]] {
          def perform(
            context: MethodBodyContext,
            command: InstantiateObject[Type, Expression, ClassContext]
          ): (MethodBodyContext, Expression) = {
            val (tpe, args) = context.resolver.instantiationOverride(command.tpe, command.constructorArguments)
            /** Expand with instantiated body (if it exists). */
            val result = new ObjectCreationExpr()
            result.setType(tpe.asClassOrInterfaceType().clone())
            result.setArguments(new NodeList(args : _*))
            if (command.body.isDefined) {
              val ci = new ClassOrInterfaceDeclaration()
              val (newCtxt, classDef) = Command.runGenerator(command.body.get, ClassCtxt(context.resolver, ci, context.extraImports))
              result.setAnonymousClassBody(newCtxt.cls.getMembers)
              (context.copy(resolver = newCtxt.resolver, extraImports = newCtxt.extraImports), result)
            } else {
              (context, result)
            }
          }
        }

      implicit val canCastInMethod: Understands[MethodBodyContext, CastObject[Type, Expression]] =
        new Understands[MethodBodyContext, CastObject[Type, Expression]] {
          def perform(
            context: MethodBodyContext,
            command: CastObject[Type, Expression]
          ): (MethodBodyContext, Expression) = {
            (context, new EnclosedExpr(new CastExpr(command.tpe.clone(), command.expr.clone())))
          }
        }

      implicit val canInstanceOfTypeInMethod: Understands[MethodBodyContext, InstanceOfType[Type, Expression]] =
        new Understands[MethodBodyContext, InstanceOfType[Type, Expression]] {
          def perform(
                       context: MethodBodyContext,
                       command: InstanceOfType[Type, Expression]
                     ): (MethodBodyContext, Expression) = {
            (context, new InstanceOfExpr(command.expr.clone(), command.tpe.asClassOrInterfaceType().clone()))
          }
        }

      val canGetMemberInMethod: Understands[MethodBodyContext, GetMember[Expression, Name]] =
        new Understands[MethodBodyContext, GetMember[Expression, Name]] {
          def perform(
            context: MethodBodyContext,
            command: GetMember[Expression, Name]
          ): (MethodBodyContext, Expression) = {
            (context, new FieldAccessExpr(command.instance.clone(), command.member.toAST.getIdentifier))
          }
        }
      val canSetAbstractInMethod: Understands[MethodBodyContext, SetAbstract] =
        new Understands[MethodBodyContext, SetAbstract] {
          def perform(
            context: MethodBodyContext,
            command: SetAbstract
          ): (MethodBodyContext, Unit) = {
            val newMethod = context.method.clone()
            newMethod.removeBody()
            newMethod.setAbstract(true)
            (context.copy(method = newMethod), ())
          }
        }

      val canSetStaticInMethod: Understands[MethodBodyContext, SetStatic] =
        new Understands[MethodBodyContext, SetStatic] {
          def perform(
            context: MethodBodyContext,
            command: SetStatic
          ): (MethodBodyContext, Unit) = {
            val newMethod = context.method.clone()
            newMethod.setStatic(true)
            newMethod.setAbstract(false)
            (context.copy(method = newMethod), ())
          }
        }

      val canSetOverrideInMethod: Understands[MethodBodyContext, SetOverride] =
        new Understands[MethodBodyContext, SetOverride] {
          def perform(
            context: MethodBodyContext,
            command: SetOverride
          ): (MethodBodyContext, Unit) = {
            val newMethod = context.method.clone()
            newMethod.addAnnotation("Override")
            (context.copy(method = newMethod), ())
          }
        }

      val canSelfReferenceInMethod: Understands[MethodBodyContext, SelfReference[Expression]] =
        new Understands[MethodBodyContext, SelfReference[Expression]] {
          def perform(
            context: MethodBodyContext,
            command: SelfReference[Expression]
          ): (MethodBodyContext, Expression) = {
            (context, new com.github.javaparser.ast.expr.ThisExpr())
          }
        }

      // if SuperReference has a base class, then use it, otherwise make standalone
      val canSuperReferenceInMethod: Understands[MethodBodyContext, SuperReference[Name,Expression]] =
        new Understands[MethodBodyContext, SuperReference[Name,Expression]] {
          def perform(
                       context: MethodBodyContext,
                       command: SuperReference[Name,Expression]
                     ): (MethodBodyContext, Expression) = {
            val start = new ClassOrInterfaceType()
            val fullName = ObjectOriented.components(config.targetPackage.getName).map(JavaNameProvider.mangle) ++ command.qualifiedName
            start.setName(fullName.head.toAST)
            val qualifiedName = fullName.tail.foldLeft(start){ case (scopes, suffix) =>
              new ClassOrInterfaceType(scopes, suffix.mangled)}

            //(context, qualifiedName)
            val parentName = new com.github.javaparser.ast.expr.Name(qualifiedName.toString)

            (context, new com.github.javaparser.ast.expr.SuperExpr(parentName))
          }
        }
      val canGetConstructorInMethod: Understands[MethodBodyContext, GetConstructor[Type, Expression]] =
        new Understands[MethodBodyContext, GetConstructor[Type, Expression]] {
          def perform(
            context: MethodBodyContext,
            command: GetConstructor[Type, Expression]
          ): (MethodBodyContext, Expression) = {
            (context, new TypeExpr(command.tpe.clone()))
          }
        }
      val canFindClassInMethod: Understands[MethodBodyContext, FindClass[Name, Type]] =
        new Understands[MethodBodyContext, FindClass[Name, Type]] {
          def perform(
            context: MethodBodyContext,
            command: FindClass[Name, Type]
          ): (MethodBodyContext, Type) = {
            val start = new ClassOrInterfaceType()
            val fullName = ObjectOriented.components(config.targetPackage.getName).map(JavaNameProvider.mangle) ++ command.qualifiedName
            start.setName(fullName.head.toAST)
            val qualifiedName = fullName.tail.foldLeft(start){ case (scopes, suffix) =>
              new ClassOrInterfaceType(scopes, suffix.mangled)}

            (context, qualifiedName)
          }
        }
    }
  val projectCapabilities: ProjectCapabilities =
    new ProjectCapabilities {
      import base._
      implicit val canAddTypeLookupForClassesInProject: Understands[ProjectContext, AddTypeLookup[ClassContext, Type]] =
        new Understands[ProjectContext, AddTypeLookup[ClassContext, Type]] {
          def perform(
            context: ProjectContext,
            command: AddTypeLookup[ClassContext, Type]
          ): (ProjectContext, Unit) = {
            def newLookup(k: ContextSpecificResolver)(tpe: TypeRep): Generator[ClassContext, Type] =
              if (tpe == command.tpe) {
                command.lookup
              } else {
                context.resolver._classTypeResolution(k)(tpe)
              }
            (context.copy(resolver = context.resolver.copy(_classTypeResolution = newLookup)), ())
          }
        }
      implicit val canAddTypeLookupForConstructorsInProject: Understands[ProjectContext, AddTypeLookup[ConstructorContext, Type]] =
        new Understands[ProjectContext, AddTypeLookup[ConstructorContext, Type]] {
          def perform(
            context: ProjectContext,
            command: AddTypeLookup[ConstructorContext, Type]
          ): (ProjectContext, Unit) = {
            def newLookup(k: ContextSpecificResolver)(tpe: TypeRep): Generator[ConstructorContext, Type] =
              if (tpe == command.tpe) {
                command.lookup
              } else {
                context.resolver._constructorTypeResolution(k)(tpe)
              }
            (context.copy(resolver = context.resolver.copy(_constructorTypeResolution = newLookup)), ())
          }
        }
    }
  val testCapabilities: TestCapabilities =
    new TestCapabilities {
      implicit val canAddMethodInTest: Understands[TestCtxt, AddMethod[base.MethodBodyContext, Name, Option[Expression]]] =
        new Understands[TestCtxt, AddMethod[base.MethodBodyContext, Name, Option[Expression]]] {
          def perform(
            context: TestCtxt,
            command: AddMethod[base.MethodBodyContext, Name, Option[Expression]]
          ): (TestCtxt, Unit) = {
            val methodToAdd = new MethodDeclaration()
            methodToAdd.setName(command.name.toAST)
            methodToAdd.setPublic(command.isPublic)
            val (methodCtxt, returnExp) =
              Command.runGenerator(
                command.spec,
                MethodBodyCtxt(context.resolver, context.extraImports, methodToAdd)
              )
            val resultingMethod = methodCtxt.method.clone()
            if (returnExp.isDefined) {
              val body = resultingMethod.getBody.orElseGet(() => new BlockStmt())
              body.addStatement(new ReturnStmt(returnExp.get))
              resultingMethod.setBody(body)
            }

            val resultCls = context.testClass.clone()
            resultCls.addMember(resultingMethod)
            val newImports = (context.extraImports ++ methodCtxt.extraImports).distinct.map(_.clone())
            (context.copy(resolver = methodCtxt.resolver, extraImports = newImports, testClass = resultCls), ())
          }
        }

      implicit val canAddFieldInTest: Understands[TestCtxt, AddField[Name, Type, Expression]] =
        new Understands[TestCtxt, AddField[Name, Type, Expression]] {
          def perform(
                       context: TestCtxt,
                       command: AddField[Name, Type, Expression]
                     ): (TestCtxt, Unit) = {
            val resultCls = context.testClass.clone()
            val expr = new ObjectCreationExpr()
            val (tpe, _) = context.resolver.instantiationOverride(command.tpe, Seq.empty)
            expr.setType(tpe.asClassOrInterfaceType().clone())

            val modifiers: Seq[Modifier.Keyword] =
              (if (command.isVisibleToSubclasses) Seq(Modifier.protectedModifier().getKeyword) else Seq(Modifier.privateModifier().getKeyword)) ++ (if (command.isMutable) Seq.empty else Seq(Modifier.finalModifier().getKeyword))

            if (command.initializer.isDefined) {
              resultCls.addFieldWithInitializer(command.tpe, command.name.toAST.toString, expr, modifiers: _*)
            } else {
              resultCls.addField(command.tpe, command.name.toAST.toString, modifiers: _*)
            }

            (context.copy(testClass = resultCls), ())
          }
        }

      implicit val canAddBlockDefinitionsInTest: Understands[TestCtxt, AddBlockDefinitions[Statement]] =
        new Understands[TestCtxt, AddBlockDefinitions[Statement]] {
          def perform(
                       context: TestCtxt,
                       command: AddBlockDefinitions[Statement]
                     ): (TestCtxt, Unit) = {
            val resultCls = context.testClass.clone()
            if (!resultCls.getDefaultConstructor.isPresent) {  // add or create default constructor
              resultCls.addConstructor()
            }
            val body = resultCls.getDefaultConstructor.get.getBody
            command.definitions.foreach(stmt => body.addStatement(stmt))
            (context.copy(testClass = resultCls), ())
          }
        }

      implicit val canInitializeFieldInTest: Understands[TestCtxt, InitializeField[Name, Expression]] =
        new Understands[TestCtxt, InitializeField[Name, Expression]] {
          def perform(
                       context: TestCtxt,
                       command: InitializeField[Name, Expression]
                     ): (TestCtxt, Unit) = {
            val assignee = new FieldAccessExpr(new ThisExpr(), command.name.toAST.getIdentifier)
            val stmt = new ExpressionStmt(new AssignExpr(assignee, command.value, AssignExpr.Operator.ASSIGN))
            Command.runGenerator(
              addBlockDefinitions(Seq(stmt)),
              context
            )
          }
        }

      implicit val canInstantiateObjectInTest: Understands[TestCtxt, InstantiateObject[Type, Expression, TestCtxt]] =
        new Understands[TestCtxt, InstantiateObject[Type, Expression, TestCtxt]] {
          def perform(
                       context: TestCtxt,
                       command: InstantiateObject[Type, Expression, TestCtxt]
                     ): (TestCtxt, Expression) = {
            val (tpe, args) = context.resolver.instantiationOverride(command.tpe, command.constructorArguments)
            /** REMOVED [Expand with instantiated body (if it exists).] */
            val result = new ObjectCreationExpr()
            result.setType(tpe.asClassOrInterfaceType().clone())
            result.setArguments(new NodeList(args : _*))
            (context, result)
          }
        }

      implicit val canAddConstructorInTest: Understands[TestCtxt, AddConstructor[ConstructorContext]] =
        new Understands[TestCtxt, AddConstructor[ConstructorContext]] {
          def perform(
                       context: TestCtxt,
                       command: AddConstructor[ConstructorContext]
                     ): (TestCtxt, Unit) = {
            val ctorToAdd = new ConstructorDeclaration()
            ctorToAdd.setPublic(true)
            ctorToAdd.setName(context.testClass.getName.clone)
            val (ctorCtxt, _) =
              Command.runGenerator(
                command.ctor,
                CtorCtxt(context.resolver, context.extraImports, ctorToAdd)
              )
            val resultCls = context.testClass.clone()
            resultCls.addMember(ctorCtxt.ctor.clone())
            val newImports = (context.extraImports ++ ctorCtxt.extraImports).distinct.map(_.clone())
            (context.copy(resolver = ctorCtxt.resolver, extraImports = newImports, testClass = resultCls), ())
          }
        }

      implicit val canAddImportInTest: Understands[TestCtxt, AddImport[Import]] =
        new Understands[TestCtxt, AddImport[Import]] {
          def perform(
                       context: TestCtxt,
                       command: AddImport[Import]
                     ): (TestCtxt, Unit) = {
            val newImports =
              if (context.extraImports.contains(command.imp)) {
                context.extraImports
              } else context.extraImports :+ command.imp.clone()
            (context.copy(extraImports = newImports), ())
          }
        }

      implicit val canResolveImportInTest: Understands[TestCtxt, ResolveImport[Import, Type]] =
        new Understands[TestCtxt, ResolveImport[Import, Type]] {
          def perform(
                       context: TestCtxt,
                       command: ResolveImport[Import, Type]
                     ): (TestCtxt, Option[Import]) = {
            val stripped = AnyParadigm.stripGenerics(command.forElem)
            Try { (context, context.resolver.importResolution(stripped)) } getOrElse {
              val newImport =
                new ImportDeclaration(
                  ObjectOriented.typeToName(stripped.asClassOrInterfaceType()),
                  false,
                  false)
              if (context.extraImports.contains(newImport)) {
                (context, None)
              } else {
                (context, Some(newImport))
              }
            }
          }
        }

      implicit val canFindClassInTest: Understands[TestCtxt, FindClass[Name, Type]] =
        new Understands[TestCtxt, FindClass[Name, Type]] {
          def perform(
                       context: TestCtxt,
                       command: FindClass[Name, Type]
                     ): (TestCtxt, Type) = {
            val start = new ClassOrInterfaceType()
            val fullName = ObjectOriented.components(config.targetPackage.getName).map(JavaNameProvider.mangle) ++ command.qualifiedName
            start.setName(fullName.head.toAST)
            val qualifiedName = fullName.tail.foldLeft(start){ case (scopes, suffix) =>
              new ClassOrInterfaceType(scopes, suffix.mangled)
            }
            (context, qualifiedName)
          }
        }

      implicit val canAddImplementedInTest: Understands[TestCtxt, AddImplemented[Type]] =
        new Understands[TestCtxt, AddImplemented[Type]] {
          def perform(
                       context: TestCtxt,
                       command: AddImplemented[Type]
                     ): (TestCtxt, Unit) = {
            val resultCls = context.testClass.clone()
            resultCls.addImplementedType(command.interface.asClassOrInterfaceType())
            (context.copy(testClass = resultCls), ())
          }
        }
    }
}

object ObjectOriented {
  def components(name: JName): Seq[String] = {
    val rest: Seq[String] =
      name.getQualifier
      .map[Seq[String]](components)
      .orElse(Seq.empty[String])
    name.getIdentifier +: rest
  }

  def fromComponents(first: String, rest: String*): JName = {
    rest.foldLeft(new JName(first))((result, next) => new JName(result, next))
  }

  def nameToExpression(name: JName): Expression = {
    name.getQualifier
      .map[Expression](n => nameToExpression(n))
      .map[Expression](qual => new FieldAccessExpr(qual, name.getIdentifier))
      .orElseGet(() => new NameExpr(name.getIdentifier))
  }

  def nameToType(name: JName): ClassOrInterfaceType = {
    name.getQualifier
      .map[ClassOrInterfaceType](n => nameToType(n))
      .map[ClassOrInterfaceType](qual => new ClassOrInterfaceType(qual, name.getIdentifier))
      .orElseGet(() => new ClassOrInterfaceType(null, name.getIdentifier))
  }

  def typeToName(tpe: ClassOrInterfaceType): JName = {
    tpe.getScope
      .map[JName](s => typeToName(s))
      .map[JName](qual => new JName(qual, tpe.getName.getIdentifier))
      .orElseGet(() => new JName(tpe.getName.getIdentifier))
  }

  def apply[AP <: AnyParadigm](base: AnyParadigm): ObjectOriented[base.type] = {
    val b: base.type = base
    new ObjectOriented[b.type] {
      val base: b.type = b
    }
  }
}