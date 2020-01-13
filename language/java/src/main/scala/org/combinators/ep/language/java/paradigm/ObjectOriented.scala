package org.combinators.ep.language.java.paradigm

import java.util.UUID

import com.github.javaparser.ast.{ImportDeclaration, Modifier}
import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, ConstructorDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.NameExpr
import com.github.javaparser.ast.stmt.{BlockStmt, ExplicitConstructorInvocationStmt, ReturnStmt}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.{ObjectOriented => OO, AnyParadigm => _, _}
import org.combinators.ep.language.java.Syntax.MangledName
import org.combinators.ep.language.java.{ClassCtxt, ContextSpecificResolver, CtorCtxt, JavaNameProvider, MethodBodyCtxt, TestCtxt}
import org.combinators.templating.twirl.Java

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
      implicit val canAddFieldInClass: Understands[ClassContext, AddField[Name, Type]] =
        new Understands[ClassContext, AddField[Name, Type]] {
          def perform(
            context: ClassContext,
            command: AddField[Name, Type]
          ): (ClassContext, Unit) = {
            val resultCls = context.cls.clone()
            resultCls.addField(command.tpe, command.name.toAST.toString, Modifier.privateModifier().getKeyword)
            (context.copy(cls = resultCls), ())
          }
        }

      implicit val canGetFieldInClass: Understands[ClassContext, GetField[Name,Expression]] =
        new Understands[ClassContext, GetField[Name, Expression]] {
          def perform(
            context: ClassContext,
            command: GetField[Name,Expression]
          ): (ClassContext, Expression) =
            (context,  Java(s"""${command.name.mangled}""").expression())
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
            Try { (context, context.resolver.importResolution(command.forElem)) } getOrElse {
              val newImport =
                new ImportDeclaration(
                  new com.github.javaparser.ast.expr.Name(config.targetPackage.getName.clone(), command.forElem.toString()),
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
            start.setName(command.qualifiedName.head.toAST)
            val qualifiedName = command.qualifiedName.tail.foldLeft(start){case (scopes, suffix) =>
              new ClassOrInterfaceType(scopes, suffix.mangled)}

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
      implicit val canInitializeParentInConstructor: Understands[ConstructorContext, InitializeParent[Expression]] =
        new Understands[ConstructorContext, InitializeParent[Expression]] {
          def perform(
            context: ConstructorContext,
            command: InitializeParent[Expression]
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
            (context.copy(ctor = newCtor), ())
          }
        }
      implicit val canInitializeFieldInConstructor: Understands[ConstructorContext, InitializeField[Name, Expression]] =
        new Understands[ConstructorContext, InitializeField[Name, Expression]] {
          def perform(
            context: ConstructorContext,
            command: InitializeField[Name, Expression]
          ): (ConstructorContext, Unit) = {
            Command.runGenerator(
              addBlockDefinitions(Java(s"this.${command.name} = ${command.value};").statements()),
              context)
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
            Try { (context, context.resolver.importResolution(command.forElem)) } getOrElse {
              val newImport =
                new ImportDeclaration(
                  new com.github.javaparser.ast.expr.Name(config.targetPackage.getName.clone(), command.forElem.toString()),
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
      implicit val canInstantiateObjectInConstructor: Understands[ConstructorContext, InstantiateObject[Type, Expression]] =
        new Understands[ConstructorContext, InstantiateObject[Type, Expression]] {
          def perform(
            context: ConstructorContext,
            command: InstantiateObject[Type, Expression]
          ): (ConstructorContext, Expression) = {
            val (tpe, args) = context.resolver.instantiationOverride(command.tpe, command.constructorArguments)
            (context, Java(s"""new ${tpe}(${args.mkString(", ")})""").expression())
          }
        }
      implicit val canApplyInConstructor: Understands[ConstructorContext, Apply[Expression, Expression, Expression]] =
        new Understands[ConstructorContext, Apply[Expression, Expression, Expression]] {
          def perform(
            context: ConstructorContext,
            command: Apply[Expression, Expression, Expression]
          ): (ConstructorContext, Expression) = {
            (context, Java(s"${command.functional}(${command.arguments.mkString(", ")})").expression())
          }
        }

      implicit val canCastInConstructor: Understands[ConstructorContext, CastObject[Type, Expression]] =
        new Understands[ConstructorContext, CastObject[Type, Expression]] {
          def perform(
            context: ConstructorContext,
            command: CastObject[Type, Expression]
          ): (ConstructorContext, Expression) = {
            (context, Java(s"""((${command.tpe})${command.expr})""").expression())
          }
        }

      implicit val canGetMemberInConstructor: Understands[ConstructorContext, GetMember[Expression, Name]] =
        new Understands[ConstructorContext, GetMember[Expression, Name]] {
          def perform(
            context: ConstructorContext,
            command: GetMember[Expression, Name]
          ): (ConstructorContext, Expression) = {
            (context, Java(s"""${command.instance}.${command.member}""").expression())
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
            (context, Java(command.tpe).expression())
          }
        }
      implicit val canFindClassInConstructor: Understands[ConstructorContext, FindClass[Name, Type]] =
        new Understands[ConstructorContext, FindClass[Name, Type]] {
          def perform(
            context: ConstructorContext,
            command: FindClass[Name, Type]
          ): (ConstructorContext, Type) = {
//            val result = new ClassOrInterfaceType()
//            result.setName(command.name.toAST)
//            (context, result)
            val start = new ClassOrInterfaceType()
            start.setName(command.qualifiedName.head.toAST)
            val qualifiedName = command.qualifiedName.tail.foldLeft(start){case (scopes, suffix) =>
              new ClassOrInterfaceType(scopes, suffix.mangled)}

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
      val canInstantiateObjectInMethod: Understands[MethodBodyContext, InstantiateObject[Type, Expression]] =
        new Understands[MethodBodyContext, InstantiateObject[Type, Expression]] {
          def perform(
            context: MethodBodyContext,
            command: InstantiateObject[Type, Expression]
          ): (MethodBodyContext, Expression) = {
            val (tpe, args) = context.resolver.instantiationOverride(command.tpe, command.constructorArguments)
            (context, Java(s"""new ${tpe}(${args.mkString(", ")})""").expression())
          }
        }

      implicit val canCastInMethod: Understands[MethodBodyContext, CastObject[Type, Expression]] =
        new Understands[MethodBodyContext, CastObject[Type, Expression]] {
          def perform(
            context: MethodBodyContext,
            command: CastObject[Type, Expression]
          ): (MethodBodyContext, Expression) = {
            (context, Java(s"""((${command.tpe})${command.expr})""").expression())
          }
        }

      val canGetMemberInMethod: Understands[MethodBodyContext, GetMember[Expression, Name]] =
        new Understands[MethodBodyContext, GetMember[Expression, Name]] {
          def perform(
            context: MethodBodyContext,
            command: GetMember[Expression, Name]
          ): (MethodBodyContext, Expression) = {
            (context, Java(s"""${command.instance}.${command.member}""").expression())
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

      val canSelfReferenceInMethod: Understands[MethodBodyContext, SelfReference[Expression]] =
        new Understands[MethodBodyContext, SelfReference[Expression]] {
          def perform(
            context: MethodBodyContext,
            command: SelfReference[Expression]
          ): (MethodBodyContext, Expression) = {
            (context, new com.github.javaparser.ast.expr.ThisExpr())
          }
        }
      val canGetConstructorInMethod: Understands[MethodBodyContext, GetConstructor[Type, Expression]] =
        new Understands[MethodBodyContext, GetConstructor[Type, Expression]] {
          def perform(
            context: MethodBodyContext,
            command: GetConstructor[Type, Expression]
          ): (MethodBodyContext, Expression) = {
            (context, Java(command.tpe).expression())
          }
        }
      val canFindClassInMethod: Understands[MethodBodyContext, FindClass[Name, Type]] =
        new Understands[MethodBodyContext, FindClass[Name, Type]] {
          def perform(
            context: MethodBodyContext,
            command: FindClass[Name, Type]
          ): (MethodBodyContext, Type) = {
//            val result = new ClassOrInterfaceType()
//            result.setName(command.name.toAST)
//            (context, result)
            val start = new ClassOrInterfaceType()
            start.setName(command.qualifiedName.head.toAST)
            val qualifiedName = command.qualifiedName.tail.foldLeft(start){case (scopes, suffix) =>
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
    }
}

object ObjectOriented {
  def apply[AP <: AnyParadigm](base: AnyParadigm): ObjectOriented[base.type] = {
    val b: base.type = base
    new ObjectOriented[b.type] {
      val base: b.type = b
    }
  }
}