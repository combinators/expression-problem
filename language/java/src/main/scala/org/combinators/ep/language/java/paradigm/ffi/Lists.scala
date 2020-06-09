package org.combinators.ep.language.java.paradigm.ffi    /*DI:LD:AI*/

import com.github.javaparser.ast.expr.{BinaryExpr, MethodCallExpr, StringLiteralExpr}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.{AddImport, Apply, GetMember}
import org.combinators.ep.generator.paradigm.ffi.{Lists => Lsts, _}
import org.combinators.ep.language.java.CodeGenerator.Enable
import org.combinators.ep.language.java.{CodeGenerator, ContextSpecificResolver, CtorCtxt, JavaNameProvider, MethodBodyCtxt, ProjectCtxt, Syntax}
import org.combinators.templating.twirl.Java
import org.combinators.ep.language.java.paradigm.{AnyParadigm, Generics}
import org.combinators.ep.language.java.Syntax.default._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._
import cats.syntax._
import cats.implicits._

trait Lists[Ctxt, AP <: AnyParadigm] extends Lsts[Ctxt] {
  val base: AP
  val applyType: Understands[Ctxt, Apply[Type, Type, Type]]
  val addImport: Understands[Ctxt, AddImport[Import]]
  val generics: Generics[base.type]

  def listCreation[Ctxt](canAddImport: Understands[Ctxt, AddImport[Import]]): Understands[Ctxt, Apply[Create[Type], Expression, Expression]] =
    new Understands[Ctxt, Apply[Create[Type], Expression, Expression]] {
      def perform(
        context: Ctxt,
        command: Apply[Create[Type], Expression, Expression]
      ): (Ctxt, Expression) = {
        val gen: Generator[Ctxt, Expression] =
          if (command.arguments.isEmpty) {
            for {
              _ <- AddImport[Import](Java("import java.util.Collections;").importDeclaration()).interpret(canAddImport)
            } yield Java(s"java.util.Collections.emptyList()").expression[Expression]()
          } else {
            for {
              _ <- AddImport[Import](Java("import java.util.Arrays;").importDeclaration()).interpret(canAddImport)
            } yield Java(s"java.util.Arrays.asList(${command.arguments.mkString(", ")})").expression[Expression]()
          }
        Command.runGenerator[Ctxt, Expression](gen, context)
      }
    }

  val listCapabilities: ListCapabilities =
    new ListCapabilities {

      implicit val canCreate: Understands[Ctxt, Apply[Create[Type], Expression, Expression]] =
        listCreation(addImport)

      implicit val canCons: Understands[Ctxt, Apply[Cons, Expression, Expression]] =
        new Understands[Ctxt, Apply[Cons, Expression, Expression]] {
          def perform(
            context: Ctxt,
            command: Apply[Cons, Expression, Expression]
          ): (Ctxt, Expression) = {
            val gen: Generator[Ctxt, Expression] =
              for {
                _ <- AddImport[Import](Java("import java.util.stream.Stream;").importDeclaration()).interpret(addImport)
                _ <- AddImport[Import](Java("import java.util.stream.Collectors;").importDeclaration()).interpret(addImport)
              } yield Java(s"java.util.stream.Stream.concat(java.util.stream.Stream.of(${command.arguments(0)}), ${command.arguments(1)}.stream()).collect(java.util.stream.Collectors.toList())").expression[Expression]()
            Command.runGenerator(gen, context)
          }
        }

      implicit val canHead: Understands[Ctxt, Apply[Head, Expression, Expression]] =
        new Understands[Ctxt, Apply[Head, Expression, Expression]] {
          override def perform(
            context: Ctxt,
            command: Apply[Head, Expression, Expression]
          ): (Ctxt, Expression) = {
            (context, Java(s"${command.arguments(0)}.get(0)").expression[Expression]())
          }
        }

      implicit val canTail: Understands[Ctxt, Apply[Tail, Expression, Expression]] =
        new Understands[Ctxt, Apply[Tail, Expression, Expression]] {
          override def perform(
            context: Ctxt,
            command: Apply[Tail, Expression, Expression]
          ): (Ctxt, Expression) = {
            (context, Java(s"${command.arguments(0)}.subList(1, ${command.arguments(0)}.size())").expression[Expression]())
          }
        }

      implicit val canAppend: Understands[Ctxt, Apply[Append, Expression, Expression]] =
        new Understands[Ctxt, Apply[Append, Expression, Expression]] {
          def perform(
            context: Ctxt,
            command: Apply[Append, Expression, Expression]
          ): (Ctxt, Expression) = {
            val gen: Generator[Ctxt, Expression] =
              for {
                _ <- AddImport[Import](Java("import java.util.stream.Stream;").importDeclaration()).interpret(addImport)
                _ <- AddImport[Import](Java("import java.util.stream.Collectors;").importDeclaration()).interpret(addImport)
              } yield Java(s"java.util.stream.Stream.concat(${command.arguments(0)}.stream(), ${command.arguments(1)}.stream()).collect(java.util.stream.Collectors.toList())").expression[Expression]()
            Command.runGenerator(gen, context)
          }
        }
    }

  def enable(): Generator[base.ProjectContext, Unit] =
    Enable.interpret(new Understands[base.ProjectContext, Enable.type] {
      def perform(
        context: ProjectCtxt,
        command: Enable.type
      ): (ProjectCtxt, Unit) = {
        val listType = Java("java.util.List").tpe()

        def updateResolver(resolver: ContextSpecificResolver): ContextSpecificResolver = {
          def addResolutionType[Ctxt](
            toResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type],
            projectResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type],
            canApplyType: Understands[Ctxt, Apply[Type, Type, Type]]
          ): ContextSpecificResolver => TypeRep => Generator[Ctxt, Type] = k => {
            case TypeRep.Sequence(elemRep) =>
              for {
                elemType <- projectResolution(k)(elemRep)
                resultType <- Apply[Type, Type, Type](listType, Seq(elemType)).interpret(canApplyType)
              } yield resultType
            case other => toResolution(k)(other)
          }

          def addReification[Ctxt](
            reify: ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression],
            projectResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type],
            projectReiification: ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression],
            canCreateList: Understands[Ctxt, Apply[Create[Type], Expression, Expression]]
          ): ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression] =
          k => rep => rep.tpe match {
            case TypeRep.Sequence(elemTypeRep) =>
              for {
                elems <- forEach (rep.inst.asInstanceOf[Seq[elemTypeRep.HostType]]) { elem =>
                  projectReiification(k)(InstanceRep(elemTypeRep)(elem))
                }
                elemType <- projectResolution(k)(elemTypeRep)
                res <- Apply[Create[Type], Expression, Expression](Create(elemType), elems).interpret(canCreateList)
              } yield res
            case _ => reify(k)(rep)
          }

          def addExtraImport(
            importResolution: ContextSpecificResolver => Type => Option[Import]
          ): ContextSpecificResolver => Type => Option[Import] = k => {
            case tpe
              if tpe
                .toClassOrInterfaceType
                .map[Boolean](clsTy => clsTy.getName == listType.asClassOrInterfaceType().getName)  // WARNING: might need to be asString
                .orElse(false) =>
              Some(Java("import java.util.List;").importDeclaration())
            case other => importResolution(k)(other)
          }

          resolver.copy(
            _methodTypeResolution =
              addResolutionType(
                resolver._methodTypeResolution,
                _.methodTypeResolution,
                generics.ppolyParadigm.methodBodyCapabilities.canApplyTypeInMethod
              ),
            _constructorTypeResolution =
              addResolutionType(
                resolver._constructorTypeResolution,
                _.constructorTypeResolution,
                generics.constructorCapabilities.canApplyTypeInConstructor
              ),
            _classTypeResolution =
              addResolutionType(
                resolver._classTypeResolution,
                _.classTypeResolution,
                generics.classCapabilities.canApplyTypeInClass
              ),
            _reificationInConstructor =
              addReification(
                resolver._reificationInConstructor,
                _.constructorTypeResolution,
                _.reificationInConstructor,
                listCreation(generics.ooParadigm.constructorCapabilities.canAddImportInConstructor)
              ),
            _reificationInMethod =
              addReification(
                resolver._reificationInMethod,
                _.methodTypeResolution,
                _.reificationInMethod,
                listCreation(base.methodBodyCapabilities.canAddImportInMethodBody)
              ),
            _importResolution = addExtraImport(resolver._importResolution)
          )
        }


        (context.copy(resolver = updateResolver(context.resolver)), ())
      }
    })
}

object Lists {
  type Aux[Ctxt, AP <: AnyParadigm, Gen <: Generics[AP]] = Lists[Ctxt, AP] {
    val generics: Gen
  }
  def apply[Ctxt, AP <: AnyParadigm, Gen <: Generics[AP]](
    base: AP,
    getMember: Understands[Ctxt, GetMember[Expression, Name]],
    applyMethod: Understands[Ctxt, Apply[Expression, Expression, Expression]],
    applyType: Understands[Ctxt, Apply[Type, Type, Type]],
    addImport: Understands[Ctxt, AddImport[Import]])(
    generics: Generics[base.type]
  ): Aux[Ctxt, base.type, generics.type] = {
    val b: base.type = base
    val gm = getMember
    val appMeth = applyMethod
    val appTy = applyType
    val addImp = addImport
    val gen: generics.type = generics

    new Lists[Ctxt, b.type] {
      lazy val base: b.type = b
      lazy val getMember = gm
      lazy val applyMethod = appMeth
      lazy val applyType = appTy
      lazy val addImport = addImp
      lazy val generics: gen.type = gen
    }
  }
}