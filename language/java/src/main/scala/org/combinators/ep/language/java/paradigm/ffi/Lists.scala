package org.combinators.ep.language.java.paradigm.ffi

import com.github.javaparser.ast.expr.{BinaryExpr, StringLiteralExpr}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.{Apply, GetMember}
import org.combinators.ep.generator.paradigm.ffi.{Lists => Lsts, _}
import org.combinators.ep.language.java.CodeGenerator.Enable
import org.combinators.ep.language.java.{CodeGenerator, ContextSpecificResolver, JavaNameProvider, ProjectCtxt, Syntax}
import org.combinators.templating.twirl.Java
import org.combinators.ep.language.java.paradigm.AnyParadigm
import org.combinators.ep.language.java.Syntax.default._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._

/* Jan: I'm currently working on this.
class Lists[Ctxt, AP <: AnyParadigm](
  val base: AP,
  getMember: Understands[Ctxt, GetMember[Expression, Name]],
  applyMethod: Understands[Ctxt, Apply[Expression, Expression, Expression]],
  applyType: Understands[Ctxt, Apply[Type, Type, Type]]
) extends Lsts[Ctxt] {
  val listCapabilities: ListCapabilities =
    new ListCapabilities {

      implicit val canCreate: Understands[Ctxt, Apply[Create[Type], Expression, Expression]]

      implicit val canCons: Understands[Ctxt, Apply[Cons, Expression, Expression]]

      implicit val canHead: Understands[Ctxt, Apply[Head, Expression, Expression]]

      implicit val canTail: Understands[Ctxt, Apply[Tail, Expression, Expression]]

      implicit val canAppend: Understands[Ctxt, Apply[Append, Expression, Expression]]

      implicit val canGetStringLength: Understands[Ctxt, Apply[GetStringLength, Expression, Expression]] =
        new Understands[Ctxt, Apply[GetStringLength, Expression, Expression]] {
          def perform(
            context: Ctxt,
            command: Apply[GetStringLength, Expression, Expression]
          ): (Ctxt, Expression) = {
            implicit val _getMember = getMember
            implicit val _applyMethod = applyMethod
            val gen = for {
              lengthMethod <- GetMember[Expression, Name](command.arguments(0), JavaNameProvider.mangle("length")).interpret
              res <- Apply[Expression, Expression, Expression](lengthMethod, Seq.empty).interpret
            } yield res
            Command.runGenerator(gen, context)
          }
        }
      implicit val canAppend: Understands[Ctxt, Apply[StringAppend, Expression, Expression]] =
        new Understands[Ctxt, Apply[StringAppend, Expression, Expression]] {
          def perform(
            context: Ctxt,
            command: Apply[StringAppend, Expression, Expression]
          ): (Ctxt, Expression) = {
            (context, command.arguments.tail.foldLeft(command.arguments.head){ case (str, next) =>
              new BinaryExpr(str, next, BinaryExpr.Operator.PLUS)
            })
          }
        }
      implicit val canToStringInCtxt: Understands[Ctxt, Apply[ToString[Type], Expression, Expression]] =
        new Understands[Ctxt, Apply[ToString[Type], Expression, Expression]] {
          def perform(
            context: Ctxt,
            command: Apply[ToString[Type], Expression, Expression]
          ): (Ctxt, Expression) = {
            implicit val _getMember = getMember
            implicit val _applyMethod = applyMethod
            val gen = Command.lift[Ctxt, Expression](Java(s"String.valueOf(${command.arguments.head})").expression())
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
        val listType = Java("List").tpe()

        def updateResolver(contextSpecificResolver: ContextSpecificResolver): ContextSpecificResolver = {
          def addResolutionType[Ctxt](
            toResolution: (TypeRep => Generator[Ctxt, Type]) => TypeRep => Generator[Ctxt, Type]
          ): (TypeRep => Generator[Ctxt, Type]) => TypeRep => Generator[Ctxt, Type] = k => {
            case TypeRep.Sequence(elemRep) =>
              for {
                elemType <- k(elemRep)
                resultType <- Apply(listType, Seq(elemType)).interpret(applyType)
              } yield resultType
            case other => toResolution(k)(other)
          }

          def addReification[Ctxt](
            reify: (InstanceRep => Generator[Ctxt, Expression]) => InstanceRep => Generator[Ctxt, Expression]
          ): (InstanceRep => Generator[Ctxt, Expression]) => InstanceRep => Generator[Ctxt, Expression] =
          k => rep => rep.tpe match {
            case TypeRep.Sequence(elemRep) =>
              for {
                elemType <-
                elems <- forEach (rep.inst.asInstanceOf[Seq[elemRep.HostType]]) { elem =>
                  k(InstanceRep(elemTpe)(elem))
                }
                res <- listCapabilities.create()
              }
            case _ => reify(k)(other)
          }
          {
            case instRep if instRep.tpe == rep =>
              Command.lift(reification(instRep.inst.asInstanceOf[rep.HostType]))
            case other => reify(other)
          }

          def addExtraImport(
            importResolution: Type => Option[Import]
          ): Type => Option[Import] = {
            case r if r == translateTo || r == possiblyBoxedTargetType(true) => extraImport
            case other => importResolution(other)
          }

          resolver.copy(
            methodTypeResolution =
              addResolutionType(
                possiblyBoxedTargetType(config.boxLevel.inMethods),
                resolver.methodTypeResolution
              ),
            constructorTypeResolution =
              addResolutionType(
                possiblyBoxedTargetType(config.boxLevel.inConstructors),
                resolver.constructorTypeResolution
              ),
            classTypeResolution =
              addResolutionType(
                possiblyBoxedTargetType(config.boxLevel.inClasses),
                resolver.classTypeResolution
              ),
            reificationInConstructor = addReification(resolver.reificationInConstructor),
            reificationInMethod = addReification(resolver.reificationInMethod),
            importResolution = addExtraImport(resolver.importResolution)
          )



        }


        val resolverUpdate =
          ContextSpecificResolver.updateResolver(base.config, TypeRep.String, Java("String").tpe())(new StringLiteralExpr(_))
        (context.copy(resolver = resolverUpdate(context.resolver)), ())
      }
    })
}
*/