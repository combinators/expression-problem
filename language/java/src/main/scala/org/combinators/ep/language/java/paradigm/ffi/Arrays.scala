package org.combinators.ep.language.java.paradigm.ffi    /*DI:LD:AI*/

import com.github.javaparser.ast.`type`.ArrayType
import com.github.javaparser.ast.expr.{ArrayAccessExpr, ArrayCreationExpr, ArrayInitializerExpr, AssignExpr, FieldAccessExpr, IntegerLiteralExpr, MethodCallExpr, NameExpr, SimpleName}
import com.github.javaparser.ast.{ArrayCreationLevel, NodeList}
import org.combinators.cogen.InstanceRep
import org.combinators.cogen.TypeRep
import org.combinators.cogen.paradigm.{Apply, ffi}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.AnyParadigm.syntax._
import org.combinators.cogen.paradigm.ffi.{CreateArray, Get, Length, Arrays as Arrs}
import org.combinators.cogen.Understands
import org.combinators.ep.language.java.CodeGenerator.Enable
import org.combinators.ep.language.java.Syntax.default._
import org.combinators.ep.language.java.paradigm.AnyParadigm
import org.combinators.ep.language.java.{ContextSpecificResolver, ProjectCtxt}

class Arrays[Ctxt, AP <: AnyParadigm](val base:AP) extends Arrs[Ctxt] {
  case object ArraysEnabled

  def arrayCreation[Ctxt](): Understands[Ctxt, Apply[CreateArray[Type], Expression, Expression]] =
    new Understands[Ctxt, Apply[CreateArray[Type], Expression, Expression]] {
      def perform(
                   context: Ctxt,
                   command: Apply[CreateArray[Type], Expression, Expression]
                 ): (Ctxt, Expression) =
        (context,
          new ArrayCreationExpr(command.functional.elementType,
            new NodeList(new ArrayCreationLevel(1)),
            new ArrayInitializerExpr(new NodeList(command.arguments*)))
        )
    }

  val arrayCapabilities: ArrayCapabilities =
    new ArrayCapabilities {

      implicit val canCreate: Understands[Ctxt, Apply[CreateArray[Type], Expression, Expression]] = arrayCreation()

      implicit val canGet: Understands[Ctxt, Apply[Get, Expression, Expression]] =
        new Understands[Ctxt, Apply[Get, Expression, Expression]] {
          override def perform(
            context: Ctxt,
            command: Apply[Get, Expression, Expression]
          ): (Ctxt, Expression) = {
            (context, new ArrayAccessExpr(command.arguments(0), command.arguments(1)))
          }
        }

      implicit val canSet: Understands[Ctxt, Apply[ffi.Set, Expression, Expression]] =
        new Understands[Ctxt, Apply[ffi.Set, Expression, Expression]] {
          override def perform(
                                context: Ctxt,
                                command: Apply[ffi.Set, Expression, Expression]
                              ): (Ctxt, Expression) = {
            (context, new AssignExpr(new ArrayAccessExpr(command.arguments(0), command.arguments(1)), command.arguments(2), AssignExpr.Operator.ASSIGN))
          }
        }

      implicit val canLength: Understands[Ctxt, Apply[Length, Expression, Expression]] =
        new Understands[Ctxt, Apply[Length, Expression, Expression]] {
          def perform(
            context: Ctxt,
            command: Apply[Length, Expression, Expression]
          ): (Ctxt, Expression) = {
            (context, new FieldAccessExpr(command.arguments(0), "length"))
          }
        }
    }

  def enable(): Generator[base.ProjectContext, Unit] =
    Enable.interpret(new Understands[base.ProjectContext, Enable.type] {
      def perform(
        context: ProjectCtxt,
        command: Enable.type
      ): (ProjectCtxt, Unit) = {
        if (!context.resolver.resolverInfo.contains(ArraysEnabled)) {

          def updateResolver(resolver: ContextSpecificResolver): ContextSpecificResolver = {
            def addResolutionType[Ctxt](
              toResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type],
              projectResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type]
            ): ContextSpecificResolver => TypeRep => Generator[Ctxt, Type] = k => {
              case TypeRep.Array(elemRep) =>
                for {
                  elemType <- projectResolution(k)(elemRep)
                } yield new ArrayType(elemType)
              case other => toResolution(k)(other)
            }

            def addReification[Ctxt](
              reify: ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression],
              projectResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type],
              projectReification: ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression],
              canCreateArray: Understands[Ctxt, Apply[CreateArray[Type], Expression, Expression]]
            ): ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression] =
              k => rep => rep.tpe match {
                case TypeRep.Array(elemTypeRep) =>
                  for {
                    elems <- forEach(rep.inst.asInstanceOf[Seq[elemTypeRep.HostType]]) { elem =>
                      projectReification(k)(InstanceRep(elemTypeRep)(elem))
                    }
                    elemType <- projectResolution(k)(elemTypeRep)
                    res <- Apply[CreateArray[Type], Expression, Expression](CreateArray(elemType), elems).interpret(canCreateArray)
                  } yield res
                case _ => reify(k)(rep)
              }

            resolver.copy(
              _methodTypeResolution =
                addResolutionType(
                  resolver._methodTypeResolution,
                  _.methodTypeResolution
                ),
              _constructorTypeResolution =
                addResolutionType(
                  resolver._constructorTypeResolution,
                  _.constructorTypeResolution
                ),
              _classTypeResolution =
                addResolutionType(
                  resolver._classTypeResolution,
                  _.classTypeResolution
                ),
              _reificationInConstructor =
                addReification(
                  resolver._reificationInConstructor,
                  _.constructorTypeResolution,
                  _.reificationInConstructor,
                  arrayCreation()
                ),
              _reificationInMethod =
                addReification(
                  resolver._reificationInMethod,
                  _.methodTypeResolution,
                  _.reificationInMethod,
                  arrayCreation()
                )
            ).addInfo(ArraysEnabled)
          }

          (context.copy(resolver = updateResolver(context.resolver)), ())
        } else (context, ())
      }
    })
}

