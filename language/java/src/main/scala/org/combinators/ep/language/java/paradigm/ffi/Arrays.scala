package org.combinators.ep.language.java.paradigm.ffi    /*DI:LD:AI*/

import com.github.javaparser.ast.`type`.ArrayType
import com.github.javaparser.ast.expr.{ArrayAccessExpr, ArrayCreationExpr, ArrayInitializerExpr, AssignExpr, FieldAccessExpr}
import com.github.javaparser.ast.{ArrayCreationLevel, NodeList}
import org.combinators.cogen.{Command, InstanceRep, TypeRep, Understands}
import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.AnyParadigm.syntax.*
import org.combinators.cogen.paradigm.ffi.{CreateArray, Get, Length, Set, Arrays as Arrs}
import org.combinators.ep.language.java.CodeGenerator.Enable
import org.combinators.ep.language.java.Syntax.default.*
import org.combinators.ep.language.java.paradigm.AnyParadigm
import org.combinators.ep.language.java.{ContextSpecificResolver, ProjectCtxt}

class Arrays[Ctxt, T, AP <: AnyParadigm](val base:AP) extends Arrs[Ctxt, T] {
  case object ArraysEnabled

  def arrayCreation[Ctxt](): Understands[Ctxt, CreateArray[Type,Expression]] =
    new Understands[Ctxt, CreateArray[Type,Expression]] {
      def perform(
                   context: Ctxt,
                   command: CreateArray[Type,Expression]
                 ): (Ctxt, Expression) = {

        if (command.dimensions.length == 1) {
          val levels = command.dimensions.map(level => if (command.contentSpec.isDefined) { new ArrayCreationLevel() } else { new ArrayCreationLevel(level) })   // inserting actual value causes problems since cannot have int[len1][len2] with initial values.

          val result = new ArrayCreationExpr(command.elementType)
          result.setLevels(new NodeList(levels *))

          command.contentSpec match {

            case Some((dims, content)) =>
              val initializer = command.contentSpec.map(elt => new ArrayInitializerExpr(new NodeList(elt._2*))).getOrElse(new ArrayInitializerExpr())
              result.setInitializer(initializer)

            case None =>
              result.setInitializer(null)
          }

          (context, result)
        } else {
          val levels = command.dimensions.map(level => if (command.contentSpec.isDefined) { new ArrayCreationLevel() } else { new ArrayCreationLevel(level) })   // inserting actual value causes problems since cannot have int[len1][len2] with initial values.

          val result = new ArrayCreationExpr(command.elementType)
          result.setLevels(new NodeList(levels *))

          command.contentSpec match {

            case Some((dims, content)) =>
              val initializers = dims.reverse.tail.foldLeft[Seq[ArrayInitializerExpr]](
                content.grouped(dims.last).toSeq.map(subSeq => new ArrayInitializerExpr(new NodeList(subSeq *)))
              ) { case (inits, dim) => inits.grouped(dim).toSeq.map(subSeq => new ArrayInitializerExpr(new NodeList(subSeq *))) }
              result.setInitializer(initializers.head)

            case None =>
              result.setInitializer(null)
          }

          // When creating array with initial values, cannot pass in lengths. Cannot do the following for example
          //     new int[2][3] { { 4, 5, 1 }, { 1, 2, 3 } }
          //
          //     new int[expr1][expr2];
            (context, result) // new ArrayInitializerExpr(new NodeList(command.arguments: _ *)))
          }
        }
      }


  val arrayCapabilities: ArrayCapabilities =
    new ArrayCapabilities {

      implicit val canCreate: Understands[Ctxt, CreateArray[Type,Expression]] = arrayCreation()

      implicit val canGet: Understands[Ctxt, Apply[Get, Expression, Expression]] =
        new Understands[Ctxt, Apply[Get, Expression, Expression]] {
          override def perform(
            context: Ctxt,
            command: Apply[Get, Expression, Expression]
          ): (Ctxt, Expression) = {

            val indices = command.arguments.tail.foldLeft(command.arguments.head) { case (acc, level) => new ArrayAccessExpr(acc, level) }

            (context, indices)
          }
        }

      implicit val canSet: Understands[Ctxt, Apply[Set, Expression, Expression]] =
        new Understands[Ctxt, Apply[Set, Expression, Expression]] {
          override def perform(
                                context: Ctxt,
                                command: Apply[Set, Expression, Expression]
                              ): (Ctxt, Expression) = {
            val indices = command.arguments.init.tail.foldLeft(command.arguments.head) { case (acc, level) => new ArrayAccessExpr(acc, level) }

            (context, new AssignExpr(indices, command.arguments.last, AssignExpr.Operator.ASSIGN))
          }
        }

      implicit val canLength: Understands[Ctxt, Apply[Length, Expression, Expression]] =
        new Understands[Ctxt, Apply[Length, Expression, Expression]] {
          def perform(
            context: Ctxt,
            command: Apply[Length, Expression, Expression]
          ): (Ctxt, Expression) = {
            val indices = command.arguments.tail.foldLeft(command.arguments.head) { case (acc, level) => new ArrayAccessExpr(acc, level) }

            (context, new FieldAccessExpr(indices, "length"))
          }
        }
    }

  def enable(): Generator[base.ProjectContext, Unit] =
    Enable.interpret(using new Understands[base.ProjectContext, Enable.type] {
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
              case TypeRep.Array(elemTypeRep) =>

                for {
                  elemType <- projectResolution(k)(elemTypeRep)
                } yield new ArrayType(elemType)
              case other => toResolution(k)(other)
            }

            def addReification[Ctxt](
              reify: ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression],
              projectResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type],
              projectReification: ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression],
              canCreateArray: Understands[Ctxt, CreateArray[Type,Expression]]
            ): ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression] =
              k => rep => rep.tpe match {
                case TypeRep.Array(elemTypeRep) => {


                  // helper function to get flattened elements
                  def elements(elemTypeRep:TypeRep)(elem:elemTypeRep.HostType) : Generator[Ctxt, Seq[Expression]] = {
                    elemTypeRep match {
                      case TypeRep.Array(innerElemTypeRep) => {
                        val seq_gen = elem.asInstanceOf[Array[innerElemTypeRep.HostType]].map(innerElem => elements(innerElemTypeRep)(innerElem))
                        for {
                          flattened <- seq_gen.foldLeft(Command.lift[Ctxt,Seq[Expression]](Seq.empty[Expression])){ case (acc, next_gen) =>
                            for {
                              acc_result <- acc
                              next_result <- next_gen
                            } yield acc_result ++ next_result
                          }
                        } yield flattened
                      }

                      // recursively translates innermost elements
                      case _ => projectReification(k)(InstanceRep(elemTypeRep)(elem)).map(Seq(_))
                    }
                  }

                  // helper function to get type of innermost element -- assume homogenous array
                  def elementType(elemTypeRep:TypeRep)(elem:elemTypeRep.HostType) : Generator[Ctxt, Type] = {
                    elemTypeRep match {
                      case TypeRep.Array(innerElemTypeRep) =>
                        elementType(innerElemTypeRep)(elem.asInstanceOf[Array[innerElemTypeRep.HostType]].head)

                      // recursively find type of innermost element
                      case _ => for {
                        elemType <- projectResolution(k)(elemTypeRep)
                      } yield elemType
                    }
                  }

                  // helper function to get flattened elements
                  def dimensions(elemTypeRep:TypeRep)(elem:elemTypeRep.HostType) : Seq[Int] = {
                    elemTypeRep match {
                      case TypeRep.Array(innerElemTypeRep) => {
                        val outer = elem.asInstanceOf[Array[elemTypeRep.HostType]].length

                        // inner arrays must be uniform length
                        val inner = dimensions(innerElemTypeRep)(elem.asInstanceOf[Array[innerElemTypeRep.HostType]].head)
                        outer +: inner
                      }

                      // recursively translates innermost elements
                      case _ => Seq()

                    }
                  }


                  for {
                    elems <- elements(rep.tpe)(rep.inst)
                    dims <- forEach(dimensions(rep.tpe)(rep.inst)) { dim =>
                      projectReification(k)(InstanceRep(TypeRep.Int)(dim))
                    }
                    elemType <- elementType(rep.tpe)(rep.inst)
                    res <- CreateArray[Type,Expression](elemType, dims, Some(dimensions(rep.tpe)(rep.inst),elems)).interpret(using canCreateArray)
                  } yield res

              }
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

