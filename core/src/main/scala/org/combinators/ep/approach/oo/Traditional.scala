package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.generator.Command
import org.combinators.ep.generator.{AbstractSyntax, ApproachImplementationProvider, EvolutionImplementationProvider, NameProvider, communication}
import org.combinators.ep.generator.paradigm.{AddMethod, AnyParadigm, Apply, GetMember, ObjectOriented}
import Command._
import cats.free.Free._
import org.combinators.ep.domain.abstractions.{DataType, Operation}

trait Traditional extends ApproachImplementationProvider {

  val names: NameProvider
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]

  import paradigm._
  import ooParadigm._
  import syntax._


  def dispatch(message: communication.SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    for {
      method <- GetMember[Expression](message.to, names.instanceNameOf(message.request.op)).interpret(canGetMemberInMethod)
      result <- Apply[Expression](method, message.request.op.parameters.map(message.request.arguments)).interpret(canApplyInMethodBody)
    } yield result
  }

  def makeBase(tpe: DataType, ops: Seq[Operation]): Generator[ProjectContext, Type] = {
    val methods: Generator[ClassContext, Unit] =
      ops.flatMap(op => AddMethod())

  }


  /** Produces all compilation units necessary to implement the given model.
    * Fills in domain specific code with the given approach independent [[EvolutionImplementationProvider]].
    */
  def implement(domain: Model, domainSpecific: EvolutionImplementationProvider.WithSyntax[syntax.type]): Seq[CompilationUnit] = {
    val flatDomain = domain.flatten


    Seq.empty
  }
}
