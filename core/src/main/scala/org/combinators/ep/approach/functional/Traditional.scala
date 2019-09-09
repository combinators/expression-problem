package org.combinators.ep.approach.functional

import org.combinators.ep.domain.{Model, abstractions}
import org.combinators.ep.generator.{AbstractSyntax, ApproachImplementationProvider, Command, EvolutionImplementationProvider, NameProvider, TestImplementationProvider, communication}
import org.combinators.ep.generator.paradigm.control.{Functional => FunControl}
import Command.{skip, _}
import cats.syntax._
import cats.implicits._
import org.combinators.ep.domain.abstractions.{Attribute, DataType, DataTypeCase, Operation, Parameter, TestCase, TypeRep}
import org.combinators.ep.generator.communication.{ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.{AddCompilationUnit, AddImport, AddMethod, AddType, AddTypeConstructor, AnyParadigm, Apply, FindMethod, Functional, GetArguments, InstantiateType, ResolveImport, SetParameters, ToTargetLanguageType}
import AnyParadigm.syntax._
import org.combinators.ep.generator.paradigm.control.Functional.WithBase

trait Traditional extends ApproachImplementationProvider {
  val functional: Functional.WithBase[paradigm.type]
  val functionalControl: FunControl.WithBase[paradigm.MethodBodyContext, paradigm.type]

  import paradigm._
  import functional._
  import syntax._

  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import functional.methodBodyCapabilities._

    for {
      method <- findMethod(names.instanceNameOf(message.request.op))
      _ <- resolveAndAddImport(method)
      res <- apply(method, message.to +: message.request.op.parameters.map(message.request.arguments))
    } yield res
  }

  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import functional.methodBodyCapabilities._
    for {
      rt <- toTargetLanguageType(TypeRep.DataType(baseTpe))
      _ <- resolveAndAddImport(rt)
      res <- instantiateType(rt, names.conceptNameOf(tpeCase), args)
    } yield res
  }

  def makeTypeConstructor(tpeCase: DataTypeCase): Generator[TypeContext, Unit] = {
    import typeCapabilities._
    for {
      params <- forEach (tpeCase.attributes) { att =>
          for {
            ty <- toTargetLanguageType(att.tpe)
            _ <- resolveAndAddImport(ty)
          } yield (names.instanceNameOf(att), ty)
        }
      _ <- addTypeConstructor(names.conceptNameOf(tpeCase), params)
    } yield ()
  }

  def makeTypeInCompilationUnit(tpe: DataType, cases: Seq[DataTypeCase]): Generator[ProjectContext, Unit] = {
    import functional.compilationUnitCapabilities._
    import projectContextCapabilities._
    val caseCode =
      for {
        _ <- forEach (cases) { tpeCase => makeTypeConstructor(tpeCase) }
      } yield ()

    addCompilationUnit(
      names.conceptNameOf(tpe),
      addType(names.conceptNameOf(tpe), caseCode)
    )
  }

  def makeCases(
      tpe: DataType,
      cases: Seq[DataTypeCase],
      op: Operation,
      selfReference: Expression,
      args: Seq[(String, Type, Expression)],
      domainSpecific: EvolutionImplementationProvider[this.type]
    )(ctorName: String, ctorArgs: Seq[Expression]): Generator[MethodBodyContext, Expression] = {
    val tpeCase = cases.find(c => names.conceptNameOf(c) == ctorName).get
    domainSpecific.logic(this)(
      ReceivedRequest(
        tpe,
        tpeCase,
        selfReference,
        tpeCase.attributes.zip(ctorArgs).toMap,
        Request(op, op.parameters.zip(args.map(_._3)).toMap)
      ))
  }

  def makeFunction(
      tpe: DataType,
      cases: Seq[DataTypeCase],
      op: Operation,
      domainSpecific: EvolutionImplementationProvider[this.type]
    ): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import functionalControl.functionalCapabilities._
    for {
      params <- forEach (Parameter("this", TypeRep.DataType(tpe)) +: op.parameters) { param: Parameter =>
          for {
            pt <- toTargetLanguageType(param.tpe)
            _ <- resolveAndAddImport(pt)
          } yield (names.mangle(param.name), pt)
        }
      _ <- setParameters(params)
      args <- getArguments()
      result <- patternMatch(
          args.head._3,
          makeCases(tpe, cases, op, args.head._3, args.tail, domainSpecific)
        )
    } yield result
  }

  def makeFunctionInCompilationUnit(
    tpe: DataType,
    cases: Seq[DataTypeCase],
    op: Operation,
    domainSpecific: EvolutionImplementationProvider[this.type]
  ): Generator[ProjectContext, Unit] = {
    import functional.compilationUnitCapabilities._
    import projectContextCapabilities._
    addCompilationUnit(
      names.conceptNameOf(op),
      addMethod(names.instanceNameOf(op), makeFunction(tpe, cases, op, domainSpecific))
    )
  }

  def implement(domain: Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    val flatDomain = domain.flatten
    for {
      _ <- makeTypeInCompilationUnit(flatDomain.baseDataType, flatDomain.typeCases)
      _ <- forEach (flatDomain.ops) { op =>
          makeFunctionInCompilationUnit(flatDomain.baseDataType, flatDomain.typeCases, op, domainSpecific)
        }
    } yield ()
  }


}

object Traditional {
  type WithParadigm[P <: AnyParadigm] = Traditional { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
    (nameProvider: NameProvider, base: P)
    (fun: Functional.WithBase[base.type],
      funControl: FunControl.WithBase[base.MethodBodyContext, base.type]): Traditional.WithParadigm[base.type] =
    new Traditional {
      override val names: NameProvider = nameProvider
      override val paradigm: base.type = base
      override val functional: Functional.WithBase[paradigm.type] = fun
      override val functionalControl: WithBase[paradigm.MethodBodyContext, paradigm.type] = funControl
    }
}