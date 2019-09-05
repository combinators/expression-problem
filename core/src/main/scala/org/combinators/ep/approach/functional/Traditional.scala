package org.combinators.ep.approach.functional

import org.combinators.ep.domain.Model
import org.combinators.ep.generator.Command
import org.combinators.ep.generator.{AbstractSyntax, ApproachImplementationProvider, EvolutionImplementationProvider, NameProvider, communication}
import Command.{skip, _}
import cats.syntax._
import cats.implicits._
import org.combinators.ep.domain.abstractions.{Attribute, DataType, DataTypeCase, Operation, Parameter, TypeRep}
import org.combinators.ep.generator.communication.{ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.{AddCompilationUnit, AddImport, AddMethod, AddType, AddTypeConstructor, AnyParadigm, Apply, FindMethod, Functional, GetArguments, InstantiateType, PatternMatch, ResolveImport, SetParameters, ToTargetLanguageType}

trait Traditional extends ApproachImplementationProvider {
  val names: NameProvider
  val functional: Functional.WithBase[paradigm.type]

  import paradigm._
  import functional._
  import syntax._

  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import functional.methodBodyCapabilities._

    for {
      method <- FindMethod[Expression](names.instanceNameOf(message.request.op)).interpret
      _ <- resolveAndAddImport(method)
      res <- Apply(method, message.to +: message.request.op.parameters.map(message.request.arguments)).interpret
    } yield res
  }

  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import functional.methodBodyCapabilities._
    for {
      rt <- ToTargetLanguageType[Type](TypeRep.DataType(baseTpe)).interpret
      _ <- resolveAndAddImport(rt)
      res <- InstantiateType[Type, Expression](rt, names.conceptNameOf(tpeCase), args).interpret
    } yield res
  }

  def makeTypeConstructor(tpeCase: DataTypeCase): Generator[TypeContext, Unit] = {
    import typeCapabilities._
    for {
      params <- tpeCase.attributes.toList.map { att =>
          for {
            ty <- ToTargetLanguageType[Type](att.tpe).interpret
            _ <- resolveAndAddImport(ty)
          } yield (names.instanceNameOf(att), ty)
        }.sequence
      _ <- AddTypeConstructor[Type](names.conceptNameOf(tpeCase), params).interpret
    } yield ()
  }

  def makeTypeInCompilationUnit(tpe: DataType, cases: Seq[DataTypeCase]): Generator[ProjectContext, Unit] = {
    import functional.compilationUnitCapabilities._
    import projectContextCapabilities._
    AddCompilationUnit[CompilationUnitContext](
      names.conceptNameOf(tpe),
      AddType(names.conceptNameOf(tpe), cases.toList.foldMapM(makeTypeConstructor)).interpret
    ).interpret
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
    import functional.methodBodyCapabilities._
    for {
      params <- (Parameter("this", TypeRep.DataType(tpe)) +: op.parameters).toList.map { param: Parameter =>
          for {
            pt <- ToTargetLanguageType[Type](param.tpe).interpret
            _ <- resolveAndAddImport(pt)
          } yield (names.mangle(param.name), pt)
        }.sequence
      _ <- SetParameters(params).interpret
      args <- GetArguments[Type, Expression]().interpret
      result <- PatternMatch(
          args.head._3,
          makeCases(tpe, cases, op, args.head._3, args.tail, domainSpecific)
        ).interpret
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
    AddCompilationUnit[CompilationUnitContext](
      names.conceptNameOf(op),
      AddMethod(names.instanceNameOf(op), makeFunction(tpe, cases, op, domainSpecific)).interpret
    ).interpret
  }

  def implement(domain: Model, domainSpecific: EvolutionImplementationProvider[this.type]): Seq[CompilationUnit] = {
    val flatDomain = domain.flatten
    val project: Generator[ProjectContext, Unit] =
      for {
        _ <- makeTypeInCompilationUnit(flatDomain.baseDataType, flatDomain.typeCases)
        _ <- flatDomain.ops.toList.map { op =>
            makeFunctionInCompilationUnit(flatDomain.baseDataType, flatDomain.typeCases, op, domainSpecific)
          }.sequence
      } yield ()

    ???
  }
}

object Traditional {
  type WithParadigm[P <: AnyParadigm] = Traditional { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
    (nameProvider: NameProvider, base: P)
    (fun: Functional.WithBase[base.type]): Traditional.WithParadigm[base.type] =
    new Traditional {
      override val names: NameProvider = nameProvider
      override val paradigm: base.type = base
      override val functional: Functional.WithBase[paradigm.type] = fun
    }
}