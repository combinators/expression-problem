package org.combinators.ep.approach.functional    /*DI:LI:AI*/

import org.combinators.ep.domain.{GenericModel, Model}
import org.combinators.ep.generator.{AbstractSyntax, ApproachImplementationProvider, Command, EvolutionImplementationProvider, NameProvider, Understands}
import org.combinators.ep.generator.paradigm.control.{Functional => FunControl}
import Command.{Generator, _}
import cats.implicits._
import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, Operation, Parameter, TypeRep}
import org.combinators.ep.generator.communication.{ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindType, Functional}
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
      method <- findMethod(Seq(names.mangle(names.instanceNameOf(message.request.op))))
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
      res <- instantiateType(rt, names.mangle(names.conceptNameOf(tpeCase)), args)
    } yield res
  }

  def makeTypeConstructor(tpeCase: DataTypeCase): Generator[TypeContext, Unit] = {
    import typeCapabilities._
    for {
      params <- forEach (tpeCase.attributes) { att =>
          for {
            ty <- toTargetLanguageType(att.tpe)
            _ <- resolveAndAddImport(ty)
          } yield (names.mangle(names.instanceNameOf(att)), ty)
        }
      _ <- addTypeConstructor(names.mangle(names.conceptNameOf(tpeCase)), params)
    } yield ()
  }

  def makeTypeInCompilationUnit(tpe: DataType, cases: Seq[DataTypeCase]): Generator[ProjectContext, Unit] = {
    import functional.compilationUnitCapabilities._
    import paradigm.projectContextCapabilities._
    val caseCode =
      for {
        _ <- forEach (cases) { tpeCase => makeTypeConstructor(tpeCase) }
      } yield ()

    addCompilationUnit(
      addType(names.mangle(names.conceptNameOf(tpe)), caseCode),
      names.mangle(names.conceptNameOf(tpe))
    )
  }

  def makeCases(
      tpe: DataType,
      cases: Seq[DataTypeCase],
      op: Operation,
      selfReference: Expression,
      args: Seq[(Name, Type, Expression)],
      domainSpecific: EvolutionImplementationProvider[this.type]
    )(ctorName: Name, ctorArgs: Seq[Expression]): Generator[MethodBodyContext, Expression] = {
    val tpeCase = cases.find(c => names.mangle(names.conceptNameOf(c)) == ctorName).get
    for {
      result <- domainSpecific.logic(this)(
        ReceivedRequest(
          tpe,
          tpeCase,
          selfReference,
          tpeCase.attributes.zip(ctorArgs).toMap,
          Request(op, op.parameters.zip(args.map(_._3)).toMap)
        ))
       _ = assert(result.nonEmpty, "Functional code generator expects non empty result expression from EIP logic")
    } yield result.get
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
      params <- forEach (Parameter(names.instanceNameOf(tpe), TypeRep.DataType(tpe)) +: op.parameters) { param: Parameter =>
          for {
            pt <- toTargetLanguageType(param.tpe)
            _ <- resolveAndAddImport(pt)
            pName <- freshName(names.mangle(param.name))
          } yield (pName, pt)
        }
      _ <- setParameters(params)
      returnType <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(returnType)
      _ <- setReturnType(returnType)
      args <- getArguments()
      result <- {
        val matchGen = makeCases(tpe, cases, op, args.head._3, args.tail, domainSpecific)(_, _)
        val tpeName = names.mangle(names.conceptNameOf(tpe))
        patternMatch(
          args.head._3,
          cases.map(tpeCase => {            
            val caseName = names.mangle(names.conceptNameOf(tpeCase))
            val attributeNames = tpeCase.attributes.map(attName => names.mangle(names.instanceNameOf(attName)))
            ((Seq[Name](tpeName, caseName), attributeNames), matchGen(caseName, _))
          }).toMap)
        }
    } yield result
  }

  def makeFunctionInCompilationUnit(
    tpe: DataType,
    cases: Seq[DataTypeCase],
    op: Operation,
    domainSpecific: EvolutionImplementationProvider[this.type]
  ): Generator[ProjectContext, Unit] = {
    import functional.compilationUnitCapabilities._
    import paradigm.projectContextCapabilities._
    addCompilationUnit(
      addMethod(names.mangle(names.instanceNameOf(op)), makeFunction(tpe, cases, op, domainSpecific)),
      names.mangle(names.conceptNameOf(op))
    )
  }

  def domainTypeLookup[Ctxt](dtpe: DataType)(implicit canFindType: Understands[Ctxt, FindType[Name, Type]]): Generator[Ctxt, Type] = {
    FindType(Seq(names.mangle(names.conceptNameOf(dtpe)))).interpret(canFindType)
  }

  def initializeApproach(domain: GenericModel): Generator[ProjectContext, Unit] = {
    import paradigm.projectContextCapabilities._
    import functional.projectContextCapabilities._
    import functional.methodBodyCapabilities._         // Needed below
    import functional.typeCapabilities._               // Needed below
    val dtpeRep = TypeRep.DataType(domain.baseDataType)
    for {
      _ <- addTypeLookupForMethods(dtpeRep, domainTypeLookup(domain.baseDataType))
      _ <- addTypeLookupForTypes(dtpeRep, domainTypeLookup(domain.baseDataType))
    } yield ()
  }

  def implement(gdomain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {

    val flatDomain = gdomain.linearize.flatten
    for {
      _ <- initializeApproach(flatDomain)
      _ <- domainSpecific.initialize(this)
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
    (base: P)
    (nameProvider: NameProvider[base.syntax.Name],
      fun: Functional.WithBase[base.type],
      funControl: FunControl.WithBase[base.MethodBodyContext, base.type]): Traditional.WithParadigm[base.type] =
    new Traditional {
      override val paradigm: base.type = base
      override val names: NameProvider[base.syntax.Name] = nameProvider
      override val functional: Functional.WithBase[paradigm.type] = fun
      override val functionalControl: WithBase[paradigm.MethodBodyContext, paradigm.type] = funControl
    }
}
