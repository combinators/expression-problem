package org.combinators.ep.approach.functional

import org.combinators.ep.domain.{Model, abstractions}
import org.combinators.ep.generator.{AbstractSyntax, ApproachImplementationProvider, Command, EvolutionImplementationProvider, NameProvider, TestImplementationProvider, Understands, communication}
import org.combinators.ep.generator.paradigm.control.{Functional => FunControl}
import Command.{skip, _}
import cats.syntax._
import cats.implicits._
import org.combinators.ep.domain.abstractions.{Attribute, DataType, DataTypeCase, Operation, Parameter, TestCase, TypeRep}
import org.combinators.ep.generator.communication.{ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.{AddCompilationUnit, AddImport, AddMethod, AddType, AddTypeConstructor, AnyParadigm, Apply, FindClass, FindMethod, FindType, Functional, GetArguments, InstantiateType, ResolveImport, SetParameters, ToTargetLanguageType}
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
      method <- findMethod(names.mangle(names.instanceNameOf(message.request.op)))
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
      names.mangle(names.conceptNameOf(tpe)),
      addType(names.mangle(names.conceptNameOf(tpe)), caseCode)
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
    val tpeCase = cases.find(c => names.conceptNameOf(c) == names.unmangle(ctorName)).get
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
      params <- forEach (Parameter("this", TypeRep.DataType(tpe)) +: op.parameters) { param: Parameter =>
          for {
            pt <- toTargetLanguageType(param.tpe)
            _ <- resolveAndAddImport(pt)
            pName <- freshName(param.name)
          } yield (pName, pt)
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
    import paradigm.projectContextCapabilities._
    addCompilationUnit(
      names.mangle(names.conceptNameOf(op)),
      addMethod(names.mangle(names.instanceNameOf(op)), makeFunction(tpe, cases, op, domainSpecific))
    )
  }

  def domainTypeLookup[Ctxt](dtpe: DataType)(implicit canFindType: Understands[Ctxt, FindType[Name, Type]]): Generator[Ctxt, Type] = {
    FindType(names.mangle(names.conceptNameOf(dtpe))).interpret(canFindType)
  }

  def initializeApproach(domain: Model): Generator[ProjectContext, Unit] = {
    import paradigm.projectContextCapabilities._
    import functional.projectContextCapabilities._
    import functional.methodBodyCapabilities._
    import functional.typeCapabilities._
    val dtpeRep = TypeRep.DataType(domain.baseDataType)
    for {
      _ <- addTypeLookupForMethods(dtpeRep, domainTypeLookup(domain.baseDataType))
      _ <- addTypeLookupForTypes(dtpeRep, domainTypeLookup(domain.baseDataType))
    } yield ()
  }

  def implement(domain: Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    val flatDomain = domain.flatten
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