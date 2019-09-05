package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.generator.{AbstractSyntax, ApproachImplementationProvider, Command, EvolutionImplementationProvider, NameProvider, Understands, communication}
import org.combinators.ep.generator.paradigm.{AddConstructor, AddField, AddImport, AddMethod, AddParent, AnyParadigm, Apply, FindClass, GetArguments, GetConstructor, GetMember, InitializeField, ObjectOriented, ResolveImport, SelfReference, SetAbstract, SetParameters, SetReturnType, ToTargetLanguageType}
import Command._
import cats.syntax._
import cats.implicits._
import org.combinators.ep.domain.abstractions.{Attribute, DataType, DataTypeCase, Operation, Parameter, TypeRep}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.generator.communication.{ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.ObjectOriented.WithBase

sealed trait Traditional extends ApproachImplementationProvider {

  val names: NameProvider
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]

  import paradigm._
  import ooParadigm._
  import syntax._


  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      method <- GetMember[Expression](message.to, names.instanceNameOf(message.request.op)).interpret
      result <- Apply[Expression](method, message.request.op.parameters.map(message.request.arguments)).interpret
    } yield result

  }

  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      rt <- FindClass[Type](names.conceptNameOf(tpeCase)).interpret
      _ <- resolveAndAddImport(rt)
      ctor <- GetConstructor[Type, Expression](rt).interpret
      res <- Apply(ctor, args).interpret
    } yield res
  }

  def makeSignature(op: Operation): Generator[MethodBodyContext, Unit] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      rt <- ToTargetLanguageType[Type](op.returnType).interpret
      _ <- resolveAndAddImport(rt)
      _ <- SetReturnType(rt).interpret
      params <- op.parameters.toList.map { param: Parameter =>
          for {
            pt <- ToTargetLanguageType[Type](param.tpe).interpret
            _ <- resolveAndAddImport(pt)
          } yield (names.mangle(param.name), pt)
        }.sequence
      _ <- SetParameters(params).interpret
    } yield ()
  }

  def makeBase(tpe: DataType, ops: Seq[Operation]): Generator[ProjectContext, Unit] = {
    val makeClass: Generator[ClassContext, Unit] = {
        import classCapabilities._
        for {
          _ <- SetAbstract().interpret
          _ <- ops.toList.foldMapM(op => addAbstractMethod(names.instanceNameOf(op), makeSignature(op)))
        } yield ()
      }
    addClassToProject(names.conceptNameOf(tpe), makeClass)
  }

  def makeField(att: Attribute): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    for {
      ft <- ToTargetLanguageType[Type](att.tpe).interpret
      _ <- resolveAndAddImport(ft)
      _ <- AddField(names.instanceNameOf(att), ft).interpret
    } yield ()
  }

  def makeImplementation(
      tpe: DataType,
      tpeCase: DataTypeCase,
      op: Operation,
      domainSpecific: EvolutionImplementationProvider[this.type]
    ): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      _ <- makeSignature(op)
      thisRef <- SelfReference[Expression]().interpret
      attAccessors: Seq[Expression] <- tpeCase.attributes.toList.map { att =>
          GetMember(thisRef, names.instanceNameOf(att)).interpret
        }.sequence
      atts = tpeCase.attributes.zip(attAccessors).toMap
      allArgs <- GetArguments[Type, Expression]().interpret
      args = allArgs.map { case (name, _, exp) => (name, exp) }.toMap
      result <-
        domainSpecific.logic(this)(
          ReceivedRequest(
            tpe,
            tpeCase,
            thisRef,
            atts,
            Request(op, op.parameters.map(param => (param, args(names.mangle(param.name)))).toMap)
          )
        )
    } yield Some(result)
  }

  def makeConstructor(tpeCase: DataTypeCase): Generator[ConstructorContext, Unit] = {
    import ooParadigm.constructorCapabilities._
    for {
      params <- tpeCase.attributes.toList.map { att: Attribute =>
          for {
            at <- ToTargetLanguageType[Type](att.tpe).interpret
            _ <- resolveAndAddImport(at)
          } yield (names.instanceNameOf(att), at)
        }.sequence
      _ <- SetParameters(params).interpret
      args <- GetArguments[Type, Expression]().interpret
      _ <- args.toList.foldMapM { case (name, _, exp) =>
          InitializeField[Expression](name, exp).interpret
        }
    } yield ()
  }

  def makeDerived(tpe: DataType, tpeCase: DataTypeCase, ops: Seq[Operation], domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        pt <- ToTargetLanguageType[Type](TypeRep.DataType(tpe)).interpret
        _ <- resolveAndAddImport(pt)
        _ <- AddParent(pt).interpret
        _ <- tpeCase.attributes.toList.foldMapM(att => makeField(att))
        _ <- AddConstructor(makeConstructor(tpeCase)).interpret
        _ <- ops.toList.foldMapM { op =>
            AddMethod(names.instanceNameOf(op), makeImplementation(tpe, tpeCase, op, domainSpecific)).interpret
          }
      } yield ()
    }
    addClassToProject(names.conceptNameOf(tpeCase), makeClass)
  }

  def implement(domain: Model, domainSpecific: EvolutionImplementationProvider[this.type]): Seq[CompilationUnit] = {
    val flatDomain = domain.flatten
    val project =
      for {
        _ <- makeBase(flatDomain.baseDataType, flatDomain.ops)
        _ <- flatDomain.typeCases.toList.foldMapM { tpeCase =>
            makeDerived(flatDomain.baseDataType, tpeCase, flatDomain.ops, domainSpecific)
          }
      } yield ()
    Seq.empty
  }
}

object Traditional {
  type WithParadigm[P <: AnyParadigm] = Traditional { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
      (nameProvider: NameProvider, base: P)
      (oo: ObjectOriented.WithBase[base.type]): Traditional.WithParadigm[base.type] =
    new Traditional {
      override val names: NameProvider = nameProvider
      override val paradigm: base.type = base
      override val ooParadigm: ObjectOriented.WithBase[paradigm.type] = oo
    }
}