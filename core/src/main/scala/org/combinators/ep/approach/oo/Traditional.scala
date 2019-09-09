package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm._
import Command._
import AnyParadigm.syntax._

sealed trait Traditional extends ApproachImplementationProvider {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]

  import paradigm._
  import ooParadigm._
  import syntax._

  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      method <- getMember(message.to, names.instanceNameOf(message.request.op))
      result <- apply(method, message.request.op.parameters.map(message.request.arguments))
    } yield result

  }

  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      rt <- findClass(names.conceptNameOf(tpeCase))
      _ <- resolveAndAddImport(rt)
      ctor <- getConstructor(rt)
      res <- apply(ctor, args)
    } yield res
  }

  def makeSignature(op: Operation): Generator[MethodBodyContext, Unit] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      rt <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(rt)
      _ <- setReturnType(rt)
      params <- forEach (op.parameters) { param: Parameter =>
          for {
            pt <- toTargetLanguageType(param.tpe)
            _ <- resolveAndAddImport(pt)
          } yield (names.mangle(param.name), pt)
        }
      _ <- setParameters(params)
    } yield ()
  }

  def makeBase(tpe: DataType, ops: Seq[Operation]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
        import classCapabilities._
        for {
          _ <- setAbstract()
          _ <- forEach(ops) { op => addAbstractMethod(names.instanceNameOf(op), makeSignature(op)) }
        } yield ()
      }
    addClassToProject(names.conceptNameOf(tpe), makeClass)
  }

  def makeField(att: Attribute): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    for {
      ft <- toTargetLanguageType(att.tpe)
      _ <- resolveAndAddImport(ft)
      _ <- addField(names.instanceNameOf(att), ft)
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
      thisRef <- selfReference()
      attAccessors: Seq[Expression] <- forEach (tpeCase.attributes) { att =>
          GetMember(thisRef, names.instanceNameOf(att)).interpret
        }
      atts = tpeCase.attributes.zip(attAccessors).toMap
      allArgs <- getArguments()
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
      params <- forEach (tpeCase.attributes) { att: Attribute =>
          for {
            at <- toTargetLanguageType(att.tpe)
            _ <- resolveAndAddImport(at)
          } yield (names.instanceNameOf(att), at)
        }
      _ <- setParameters(params)
      args <- getArguments()
      _ <- forEach(args) { case (name, _, exp) => initializeField(name, exp) }
    } yield ()
  }

  def makeDerived(tpe: DataType, tpeCase: DataTypeCase, ops: Seq[Operation], domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        pt <- toTargetLanguageType(TypeRep.DataType(tpe))
        _ <- resolveAndAddImport(pt)
        _ <- addParent(pt)
        _ <- forEach (tpeCase.attributes) { att => makeField(att) }
        _ <- addConstructor(makeConstructor(tpeCase))
        _ <- forEach (ops) { op =>
            addMethod(names.instanceNameOf(op), makeImplementation(tpe, tpeCase, op, domainSpecific))
          }
      } yield ()
    }
    addClassToProject(names.conceptNameOf(tpeCase), makeClass)
  }

  def implement(domain: Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    val flatDomain = domain.flatten
    for {
      _ <- makeBase(flatDomain.baseDataType, flatDomain.ops)
      _ <- forEach (flatDomain.typeCases) { tpeCase =>
          makeDerived(flatDomain.baseDataType, tpeCase, flatDomain.ops, domainSpecific)
        }
    } yield ()
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