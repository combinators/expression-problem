package org.combinators.ep.approach.oo    /*DI:LI:AD*/

import org.combinators.ep.domain.GenericModel
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm._
import Command._
import AnyParadigm.syntax._
import org.combinators.ep.domain.GraphViz.GraphViz

trait Traditional extends SharedOO {  // this had been sealed. not sure why
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]

  import paradigm._
  import ooParadigm._
  import syntax._

  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      method <- getMember(message.to, names.mangle(names.instanceNameOf(message.request.op)))
      result <- apply(method, message.request.op.parameters.map(message.request.arguments))
    } yield result
  }

  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      rt <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(rt)

      res <- instantiateObject(rt, args)
    } yield res
  }

  /**
   * When a DataTypeCase forms a class (given a sequence of operations) this function does the heavy lifting.
   *
   * A constructor is generated, using [[makeConstructor]]. Fields are generates, using [[makeField]]. Each
   * operation is embedded as a method within each class, using [[makeImplementation]]
   */
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
            addMethod(names.mangle(names.instanceNameOf(op)), makeImplementation(tpe, tpeCase, op, domainSpecific))
          }
      } yield ()
    }
    addClassToProject(makeClass, names.mangle(names.conceptNameOf(tpeCase)))
  }

  def makeBase(tpe: DataType, ops: Seq[Operation]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- setAbstract()
        _ <- forEach(ops) { op => addAbstractMethod(names.mangle(names.instanceNameOf(op)), makeSignature(op)) }
      } yield ()
    }

    addClassToProject(makeClass, names.mangle(names.conceptNameOf(tpe)))
  }

  def implement(gdomain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {

    // Just trying to find a spot to generate graphviz of eips.
    GraphViz.outputGraphWithDependenciesViz(gdomain, domainSpecific)   // documenting graph


    val flatDomain = gdomain.linearize.flatten
    for {
      _ <- registerTypeMapping(flatDomain)
      _ <- domainSpecific.initialize(this)

      _ <- makeBase(flatDomain.baseDataType, flatDomain.ops)
      _ <- forEach (flatDomain.typeCases.distinct) { tpeCase =>
          makeDerived(flatDomain.baseDataType, tpeCase, flatDomain.ops, domainSpecific)
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
        oo: ObjectOriented.WithBase[base.type]
      ): Traditional.WithParadigm[base.type] =
    new Traditional {
      override val paradigm: base.type = base
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val ooParadigm: ObjectOriented.WithBase[paradigm.type] = oo
    }
}
