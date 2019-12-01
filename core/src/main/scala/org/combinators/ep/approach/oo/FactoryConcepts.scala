package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.generator.ApproachImplementationProvider
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.ObjectOriented

/**
 * Some EP approaches need factory methods for either operations or data types
 */
trait FactoryConcepts extends ApproachImplementationProvider with SharedOO {
    val ooParadigm: ObjectOriented.WithBase[paradigm.type]

    import ooParadigm._
    import paradigm._
    import syntax._

  /**
   * {{{
   *   public FACTORYNAME () {
   *     return new TYPENAME();
   *   }
   * }}}
   * @param factoryName
   * @param model
   * @param op
   * @param typeName
   * @return
   */
  def createFactoryOp(factoryName:Name, model:Model, op:Operation, typeName:Name): Generator[ClassContext, Unit] = {

    import ooParadigm.classCapabilities._
    for {
      _ <- addMethod(factoryName, makeFactoryOperationImpl(model, op, typeName))
    } yield ()
  }

  /**
   * Just return the expression required for a factory
   *
   * {{{
   *     return new EvalDivdMultNeg();
   * }}}
   *
   * TODO: Eventually will have to add parameters...
   * @return
   */
  def makeFactoryOperationImpl(model:Model, op: Operation, typeName:Name): Generator[MethodBodyContext, Option[Expression]] = {

    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      opClass <- findClass(typeName)    // should check!
      _ <- resolveAndAddImport(opClass)
      _ <- setReturnType(opClass)

      res <- instantiateObject(opClass, Seq.empty)
    } yield Some(res)
  }
}
