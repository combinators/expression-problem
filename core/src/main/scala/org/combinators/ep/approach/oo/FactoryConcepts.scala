package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions.{Attribute, DataTypeCase, Operation}
import org.combinators.ep.generator.ApproachImplementationProvider
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
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
   * Standard factory name for an operation.
   *
   * {{{
   *   makeEval
   * }}}
   *
   * Model is passed in should it become necessary to be overridden more specifically
   *
   * @param op    operation for which a factory is desired.
   * @return
   */
  def factoryNameOp(model:Option[Model] = None, op:Operation) : Name = {
    names.addPrefix("make", names.mangle(names.conceptNameOf(op)))
  }

  /**
   * {{{
   *   public FACTORYNAME () {
   *     return new TYPENAME();
   *   }
   * }}}
   * @param model
   * @param op
   * @param typeName
   * @return
   */
  def createFactoryOp(model:Model, op:Operation, typeName:Name): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    for {
      _ <- addMethod(factoryNameOp(Some(model), op), makeFactoryOperationImpl(model, op, typeName))
    } yield ()
  }

  // TODO: would love to avoid duplicating contexts
  def createTestFactoryOp(model:Model, op:Operation, typeName:Name): Generator[TestContext, Unit] = {
    import ooParadigm.testCapabilities._
    for {
      _ <- addMethod(factoryNameOp(Some(model), op), makeFactoryOperationImpl(model, op, typeName))
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

  /**
   * Standard factory name for a dataTypeCase.
   *
   * {{{
   *   makeAdd
   * }}}
   *
   * Model is passed in should it become necessary to be overridden more specifically
   *
   * @param tpeCase    DataTypeCase for which a factory is desired.
   * @return
   */
  def factoryNameDataTypeCase(model:Option[Model] = None, tpeCase:DataTypeCase) : Name = {
    names.addPrefix("make", names.mangle(names.conceptNameOf(tpeCase)))
  }

  /**
   * AddPrettypFinal Add(PrettypExp left, PrettypExp right) {
   *   return new AddPrettypFinal(left, right);
   * }
   *
   * LitPrettypFinal Lit(Double v) {
   *    return new LitPrettypFinal(v);
   * }
   *
   * @param model
   * @param tpeCase
   * @return
   */
  def createFactoryDataTypeCase(model:Model, tpeCase:DataTypeCase): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    // find last operation
    //val lastModelWithOp = model.lastModelWithOperation.get

    //val binp = baseInterfaceNamesPrefix(lastModelWithOp.ops, names.mangle("Final"))
    //val actualName = names.addPrefix(names.conceptNameOf(tpeCase), binp)
    //val baseType = model.baseDataType
    //val paramType = baseInterfaceNames(lastModelWithOp, lastModelWithOp.ops)  // was model

    for {
      opClass <- findClass(factoryNameDataTypeCase(Some(model), tpeCase))    // should check!
      _ <- resolveAndAddImport(opClass)
      _ <- setReturnType(opClass)

      params <- forEach (tpeCase.attributes) { att: Attribute =>
        for {
          at <- toTargetLanguageType(att.tpe)
          pName <- freshName(names.mangle(names.instanceNameOf(att)))
        } yield (pName, at)
      }
      _ <- setParameters(params)

      argSeq <- getArguments().map( args => { args.map(triple => triple._3) })
      res <- instantiateObject(opClass, argSeq)

    } yield Some(res)
  }
}
