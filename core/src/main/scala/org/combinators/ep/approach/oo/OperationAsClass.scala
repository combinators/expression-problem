package org.combinators.ep.approach.oo

import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, Operation}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.ObjectOriented
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

/**
 * In some EP approaches, each operation is mapped to a specific class.
 *
 */
trait OperationAsClass extends ApproachImplementationProvider with SharedOO {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]

  import ooParadigm._
  import paradigm._
  import syntax._

  /**
   * Constructor for an operation which MAY have parameters
   * @param op
   * @return
   */
  def makeOperationConstructor(op: Operation): Generator[ConstructorContext, Unit] = {
    import constructorCapabilities._
    for {
      params <- forEach (op.parameters) { param =>
        for {
          paramTy <- toTargetLanguageType(param.tpe)
          _ <- resolveAndAddImport(paramTy)
          pName <- freshName(names.mangle(param.name))
        } yield (pName, paramTy)
      }
      _ <- setParameters(params)
      args <- getArguments()
      _ <- forEach (op.parameters.zip(args)) { case (param, (_, _, arg)) =>
        initializeField(names.mangle(param.name), arg)
      }
    } yield ()

  }


  /**
   *
   * Each operation maps to a class
   *
   * {{{
   * public class OP-NAME ... {
   *    public OP-NAME () { }
   *    public .... methodName(DataType ...) { ... }
   *    public .... methodName(DataType ...) { ... }
   * }
   * }}}
   *
   * @param methodName        The name to use for all methods
   * @param op                The operation which is the reason this class is to be created
   * @param typeCases         All typeCases necessary to have implementations in this class
   * @param base              Known base DataType of the domain
   * @param domainSpecific    Knowledge of the chosen evolution. Needed when generating the methods
   * @return
   * @see  makeImplementation
   */
  def operationClass(methodName:Name, op:Operation, typeCases:Seq[DataTypeCase], base:DataType, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._

    for {
      returnTpe <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(returnTpe)

      _ <- addConstructor(makeOperationConstructor(op))
      _ <- forEach (typeCases) { tpe =>
        addMethod(methodName, makeImplementation(base, tpe, op, domainSpecific))
      }
    } yield ()
  }
}
