package org.combinators.ep.approach.oo    /*DI:LI:AD*/

import org.combinators.ep.domain.GenericModel
import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, Operation, Parameter}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.ObjectOriented
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

/**
 * In some EP approaches, each operation is mapped to a specific class.
 */
trait OperationAsClass extends ApproachImplementationProvider {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]

  import ooParadigm._
  import paradigm._
  import syntax._

  /** Requires the capability of constructing an implementation */
  def makeImplementation(
                          tpe: DataType,
                          tpeCase: DataTypeCase,
                          op: Operation,
                          model: GenericModel,
                          domainSpecific: EvolutionImplementationProvider[this.type]
                        ): Generator[MethodBodyContext, Option[Expression]]

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

  /** Create parameter fields for operation parameter.  */
  def addParamFields(op: Operation): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    def addParamField(param: Parameter): Generator[ClassContext, Unit] =
      for {
        ft <- toTargetLanguageType(param.tpe)
        _ <- resolveAndAddImport(ft)
        _ <- addField(names.mangle(param.name), ft)
      } yield ()

    for {
      _ <- forEach (op.parameters) { param => addParamField(param) }
    } yield ()
  }

  /**
   *
   * Each operation maps to a class
   *
   * {{{
   * public class OP-NAME ... {
   *    T1 param1;
   *    T2 param2;
   *    ...
   *    public OP-NAME (T1 param1, T2, param2, ...) { }
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
  def operationClass(methodName:Name, op:Operation, model:GenericModel, typeCases:Seq[DataTypeCase], base:DataType, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._

    for {
      returnTpe <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(returnTpe)
      _ <- addParamFields(op)
      _ <- addConstructor(makeOperationConstructor(op))
      _ <- forEach (typeCases) { tpe =>
        addMethod(methodName, makeImplementation(base, tpe, op, model, domainSpecific))
      }
    } yield ()
  }
}
