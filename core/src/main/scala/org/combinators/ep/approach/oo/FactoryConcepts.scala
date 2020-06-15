package org.combinators.ep.approach.oo    /*DI:LI:AD*/

import org.combinators.ep.domain.{GenericModel, Model}
import org.combinators.ep.domain.abstractions.{Attribute, DataTypeCase, Operation, TypeRep}
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, Understands}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.{FindClass, ObjectOriented, ParametricPolymorphism}

/**
 * Some EP approaches need factory methods for either operations or data types
 */
trait FactoryConcepts extends ApproachImplementationProvider {
    val ooParadigm: ObjectOriented.WithBase[paradigm.type]
    val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
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
  def factoryNameOp(model:Option[GenericModel] = None, op:Operation) : Name = {
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
  def createTestFactoryOp(model:GenericModel, op:Operation, typeName:Name): Generator[TestContext, Unit] = {
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
  def makeFactoryOperationImpl(model:GenericModel, op: Operation, typeName:Name): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      opClass <- toTargetLanguageType(op.returnType)    // should check!
      _ <- resolveAndAddImport(opClass)
      _ <- setReturnType(opClass)

      // need parameters for operations with parameters
      params <- forEach (op.parameters) { param =>
        for {
          paramTy <- toTargetLanguageType(param.tpe)
          _ <- resolveAndAddImport(paramTy)
          pName <- freshName(names.mangle(param.name))
        } yield (pName, paramTy)
      }
      _ <- setParameters(params)  // params: Seq[(Name, Type)]

      args <- getArguments()
      res <- instantiateObject(opClass,args.map(_._3))
    } yield Some(res)
  }

  /**
   * Standard factory name for a dataTypeCase is just the concept name of the data type.
   *
   * {{{
   *   Add
   * }}}
   *
   * Model is passed in should it become necessary to be overridden more specifically
   *
   * @param tpeCase    DataTypeCase for which a factory is desired.
   * @return
   */
  def factoryNameDataTypeCase(model:Option[GenericModel] = None, tpeCase:DataTypeCase) : Name = {
    names.mangle(names.conceptNameOf(tpeCase))
  }

  /**
   * The class to instantiate is a sub-type (by default the same) as the [[factoryNameDataTypeCase]]
   *
   * {{{
   *   EvalAdd
   * }}}
   *
   * Model is passed in should it become necessary to be overridden more specifically
   *
   * @param tpeCase    DataTypeCase for which a factory is desired.
   * @return
   */
  def factoryInstanceDataTypeCase(model:Option[GenericModel] = None, tpeCase:DataTypeCase) : Seq[Name] = {
    model.map(m => names.mangle(m.name)).toSeq :+ names.mangle(names.conceptNameOf(tpeCase))
  }

  /**
   * When factory-like methods need to be generated for a class based upon a dataTypeCase, this function
   * does most of the heavy lifting.
   *
   * Return type can be overridden by [[factoryNameDataTypeCase]]
   * Instantiated object internally can be overridden by [[factoryInstanceDataTypeCase]]
   *
   * Trivially requires the following in its test cases:
   *
   * {{{
   * AddPrettypFinal Add(PrettypExp left, PrettypExp right) {
   *   return new AddPrettypFinal(left, right);
   * }
   *
   * LitPrettypFinal Lit(Double v) {
   *    return new LitPrettypFinal(v);
   * }
   * }}}
   *
   * While interpreter calls for:
   *
   * {{{
   *   public class EvalIdzExpFactory {
   *
   *     public static EvalIdzExp Neg(EvalIdzExp inner) {
   *         return new EvalIdzNeg(inner);
   *     }
   *
   *     public static EvalIdzExp Mult(EvalIdzExp left, EvalIdzExp right) {
   *         return new EvalIdzMult(left, right);
   *     }
   * }}}
   *
   * Can't set as abstract because later one might have default methods which can't be both default/abstract.
   *
   * Might require generics for the class.
   * @param model
   * @param tpeCase
   * @return
   */
  def createFactorySignatureDataTypeCase(model:GenericModel, tpeCase:DataTypeCase, paramBaseClass:Type, returnClass:Type, isStatic:Boolean = false): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._
    for {
      _ <- resolveAndAddImport(paramBaseClass)
      _ <- setReturnType(returnClass)
      _ <- if (isStatic) { setStatic() } else { Command.skip[MethodBodyContext] }
      params <- forEach (tpeCase.attributes) { att: Attribute => {
          if (tpeCase.isRecursive(model)) {
            for {
              pName <- freshName(names.mangle(names.instanceNameOf(att)))

            } yield (pName, paramBaseClass)
          } else {
            for {
              at <- toTargetLanguageType(att.tpe)

              pName <- freshName(names.mangle(names.instanceNameOf(att)))
            } yield (pName, at)
          }
        }
      }

      _ <- setParameters(params)

    } yield None
  }

  /**
   * When factory-like methods need to be generated for a class based upon a dataTypeCase, this function
   * does most of the heavy lifting.
   *
   * Return type can be overridden by [[factoryNameDataTypeCase]]
   * Instantiated object internally can be overridden by [[factoryInstanceDataTypeCase]]
   *
   * Trivially requires the following in its test cases:
   *
   * {{{
   * AddPrettypFinal Add(PrettypExp left, PrettypExp right) {
   *   return new AddPrettypFinal(left, right);
   * }
   *
   * LitPrettypFinal Lit(Double v) {
   *    return new LitPrettypFinal(v);
   * }
   * }}}
   *
   * While interpreter calls for:
   *
   * {{{
   *   public class EvalIdzExpFactory {
   *
   *     public static EvalIdzExp Neg(EvalIdzExp inner) {
   *         return new EvalIdzNeg(inner);
   *     }
   *
   *     public static EvalIdzExp Mult(EvalIdzExp left, EvalIdzExp right) {
   *         return new EvalIdzMult(left, right);
   *     }
   * }}}
   *
   * base class might require type parameter, i.e., Exp<V>, so we must pass it in here since we can't add types
   * once we get into a MethodBodyContext.
   * @param model
   * @param tpeCase
   * @return
   */
  def createFactoryDataTypeCase(model:GenericModel, tpeCase:DataTypeCase, paramBaseType:Type, returnType:Type, isStatic:Boolean = false): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      _ <- createFactorySignatureDataTypeCase(model, tpeCase, paramBaseType, returnType, isStatic)

      opInst <- findClass(factoryInstanceDataTypeCase(Some(model), tpeCase): _*)    // should check!

      argSeq <- getArguments().map( args => { args.map(triple => triple._3) })
      res <- instantiateObject(opInst, argSeq)

    } yield Some(res)
  }

  def createStaticFactoryDataTypeCase(model:Model, tpeCase:DataTypeCase, paramBaseType:Type, returnType:Type): Generator[MethodBodyContext, Option[Expression]] = {
     createFactoryDataTypeCase(model, tpeCase, paramBaseType, returnType, true);
  }

}
