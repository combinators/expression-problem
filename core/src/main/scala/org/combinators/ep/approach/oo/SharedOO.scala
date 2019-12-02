package org.combinators.ep.approach.oo

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.communication.{ReceivedRequest, Request}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.ObjectOriented
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

trait SharedOO extends ApproachImplementationProvider {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]

  import ooParadigm._
  import paradigm._
  import syntax._

  /** Create standard signature to access the result of an operation
   *
   * {{{
   *   public Double OPERATION(PARAM...)
   * }}}
   * @param op
   * @return
   */
  def makeSignature(op: Operation): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._

    for {
      rt <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(rt)
      _ <- setReturnType(rt)
      params <- forEach (op.parameters) { param: Parameter =>
        for {
          pt <- toTargetLanguageType(param.tpe)
          _ <- resolveAndAddImport(pt)
          pName <- freshName(names.mangle(param.name))
        } yield (pName, pt)
      }
      _ <- setParameters(params)
    } yield ()
  }

  /**
   * Provides a method implementation that contains the logic of the operation encoded
   * as a single method.
   *
   * {{{
   *   public Double eval() {
   *      return getLeft().eval() + getRight().eval();
   *   }
   * }}}
   *
   * @param tpe
   * @param tpeCase
   * @param op
   * @param domainSpecific
   * @return
   */
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
        getMember(thisRef, names.mangle(names.instanceNameOf(att)))
      }
      atts = tpeCase.attributes.zip(attAccessors).toMap
      allArgs <- getArguments()
      args = op.parameters.zip(allArgs).map { case (param, (_, _, exp)) => (param, exp) }.toMap
      result <-
        domainSpecific.logic(this)(
          ReceivedRequest(
            tpe,
            tpeCase,
            thisRef,
            atts,
            Request(op, args)
          )
        )
    } yield result
  }

  /**
   * Create constructor for a class based on the given data type. The name of the class is not defined
   * yet. Every attribute for the [[DataTypeCase]] is converted to the targetLanguage, and the parameters
   * to the constructor are used to initialize the fields for the class.
   *
   * An example for the Add DataType Case, which is a binary data type with a left and right attribute.
   * {{{
   *   public ?(ATT-TPE _left, ATT-TPE _right) {
   *         this.left = _left;
   *         this.right = _right;
   *   }
   * }}}
   *
   * Ultimately would provide a function that converts att.tpe into ATT-TPE, for example, as needed
   * by Trivially.
   *
   * @param tpeCase
   * @return
   */
  def makeConstructor(tpeCase: DataTypeCase, useSuper:Boolean = false): Generator[ConstructorContext, Unit] = {
    import ooParadigm.constructorCapabilities._

    for {
      params <- forEach (tpeCase.attributes) { att: Attribute =>
        for {
          at <- toTargetLanguageType(att.tpe)
          pName <- freshName(names.mangle(names.instanceNameOf(att)))
        } yield (pName, at)
      }

      _ <- setParameters(params)
      args <- getArguments()
      _ <- if (useSuper) {
        initializeParent(args.map(p => p._3))
      } else {
        forEach(tpeCase.attributes.zip(args)) { case (att, (_, _, exp)) =>
          initializeField(names.mangle(names.instanceNameOf(att)), exp)
        }
      }
    } yield ()
  }


  /**
   *  Make a single getter method for the 'att' attribute which only has signature, and no body.
   *
   * The following example is for the 'Right' attribute where the type is mapped to Exp
   * {{{
   * public Exp getRight();
   * }}}
   *
   * Caller can decide whether to make this abstract or leave concrete.
   * @param att
   * @return
   */
  def makeGetterSignature(att:Attribute): Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      for {
        rt <- toTargetLanguageType(att.tpe)
        _ <- resolveAndAddImport(rt)
        _ <- setReturnType(rt)
      } yield None
    }

  /**
   * Make a single getter method for the 'att' attribute with a body that returns the associaed field's value.
   *
   * {{{
   * public Exp getRight() {
   *   return this.right;
   * }
   * }}}
   *
   * Directly access field attribute.
   *
   * @param att
   * @return
   */
  def makeGetter(att:Attribute): Generator[ClassContext, Unit] = {
    val makeBody: Generator[MethodBodyContext, Option[Expression]] = {
      import ooParadigm.methodBodyCapabilities._

      for {
        _ <- makeGetterSignature(att)

        self <- selfReference()
        result <- getMember(self, names.mangle(names.instanceNameOf(att)))
      } yield Some(result)
    }

    import ooParadigm.classCapabilities._
    addMethod(names.addPrefix("get", names.mangle(names.conceptNameOf(att))), makeBody)
  }
}
