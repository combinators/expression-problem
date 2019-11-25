package org.combinators.ep.approach.oo

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.communication.{ReceivedRequest, Request}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.{FindClass, ObjectOriented, ToTargetLanguageType}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider, Understands}

trait SharedOO extends ApproachImplementationProvider {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]

  import ooParadigm._
  import paradigm._
  import syntax._

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

  def makeBase(tpe: DataType, ops: Seq[Operation]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- setAbstract()
        _ <- forEach(ops) { op => addAbstractMethod(names.mangle(names.instanceNameOf(op)), makeSignature(op)) }
      } yield ()
    }
    addClassToProject(names.mangle(names.conceptNameOf(tpe)), makeClass)
  }

  /** Return Signature of getXXX() for an attribute. Can be made covariant as needed. Not abtstract; caller decides what to do. */
  def makeGetterSignature(att:Attribute): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for {
      pt <- toTargetLanguageType(att.tpe)   // PULL OUT of the context
      _ <- setReturnType(pt)
      // _ <- setAbstract()
    } yield (None)
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
   * Create constructor for a class based on the given data type.
   *
   * {{{
   *   public ?(ATT-TPE _left, ATT-TPE _right) {
   *         this.left = _left;
   *         this.right = _right;
   *     }
   * }}}
   *
   * Ultimately would provide a function that converts att.tpe into ATT-TPE, for example, as needed
   * by Trivially
   * @param tpeCase
   * @return
   */
  def makeConstructor(tpeCase: DataTypeCase): Generator[ConstructorContext, Unit] = {
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
      _ <- forEach(tpeCase.attributes.zip(args)) { case (att, (_, _, exp)) =>
        initializeField(names.mangle(names.instanceNameOf(att)), exp)
      }
    } yield ()
  }


  /** Make a single getter method for the 'att' attribute, such as:
   * {{{
   * public abstract Exp getRight();
   * }}}
   *
   * parameterized, as necessary, with attToType method that overrides default behavior
   * @param att
   * @return
   */
  def makeGetterInterface(att:Attribute): Generator[MethodBodyContext, Option[Expression]] = {
     import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      for {
        rt <- toTargetLanguageType(att.tpe)
        _ <- resolveAndAddImport(rt)
        _ <- setReturnType(rt)
      } yield (None)
    }

  /** Make a single getter method for the 'att' attribute, such as:
   * {{{
   * public Exp getRight() {
   *   return this.right;
   * }
   * }}}
   *
   * parameterized, as necessary, with attToType method that overrides default behavior
   * @param att
   * @return
   */
  def makeGetter(att:Attribute): Generator[ClassContext, Unit] = {
    val makeBody: Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._

      for {
        _ <- makeGetterInterface(att)

        self <- selfReference()
        result <- getMember(self, names.mangle(names.instanceNameOf(att)))
      } yield Some(result)
    }

    import ooParadigm.classCapabilities._
    addMethod(names.addPrefix("get", names.mangle(names.conceptNameOf(att))), makeBody)
  }

  def domainTypeLookup[Ctxt](dtpe: DataType)(implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]]): Generator[Ctxt, Type] = {
    FindClass(names.mangle(names.conceptNameOf(dtpe))).interpret(canFindClass)
  }

}
