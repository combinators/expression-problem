package org.combinators.ep.approach.oo    /*DI:LI:AD*/

import org.combinators.ep.domain.GenericModel
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.{FindClass, ObjectOriented}
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider, Understands}

trait SharedOO extends ApproachImplementationProvider {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]

  import ooParadigm._
  import paradigm._
  import syntax._


  import paradigm._
  import syntax._

  /**
   * Default registration for findClass, which works with each registerTypeMapping for the different approaches.
   *
   * Sometimes the mapping is fixed for an EP approach, but sometimes it matters when a particular class is requested
   * in the evolution of the system over time.
   *
   * @param dtpe           Base Data Type.
   * @param canFindClass   Ability to locate a class.
   * @tparam Ctxt          Made generic by having context as a type parameter.
   * @return               Get the class.
   */
  def domainTypeLookup[Ctxt](dtpe: DataType)(implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]]): Generator[Ctxt, Type] = {
    FindClass(Seq(names.mangle(names.conceptNameOf(dtpe)))).interpret(canFindClass)
  }

  /** Provides meaningful default solution to find the base data type in many object-oriented approaches.
   *
   * This enables target-language classes to be retrieved from within the code generator in the Method, Class or Constructor contexts.
   */
  def registerTypeMapping(domain: GenericModel): Generator[ProjectContext, Unit] = {
    import paradigm.projectCapabilities.addTypeLookupForMethods
    import ooParadigm.methodBodyCapabilities.canFindClassInMethod
    import ooParadigm.projectCapabilities.addTypeLookupForClasses
    import ooParadigm.projectCapabilities.addTypeLookupForConstructors
    import ooParadigm.classCapabilities.canFindClassInClass
    import ooParadigm.constructorCapabilities.canFindClassInConstructor
    val dtpeRep = TypeRep.DataType(domain.baseDataType)
    for {
      _ <- addTypeLookupForMethods(dtpeRep, domainTypeLookup(domain.baseDataType))
      _ <- addTypeLookupForClasses(dtpeRep, domainTypeLookup(domain.baseDataType))
      _ <- addTypeLookupForConstructors(dtpeRep, domainTypeLookup(domain.baseDataType))
    } yield ()
  }

  /** Return standard method name getATTRIBUTE for given attribute. */
  def getterName(att:Attribute):Name = {
    names.addPrefix("get", names.mangle(names.conceptNameOf(att)))
  }

  /** Make a field from an attribute in the given class.  If the type needs to be different from default, then register Types accordingly. */
  def makeField(att: Attribute): Generator[ClassContext, Type] = {
    import ooParadigm.classCapabilities._
    for {
      ft <- toTargetLanguageType(att.tpe)
      _ <- resolveAndAddImport(ft)
      _ <- addField(names.mangle(names.instanceNameOf(att)), ft)
    } yield ft
  }

  /** Create standard signature to access the result of an operation
   *
   * {{{
   *   public Double OPERATION(PARAM...)
   * }}}
   * @param op     Operation for whom to make the signature.
   * @return       Return the full method body.
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


  def latestModelDefiningOperatorClass(domain: GenericModel, tpeCase:DataTypeCase, op:Operation, domainSpecific: EvolutionImplementationProvider[this.type]) : Option[GenericModel] = {
    // Find all domains with an EIP that implements op for any type case
    val domainsImplementingOp = domainSpecific.evolutionSpecificDependencies(PotentialRequest(domain.baseDataType, tpeCase, op)).keySet

    def cmp(l: GenericModel, r: GenericModel) = {
      if (l.before(r)) -1 else if (r.before(l)) 1 else 0
    }

    def futureMergePoint(l: GenericModel, r: GenericModel)(m: GenericModel): Boolean = {
      l.beforeOrEqual(m) && r.beforeOrEqual(m)
    }

    val orderedImplementers = domainsImplementingOp.toSeq
      .filter(d => d.beforeOrEqual(domain)) // filter to make sure we are before the current domain (we are not interested in later EIPs)
      .sorted(cmp)
      .reverse

    // Are there two non-comparable ancestors l, r that haven't been merged by a third m which is past both? Then we are
    // responsible for the merge!
    if (orderedImplementers.size > 1 && orderedImplementers.exists(l => orderedImplementers.exists(r =>
      cmp(l, r) == 0 && !orderedImplementers.exists(futureMergePoint(l, r))))
    ) {
      return Some(domain)
    }
    Some(orderedImplementers.head)     // latest one
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
   * @param tpe             Need the base Data Type.
   * @param tpeCase         For the specific data type case.
   * @param op              That has an operation to be implemented.
   * @param domainSpecific  And the logic can be found in the EIP.
   * @return                Return the body of the method.
   */
  def makeImplementation(
                          tpe: DataType,
                          tpeCase: DataTypeCase,
                          op: Operation,
                          model: GenericModel,
                          domainSpecific: EvolutionImplementationProvider[this.type]
                        ): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    val properModel = latestModelDefiningOperatorClass(model, tpeCase, op,  domainSpecific).get

    for {
      _ <- makeSignature(op)
      thisRef <- selfReference()
      attAccessors <- forEach (tpeCase.attributes) { att =>
        getMember(thisRef, names.mangle(names.instanceNameOf(att)))
      }
      atts = tpeCase.attributes.zip(attAccessors).toMap
      allArgs <- getArguments()
      args = op.parameters.zip(allArgs).map { case (param, arg) => (param, arg._3) }.toMap
      result <-
        domainSpecific.logic(this)(
          ReceivedRequest(
            tpe,
            tpeCase,
            thisRef,
            atts,
            Request(op, args),
            Some(properModel)
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
   * @param tpeCase      DataTypeCase that needs a constructor.
   * @param initFields   Do the fields need to be initialized?
   * @param useSuper     Should constructor invoke its super()?
   * @return
   */
  def makeConstructor(tpeCase: DataTypeCase, initFields:Boolean = true, useSuper:Option[Type] = Option.empty): Generator[ConstructorContext, Unit] = {
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

      _ <- if (useSuper.isDefined) {
        initializeParent(useSuper.get, args.map(p => p._3))
      } else {
        Command.skip[ConstructorContext]
      }

      _ <- if (initFields) {
        forEach(tpeCase.attributes.zip(args)) { case (att, (_, _, exp)) =>
          initializeField(names.mangle(names.instanceNameOf(att)), exp)
        }
      } else {
        Command.skip[ConstructorContext]
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
   * @param att      The Attribute for which a getAtt() method is needed.
   * @return         Return the implementation.
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
   * @param att     The attribute for which a getter is needed.
   * @return        The implemented class.
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
    addMethod(getterName(att), makeBody)
  }

}
