package org.combinators.ep.approach.oo    /*DI:LI:AD*/

import org.combinators.ep.domain.GenericModel
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._
import org.combinators.ep.generator.paradigm._
import org.combinators.ep.generator.paradigm.control.Imperative

/**
 * CoCo Framework as submitted for publication.
 *
 *  HACK HACK -- had to hack java/ObjectOriented.scala for some reason. HELP!
 *  Minor Nitpick - can we avoid (a) generating public methods in public interfaces; (b) in an interface,
 *  methods are "public abstract" which is redunant and can be removed
 */

trait CoCo extends OOApproachImplementationProvider with BaseDataTypeAsInterface with SharedOO with OperationInterfaceChain with FieldDefinition with FactoryConcepts {
  import ooParadigm._
  import paradigm._
  import syntax._

  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]
  val impParadigm: Imperative.WithBase[MethodBodyContext,paradigm.type]

  lazy val finalized:Name = names.mangle("finalized")     // sub package within each evolution that contains final classes
  lazy val expTypeParameter:Name = names.mangle("FT")
  lazy val getSelf:Name = names.mangle("getSelf")
  lazy val Factory:Name = names.mangle("Factory")
  lazy val toConvert = names.mangle("other")   // looks better this way

  lazy val convert: Name = names.mangle("convert")

  /** Placeholder for the ancestral type ep.Exp so it can be registered separately within the type mapping */
  lazy val ancestralTypePrefix:String = "ancestor"

  // figure out WHAT to do about the ancestral type? Seem useful but I'm not really using it properly!

  // dup from FutureVisitor (needs refactoring!)
  /** Same as below but works without domain, providing you pass in the base type. */
  def computedBaseType[Context](ofBaseType:DataType)(implicit canFindClass: Understands[Context, FindClass[Name, Type]]): Generator[Context, Type] = {
    FindClass(Seq(names.mangle(names.conceptNameOf(ofBaseType)))).interpret(canFindClass)
  }

  // names.mangle(domain.name), finalized, names.mangle(names.conceptNameOf(domain.baseDataType))
  def finalizedBase[Context](model:GenericModel)(implicit canFindClass: Understands[Context, FindClass[Name, Type]]): Generator[Context, Type] = {
    FindClass(Seq(names.mangle(model.name), finalized, names.mangle(names.conceptNameOf(model.baseDataType)))).interpret(canFindClass)
  }

  def finalizedFactory[Context](model:GenericModel)(implicit canFindClass: Understands[Context, FindClass[Name, Type]]): Generator[Context, Type] = {
    FindClass(Seq(names.mangle(model.name), finalized, Factory)).interpret(canFindClass)
  }

  // dup from FutureVisitor (needs refactoring!)
  /** Returns the base type of trivially. */
  def computedBaseType[Context](ofModel: GenericModel)(implicit canFindClass: Understands[Context, FindClass[Name, Type]]): Generator[Context, Type] = {
    computedBaseType(ofModel.baseDataType)
  }

  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      self <- selfReference()
      convert <- getMember(self, convert)  // message.to
      converted <- apply(convert, Seq(message.to))
      method <- getMember(converted, names.mangle(names.instanceNameOf(message.request.op)))

      result <- apply(method, message.request.op.parameters.map(message.request.arguments))
    } yield result
  }

  /**
   * Instantiating an instance of the data type.
   *
   * public void testTest() {
   *   org.junit.Assert.assertTrue("", Double.valueOf(new Add(new Lit(1.0), new Lit(2.0)).eval()).equals(3.0));
   *   org.junit.Assert.assertTrue("", Double.valueOf(new Lit(5.0).eval()).equals(5.0));
   * }
   *
   * @param baseTpe
   * @param tpeCase
   * @param args
   * @return
   */
  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      thisRef <- selfReference()
      factory <- getMember(thisRef, names.mangle(names.instanceNameOf(tpeCase)))
      res <- apply(factory, args)
    } yield res
  }

  // critical.... new factory finalized classes are stored in proper package
  override def factoryInstanceDataTypeCase(model:Option[GenericModel] = None, tpeCase:DataTypeCase) : Seq[Name] = {
    model.map(m => names.mangle(m.name)).toSeq :+ finalized :+ names.mangle(names.conceptNameOf(tpeCase))
  }

  /**
   *
   */
  def makeConvertSignature(from: Type, to:Type): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for {
      _ <- setParameters(Seq((toConvert, from)))
      _ <- setReturnType(to)
    } yield None
  }

  /**
   * Defines an operation and must also insert parameters when they exist.
   *
   * public default Boolean equals(ep.Exp<V> other) {*
   *   return this.astree().equals(convert(other).astree());
   * }
   *
   * Be careful to call convert on recursive types
   *
   * Special logic for CoCo ensures that the default producer method implementations are used when domainSpecific
   * is not applicable, otherwise they are overriden.
   *
   * @param tpe
   * @param tpeCase
   * @param op
   * @param domainSpecific
   * @return
   */
  def makeCoCoImplementation(model:GenericModel, tpe: DataType,
                             tpeCase: DataTypeCase,
                             op: Operation,
                             domainSpecific: EvolutionImplementationProvider[this.type]
                            ): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._

    def producerConvert(applicableModel:GenericModel) : Generator[MethodBodyContext, Option[Expression]] = {
      import ooParadigm.methodBodyCapabilities._
      import paradigm.methodBodyCapabilities._
      for {
        ft <- getTypeArguments()
        _ <- triviallyMakeSignature(tpe, ft, op, op.isProducer(applicableModel))
        thisRef <- selfReference()
        convertMethod <- getMember(thisRef, convert)

        argSeq <- getArguments().map( args => { args.map(triple => triple._3) })

        // M0 <- M1 [producerOp] <- M2
        //        \                   \
        //          Ma <---------------Merge{m2,Ma} <- M3

        // find which past branch contains the operation? Go to the last point where op was defined BUT be
        // careful if any branch has chosen to override the operation in a way (not just forwarded). So
        // new strategy is (a) where Mult was first declared; or (b) simplify was first declared for Mult.
        //       return this.convert(ep.m4.Mult.super.simplify());
        // Java Restriction: Turns out you cannot bypass the more direct superclass. So you are left with
        // checking which past branch to go through

        // should work for merging?
        superRef <- superReference(names.mangle(applicableModel.name), names.mangle(names.conceptNameOf(tpeCase)))  // TODO: HAVE TO FIX THIS
        opMethod <- getMember(superRef, names.mangle(names.instanceNameOf(op)))
        innerResult <- apply(opMethod, argSeq)
        result <- if (op.isProducer(applicableModel)) {                    // only have to convert for producer methods.
          apply(convertMethod, Seq(innerResult))
        } else {
          Command.lift[MethodBodyContext,Expression](innerResult)
        }
      } yield Some(result)
    }

    for {
      thisRef <- selfReference()
      attAccessors: Seq[Expression] <- forEach (tpeCase.attributes) { att =>
        for {
          getter <- getMember(thisRef, getterName(att))
          getterCall <- apply(getter, Seq.empty)
        } yield getterCall
      }

      ft <- getTypeArguments()
      _ <- triviallyMakeSignature(tpe, ft, op, op.isProducer(model))

      args <- getArguments()

      convertMethod <- getMember(thisRef, convert)

      processedArgs <- forEach (args.zip(op.parameters)) { argPair =>
        for {
          // must convert recursive using convert() invocations
          pArg <- if (argPair._2.tpe == TypeRep.DataType(tpe)) {
            // must invoke current convert()
            apply(convertMethod, Seq(argPair._1._3))   // invoke convert() on the expression
          } else {
            Command.lift[MethodBodyContext, Expression](argPair._1._3)
          }
        } yield (argPair._2, pArg)
      }

      receivedRequest = ReceivedRequest(
        tpe,
        tpeCase,
        thisRef,
        tpeCase.attributes.zip(attAccessors).toMap,
        Request(op, processedArgs.toMap)
      )

      _ <- debug(receivedRequest.request.op.name + "@" + receivedRequest.tpeCase.name + " for " + model.name)
      applicableModel = domainSpecific.applicableIn(this)(receivedRequest, model).get

      // TODO: must call convert(baseType) or pass literals through
      // if applicable model is self then WE want to do something, so we have to delegate to EIP
      // to provide implementation; otherwise we fall back to default
      result <- if (applicableModel == model) {
        domainSpecific.logic(this)(receivedRequest)
      } else {

        // NOW have to find which of my former branches because this is a producer op
        producerConvert(model.former.find(gm => applicableModel.before(gm) || gm == applicableModel).get)
      }

    } yield result
  }

  def derivedInterfaceName(tpe: DataTypeCase): Name = {
    names.mangle(tpe.name)
  }

  /** Former derived interfaced for the tpe must be qualified with model package */
  def getFormerDerivedInterfaces(domainDefiningType: GenericModel, current:DataTypeCase): Generator[ClassContext, List[Type]] = {
    import classCapabilities._

    //val pastTypes = domainDefiningType.former.filter(!_.isDomainBase).filter(m => m.findTypeCase(current).isDefined)
    val whereDefined = domainDefiningType.findTypeCase(current).get
    // if a former operation is defined between domainDefiningTypes and whereDefined, take that instead
    val withOps = domainDefiningType.former.flatMap(prior => prior.lastModelWithOperation).distinct

    val finalSet = withOps.map(pm => {
      if (whereDefined.before(pm)) { pm } else {whereDefined}
    })
    for {
      // only take those who are not the bottom BUT ALSO only those for whom the current is meaningful.
      // COCO: May have to go back further to an operation-level in which this type was defined
//      group <- forEach(domainDefiningType.former.filter(!_.isDomainBase).filter(m => m.findTypeCase(current).isDefined)) { prior =>
//        findClass(names.mangle(prior.name), derivedInterfaceName(current))
//      }
      group <- forEach(finalSet) { prior =>
                findClass(names.mangle(prior.name), derivedInterfaceName(current))
        }

    } yield group

    //    findClass(names.mangle(domainDefiningType.former.get.name), derivedInterfaceName(current))
  }

  /**
   * For any datatype that *could* have been defined in domain or perhaps was defined earlier
   */
  def makeDerivedInterfaces(tpeCase:DataTypeCase, currentModel:GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val ddn = derivedInterfaceName (tpeCase)
    addClassToProject(makeDerivedInterface(currentModel.baseDataType, tpeCase, currentModel, domainSpecific), names.mangle(currentModel.name), ddn)
  }

  /**
   * Pivotal concept in CoCo is to find the "proper finalized Type" to use. This depends on several factors, as encapsulated here.
   * @param domain
   * @param att
   * @return
   */
  def properFinalizedType(domain:GenericModel,att:Attribute): Generator[MethodBodyContext, Type] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      pt <- if (names.conceptNameOf(att.tpe) == names.conceptNameOf(domain.baseDataType)) {
        if (domain.ops.nonEmpty) {
          finalizedBase(domain)
        } else {
          findClass(names.mangle(domain.lastModelWithOperation.head.name), finalized, names.mangle(names.conceptNameOf(domain.baseDataType)))
        }
      } else {
        toTargetLanguageType(att.tpe)
      }
    } yield pt
  }

  /**
   * Pivotal concept in CoCo is to find the "proper finalized Type" to use. This depends on several factors, as encapsulated here.
   * @param domain
   * @param att
   * @return
   */
  def properFinalizedTypeConstructor(domain:GenericModel,att:Attribute): Generator[ConstructorContext, Type] = {
    import ooParadigm.constructorCapabilities._
    for {
      pt <- if (names.conceptNameOf(att.tpe) == names.conceptNameOf(domain.baseDataType)) {
        if (domain.ops.nonEmpty) {
          finalizedBase(domain)
        } else {
          findClass(names.mangle(domain.lastModelWithOperation.head.name), finalized, names.mangle(names.conceptNameOf(domain.baseDataType)))
        }
      } else {
        toTargetLanguageType(att.tpe)
      }
    } yield pt
  }

  /**
   * Pivotal concept in CoCo is to find the "proper finalized Type" to use. This depends on several factors, as encapsulated here.
   * @param domain
   * @param att
   * @return
   */
  def properFinalizedTypeClass(domain:GenericModel,att:Attribute): Generator[ClassContext, Type] = {
    import ooParadigm.classCapabilities._
    for {
      pt <- if (names.conceptNameOf(att.tpe) == names.conceptNameOf(domain.baseDataType)) {
        if (domain.ops.nonEmpty) {
          finalizedBase(domain)
        } else {
          findClass(names.mangle(domain.lastModelWithOperation.head.name), finalized, names.mangle(names.conceptNameOf(domain.baseDataType)))
        }
      } else {
        toTargetLanguageType(att.tpe)
      }
    } yield pt
  }


  /**
   * Make a single getter method for the 'att' attribute, such as:
   * {{{
   * public abstract Exp getRight();
   * }}}
   *
   * parameterized, as necessary, with attToType method that overrides default behavior
   * @param att
   * @return
   */
  override def makeGetter(att:Attribute): Generator[ClassContext, Unit] = {
    val makeBody: Generator[MethodBodyContext, Option[Expression]] = {
      import ooParadigm.methodBodyCapabilities._

      for {
        _ <- makeGetterSignature(att)
        _ <- setAbstract()
      } yield None
    }

    import ooParadigm.classCapabilities._
    addMethod(getterName(att), makeBody)
  }

  /**
   * Create a derived class for a datatype for a model that has operations
   *
   * {{{
   *   package ep.m1
   *   public interface Sub<V> extends Exp<V> {
   *     public abstract Exp getLeft();
   *     public abstract Exp getRight();
   *
   *     default Double eval() {
   *         return getLeft().eval() - getRight().eval();
   *     }
   * }
   *
   * when subsequent we have
   *
   * package ep.m2
   * public abstract interface Lit<V> extends ep.m1.Lit<V>, ep.m2.Exp<V> {
   *     public abstract Double getValue();
   *
   *     public default String prettyp() {
   *         return String.valueOf(this.getValue());
   *     }
   * }
   * }}}
   *
   *  add another parent IF there is a prior operation defined before this model.
   *
   *  These are all defined in the "current" model. Note that if the operation is a producer method, then
   *  you need factory methods for all known data types.
   *
   * All producer methods (from the past!) most provide future-proof conversions
   *
   * public default ep.m5.Exp<V> simplify() {
   *    return convert(ep.m4.Add.super.simplify());
   * }
   *
   * Note: when a GenericModel merges multiple pathways, must find the one where this operation
   * is defined, and route that way. If defined in multiple branches, then either one is eligible!
   *
   * @param tpe
   * @param tpeCase
   * @param domainSpecific
   * @return
   */
  def makeDerivedInterface(tpe: DataType, tpeCase: DataTypeCase, model:GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._

      def extendParents(genType:Seq[Type]):Generator[ClassContext,Unit] = {
        import genericsParadigm.classCapabilities._

        // Go back to
        for {
          parentFormers <- getFormerDerivedInterfaces(model, tpeCase)
          _ <- forEach(parentFormers) { parentFormer =>
            for {
              _ <- resolveAndAddImport(parentFormer)
              paramType <- applyType(parentFormer, genType)
              _ <- addParent(paramType)
            } yield ()
          }
        } yield ()
      }

      import genericsParadigm.classCapabilities._
      import polymorphics.TypeParameterContext

      // If we are defining type, must output all methods for all past operations
      val opsToGenerate = if (model.typeCases.contains(tpeCase)) {
        model.flatten.ops
      } else {
        model.ops
      }

      for {
        // in new solution, all Exp<> references are parameterized
        ft <- freshName(expTypeParameter)  // HACK: MOVE UP
        _ <- addTypeParameter(ft, Command.skip[TypeParameterContext])
        genType <- getTypeArguments()

        // TODO: will need to deal with multiple parents
        parent <- if (model.ops.nonEmpty) {
          toTargetLanguageType(TypeRep.DataType(tpe))
        } else {
          findClass(names.mangle(model.lastModelWithOperation.head.name), names.mangle(names.conceptNameOf(model.baseDataType)))
        }

        _ <- resolveAndAddImport(parent)
        paramType <- applyType(parent, genType)
        _ <- registerLocally(tpe, paramType)

        // Wow. Ok, so this is the derived interface which, for operations that involve parameters, will need
        // to have access to the ancestral type. This is patched into the regular (locally!) type mapping
        // by defining a special entry that refers to ep.Exp<V> -- this is used later by makeImplementation(...)
        cbt <- computedBaseType(tpe)
        parameterizedBase <- applyType(cbt, genType)
        _ <- registerLocally(triviallyBaseDataType(tpe), parameterizedBase)

        // only add parent to the chain if NOT the first one OR defining type here
        // this second condition was added when we introduced GenericModel
        _ <- if (model.former.nonEmpty && !model.typeCases.contains(tpeCase)) {
          extendParents(genType)
        } else {
          Command.skip[ClassContext]
        }

        // always have to extend current Exp at this level
        _ <-  for {
          // first one needs to extend Exp
          // parent <-  toTargetLanguageType(TypeRep.DataType(tpe))
          //TODO: only when defining Exp. Might Break in M1... MERGE -- when MULTIPLE HAVE to go to current one.
          parent <- findClass(names.mangle(model.lastModelWithOperation.head.name), names.mangle(model.baseDataType.name))
          _ <- resolveAndAddImport(parent)
          paramType <- applyType(parent, genType)
          _ <- addParent(paramType)
        } yield ()

        // prepare for makeGetters. // TODO: HACK! NEEDS SOMETHING BETTER
//        expParent <- findClass(names.mangle(model.baseDataType.name))
//        _ <- resolveAndAddImport(expParent)

        _ <- if (model.typeCases.contains(tpeCase)) {
          forEach (tpeCase.attributes) { att => {
            for {
              mi <- makeGetter(att)
            } yield mi
          }
          }
        } else {
          Command.skip[ClassContext]
        }

        // TODO: Need To Deal With Merging "If my data type is new since if a branch that I do not belong to has a new operation, then
        // I need to incorporate that one"
        // Using new EIP capabilities, just generate
        //_ <- forEach (model.flatten.ops.distinct) { op =>
        _ <- forEach (opsToGenerate) { op =>
          addMethod(names.mangle(names.instanceNameOf(op)), makeCoCoImplementation(model, tpe, tpeCase, op, domainSpecific))
        }

        _ <- setInterface()  // do LAST because then methods with bodies are turned into default methods in interface
      } yield ()
    }

    makeClass
  }

  def makeBodyImpl(domain:GenericModel, baseType:DataType, att:Attribute): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      pt <- properFinalizedType(domain, att)
      _ <- setReturnType(pt)

      self <- selfReference()
      result <- getMember(self, names.mangle(names.instanceNameOf(att)))
    } yield Some(result)
  }

  /**
   * @return
   */
  def makeConvertImplementation(model:GenericModel, baseExp:Type): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._

    for {
      selfExp <- findClass(names.mangle(model.name), finalized, names.mangle(names.conceptNameOf(model.baseDataType)))
      _ <- setReturnType(selfExp)

      typedExp <- applyType(baseExp, Seq(selfExp))
      _ <- setParameters(Seq((toConvert, typedExp)))

      // only one argument.
      argSeq <- getArguments()
      other <- forEach(argSeq) { arg =>
        Command.lift[MethodBodyContext,Expression](arg._3)
      }

      getSelfMethod <- getMember(other.head, getSelf)
      res <- apply(getSelfMethod, Seq.empty)
    } yield Some(res)
  }

  /**
   * Add Method to class with given name
   *
   * @param methodName
   * @param bodyGenerator
   * @return
   */
  def addMethodToClass(methodName: Name, bodyGenerator:Generator[MethodBodyContext, Option[Expression]]): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    addMethod(methodName, bodyGenerator)
  }

  // TODO: if left unchecked, Exp => "Exp<FT>" so we need to match to finalized.
  /** Make a field from an attribute in the given class.  If the type needs to be different from default, then register Types accordingly. */
  def makeCoCoField(domain:GenericModel, att: Attribute): Generator[ClassContext, Type] = {
    import ooParadigm.classCapabilities._
    for {
      finalizedType <- properFinalizedTypeClass(domain, att)
      _ <- resolveAndAddImport(finalizedType)
      _ <- addField(names.mangle(names.instanceNameOf(att)), finalizedType)
    } yield finalizedType
  }

  // TODO: if left unchecked, Exp => "Exp<FT>" so we need to match this. Is there a way to remove a generic once there?
  def makeCoCoConstructor(domain:GenericModel, tpeCase: DataTypeCase, initFields:Boolean = true, useSuper:Boolean = false): Generator[ConstructorContext, Unit] = {
    import ooParadigm.constructorCapabilities._

    for {
      params <- forEach (tpeCase.attributes) { att: Attribute =>
        for {
          at <- properFinalizedTypeConstructor(domain, att)
          pName <- freshName(names.mangle(names.instanceNameOf(att)))
        } yield (pName, at)
      }

      _ <- setParameters(params)
      args <- getArguments()

      _ <- if (useSuper) {
        initializeParent(args.map(p => p._3))
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
   *
   * package ep.m0.finalized;
   *
   * public class Add implements ep.m0.Add<Visitor>, Factory {
   *   private Exp<Visitor> left;
   *   private Exp<Visitor> right;
   *
   *   public Add(Exp<Visitor> _left, Exp<Visitor> _right) {
   *     this.left = _left;
   *     this.right = _right;
   *   }
   *
   *   ...
   * }
   * HACK: Just duplicate and embed; figure out later.
   *
   * @param model
   * @param tpeCase
   * @return
   */
  def makeFinalClass(model:GenericModel, tpeCase: DataTypeCase): Generator[ProjectContext,Unit] = {
    import ooParadigm.projectCapabilities._

    val makeClass: Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._

      for {
        // if no operation is defined, then must go back to last one which is defined
        //selfExp <- findClass(names.mangle(model.name), finalized, names.mangle(names.conceptNameOf(model.baseDataType)))
        selfExp <- if (model.ops.nonEmpty) {
          findClass(names.mangle(model.name), finalized, names.mangle(names.conceptNameOf(model.baseDataType)))
        } else {
          findClass(names.mangle(model.lastModelWithOperation.head.name), finalized, names.mangle(names.conceptNameOf(model.baseDataType)))
        }
        _ <- addParent(selfExp)

        tpeExtension <- findClass(names.mangle(model.name), names.mangle(tpeCase.name))  // ep.m0.Lit for example
        tpeTyped <- applyType(tpeExtension, Seq(selfExp))
        _ <- addImplemented(tpeTyped)

        // these fields are no longer parameterized and should be Exp. BUT MAKE SURE USES PROPER FINALIZED EXP (ala selfExp above)
        _ <- forEach(tpeCase.attributes) { att => makeCoCoField(model, att) }
        _ <- addConstructor(makeCoCoConstructor(model, tpeCase))
        _ <- forEach(tpeCase.attributes) { att =>
          addMethod(getterName(att), makeBodyImpl(model, model.baseDataType, att))
        }
      } yield ()
    }

    addClassToProject(makeClass, factoryInstanceDataTypeCase(Some(model), tpeCase): _*)
  }

  /** For Trivially, the covariant type needs to be selected whenever a BaseType in the domain is expressed. */
  def domainTypeLookup[Ctxt](covariantType: Name*)(implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]]): Generator[Ctxt, Type] = {
    FindClass(covariantType).interpret(canFindClass)
  }

  /** Supports ability to re-register classes after they have acquired a type parameter. */
  def registerLocally(tpe:DataType, paramType:Type) : Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._

    val dtpeRep = TypeRep.DataType(tpe)

    for {
      _ <- addTypeLookupForMethods(dtpeRep, Command.lift(paramType))
      _ <- addTypeLookupForClasses(dtpeRep, Command.lift(paramType))
      _ <- addTypeLookupForConstructors(dtpeRep, Command.lift(paramType))

    } yield ()
  }

  /**
   * Allow for more fine-grained mapping for ancestral types
   */
  def triviallyBaseDataType(tpe:DataType): DataType = DataType(ancestralTypePrefix + tpe.name)

  /** What model is delivered has operations which is essential for the mapping. */
  override def registerTypeMapping(model: GenericModel): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    import ooParadigm.classCapabilities.canFindClassInClass
    import ooParadigm.constructorCapabilities.canFindClassInConstructor
    import ooParadigm.methodBodyCapabilities.canFindClassInMethod
    import paradigm.projectContextCapabilities._

    val baseInterface = Seq(names.mangle(model.baseDataType.name)) //baseInterfaceNames(model.standAlone)   // registers as m#.Exp   -- might not be necessary to convert standAlone
    val dtpeRep = TypeRep.DataType(model.baseDataType)

    for {
      _ <- addTypeLookupForMethods(dtpeRep, domainTypeLookup(baseInterface : _*))
      _ <- addTypeLookupForClasses(dtpeRep, domainTypeLookup(baseInterface : _*))
      _ <- addTypeLookupForConstructors(dtpeRep, domainTypeLookup(baseInterface : _*))
    } yield ()
  }

  // add type parameter to a class!
  def addFTTypeParameter(): Generator[ClassContext, Type] = {
    import classCapabilities._
    import genericsParadigm.classCapabilities._
    import polymorphics._
    for {
      ftTypeParamName <- freshName(expTypeParameter)
      _ <- addTypeParameter(ftTypeParamName, Command.skip[TypeParameterContext])
      ftType <- getTypeArguments().map(_.head)
    } yield ftType
  }

  /**
   * Instead of using the standard 'makeBase' we need to add accept and convert methods.
   *
   * Returns the fully qualified and not yet applied type to access this class, e.g. ep.Exp.
   */
  def makeCoCoBase(tpe: DataType, ops: Seq[Operation]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    def makeAbstractSelfSignature(ft: Type): Generator[MethodBodyContext, Option[Expression]] = {
      import ooParadigm.methodBodyCapabilities._
      import paradigm.methodBodyCapabilities._

      for {
        _ <- setReturnType(ft)
        _ <- setAbstract()
      } yield None
    }

    // at this point, we only want ep.Exp not the most qualified ep.m4.Exp
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      import genericsParadigm.classCapabilities._
      import polymorphics.TypeParameterContext
      for {
        ftType <- addFTTypeParameter()
        _ <- setInterface()

        _ <- addMethodToClass(getSelf, makeAbstractSelfSignature(ftType))
        factory <- findClass(Factory)
        factoryParam <- applyType(factory, Seq(ftType))
        _ <- addParent(factoryParam)
      } yield ()
    }
    addClassToProject(makeClass, names.mangle(names.conceptNameOf(tpe)))
  }

  /**
   * Factory now a stand-alone interface
   * Returns the fully qualified and not yet applied type to access this class, e.g. ep.Exp.
   */
  def makeCoCoFactory(tpe: DataType, ops: Seq[Operation]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    def makeAbstractConvertSignature(selfClassWithVisitorType: Type): Generator[MethodBodyContext, Option[Expression]] = {
      import ooParadigm.methodBodyCapabilities._
      import paradigm.methodBodyCapabilities._
      for {
        _ <- makeConvertSignature(selfClassWithVisitorType, selfClassWithVisitorType)
        _ <- setAbstract()
      } yield None
    }

    // at this point, we only want ep.Exp not the most qualified ep.m4.Exp
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      import genericsParadigm.classCapabilities._
      import polymorphics.TypeParameterContext
      for {
        ftType <- addFTTypeParameter()
        _ <- setInterface()
        selfClass <- findClass(names.mangle(names.conceptNameOf(tpe)))
        selfClassWithFTType <- applyType(selfClass, Seq(ftType))
        _ <- addMethodToClass(convert, makeAbstractConvertSignature(selfClassWithFTType))
      } yield ()
    }
    addClassToProject(makeClass, Factory)
  }

  /** Create standard signature to access the result of an operation
   *
   * {{{
   *   public Double OPERATION(PARAM...)
   * }}}
   *
   * @param baseType   which is necessary for the most ancestral type (and reason this method is not override of makeSignature)
   * @param op
   * @return
   */
  def triviallyMakeSignature(baseType:DataType, ft:Seq[Type], op: Operation, isProducer:Boolean): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {

      //rt <- toTargetLanguageType(op.returnType)
      rt <- if (isProducer) {
        findClass(names.mangle(baseType.name))  // needs to have <FT> applied to it
      } else {
        toTargetLanguageType(op.returnType)
      }

      _ <- resolveAndAddImport(rt)
      _ <- setReturnType(rt)

      params <- forEach (op.parameters) { param: Parameter =>
        for {
          pt <- if (param.tpe == TypeRep.DataType(baseType)) {
            toTargetLanguageType(TypeRep.DataType(triviallyBaseDataType(baseType)))
          } else {
            toTargetLanguageType(param.tpe)
          }
          _ <- resolveAndAddImport(pt)
          pName <- freshName(names.mangle(param.name))
        } yield (pName, pt)
      }
      _ <- setParameters(params)
    } yield ()
  }

  /** I had to copy this entire thing.
   *
   * is there any way to have one generator create a class and then another function can just ADD to it?
   *  */
  def makeCoCoInterface(domain:GenericModel, domainSpecific: EvolutionImplementationProvider[this.type], typeParameter:Option[Name] = None): Generator[ClassContext, Unit] = {
    // create class which is an interface containing abstract methods
    import classCapabilities._
    import genericsParadigm.classCapabilities._

    def extendParents:Generator[ClassContext, Unit] = {
      // HAVE TO GRAB LAST ONE THAT DEFINED OPERATION IN EACH OF FORMER
      //val formers:Seq[Seq[Name]] = domain.former.map(dom => baseInterfaceNames(dom))
      val formers:Seq[Seq[Name]] = if (domain.former.head.isBottom) {
        domain.former.map(dom => baseInterfaceNames(dom))
      } else {
        domain.former.map(dom => dom.lastModelWithOperation.flatMap(dom => baseInterfaceNames(dom)))
      }
        for {
        // get formers,filter all of those that "know" about the DataType, and then extend those
        _ <- forEach(formers) { former => {
          for {
            parent <- findClass(former: _*)
            _ <- resolveAndAddImport(parent)
            ft <- getTypeArguments().map(_.head)
            paramType <- applyType(parent, Seq(ft))

            _ <- resolveAndAddImport(paramType)
            _ <- addParent(paramType)
          } yield ()
        }
        }
      } yield ()

    }

    for {
      _ <- setInterface()

      _ <- if (typeParameter.isDefined) {
        for {
          _ <- addTypeParameter(typeParameter.get, Command.skip)
        } yield ()
      } else {
        Command.skip[ClassContext]
      }

      _ <- if (domain.former.nonEmpty) {
        extendParents
      } else {
        Command.skip[ClassContext]
      }
    } yield ()
  }

  def baseFactoryInterfaceNames(domain: GenericModel): Seq[Name] = {
    Seq(names.mangle(domain.name), Factory)
  }

  // extends Exp [first one] or ExpEval [previous one]
  // Works for both Exp* interface declarations as well as DataTypeOp declarations
  def getParentFactoryInterface(domain: GenericModel, tpe: DataType): Seq[Name] = {

    if (domain.isEmpty || domain.lastModelWithOperation.isEmpty) {
      Seq(Factory)
    } else {
      baseFactoryInterfaceNames(domain)
    }
  }

  /** I had to copy this entire thing. AGAIN from makeInterface
   *
   * is there any way to have one generator create a class and then another function can just ADD to it?
   *  */
  def makeFactoryInterface(domain:GenericModel, domainSpecific: EvolutionImplementationProvider[this.type], typeParameter:Option[Name] = None): Generator[ClassContext, Unit] = {
    // create class which is an interface containing abstract methods
    import classCapabilities._
    import genericsParadigm.classCapabilities._

    def extendParents:Generator[ClassContext, Unit] = {
      val formers:Seq[Seq[Name]] = domain.former.map(dom => getParentFactoryInterface(dom, dom.baseDataType))
      for {
        _ <- forEach(formers) { former =>
          for {
            parent <- findClass(former: _*)
            _ <- resolveAndAddImport(parent)
            justV <- getTypeArguments().map(_.head)
            paramType <- applyType(parent, Seq(justV))

            _ <- resolveAndAddImport(paramType)
            _ <- addParent(paramType)
          } yield ()
        }
      } yield ()
    }

    for {
      _ <- setInterface()

      _ <- if (typeParameter.isDefined) {
        for {
          _ <- addTypeParameter(typeParameter.get, Command.skip)
        } yield ()
      } else {
        Command.skip[ClassContext]
      }

      _ <- if (domain.former.nonEmpty) {
        extendParents
      } else {
        for {
          parent <- findClass(Factory)
          justV <- getTypeArguments().map(_.head)
          paramType <- applyType(parent, Seq(justV))

          //_ <- addConvertMethod(makeConvertSignature(paramType, paramType))
          _ <- addMethodToClass(convert, makeConvertSignature(paramType, paramType))
        } yield ()

        Command.skip[ClassContext]
      }
    } yield ()
  }

  /**
   * Starting with the operation chain and add the requisite interfaces for known factories.
   *
   * package ep.m4;
   *
   * public interface Exp<FT> extends ep.m2.Exp<FT>, Factory<FT> {
   *  public abstract ep.Exp<FT> simplify();
   *  public abstract List<Double> collect();
   * }
   */
  def extendIntermediateInterface(domain:GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._
    import polymorphics.TypeParameterContext

    for {
      _ <- makeCoCoInterface(domain, domainSpecific, Some(expTypeParameter))   // this creates TYPE

      // This block (up to forEach...) ensures that we are properly assigning ep.Exp<V> for future discovery
      // in createFactorySignatureDataTypeCase
      genType <- getTypeArguments()

      parent <-  toTargetLanguageType(TypeRep.DataType(domain.baseDataType))
      _ <- resolveAndAddImport(parent)
      paramType <- applyType(parent, genType)
      _ <- registerLocally(domain.baseDataType, paramType)

      // pull in factory
      parentFactory <- findClass(Factory)
      parentFactoryTyped <- applyType(parentFactory, genType)
      _ <- addParent(parentFactoryTyped)

      // Wow. Ok, so this is the derived interface which, for operations that involve parameters, will need
      // to have access to the ancestral type. This is patched into the regular (locally!) type mapping
      // by defining a special entry that refers to ep.Exp<V> -- this is used later by makeImplementation(...)
      cbt <- computedBaseType(domain.baseDataType)
      _ <- resolveAndAddImport(cbt)
      parameterizedBase <- applyType(cbt, genType)
      _ <- registerLocally(triviallyBaseDataType(domain.baseDataType), parameterizedBase)

      // if there are past operations, find those that are producers and create overloaded specifications
      // make sure not to duplicate by calling distinct. TODO: MuST CLEAN UP WHEN MERGING COMES IN
      _ <- forEach (domain.ops) { op => addAbstractMethod(names.mangle(names.instanceNameOf(op)), triviallyMakeSignature(domain.baseDataType, genType, op, op.isProducer(domain))) }
//      _ <- if (domain.former.nonEmpty) {
//        forEach(domain.former.flatMap(_.lastModelWithOperation.flatMap(_.flatten.ops).filter(_.isProducer(domain))).distinct) {
//          op => addAbstractMethod(names.mangle(names.instanceNameOf(op)), triviallyMakeSignature(domain.baseDataType, genType, op, op.isProducer(domain)))
//        }
//      } else {
//        Command.skip[ClassContext]
//      }

    } yield ()
  }

  /** Same as below but works without domain, providing you pass in the base type. */
  def computedFactoryType[Context]()(implicit canFindClass: Understands[Context, FindClass[Name, Type]]): Generator[Context, Type] = {
    FindClass(Seq(Factory)).interpret(canFindClass)
  }

  // TODO: Needs to return "ep.Exp<FT>" not "Exp<FT>" for all constructors and convert method
  def createFactorySignatureDataTypeCaseInFactory(model:GenericModel, tpeCase:DataTypeCase, paramBaseClass:Type): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      _ <- resolveAndAddImport(paramBaseClass)
      _ <- setReturnType(paramBaseClass)

      params <- forEach (tpeCase.attributes) { att => {
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
   * Starting with the operation chain and add the requisite interfaces for known factories.

        package ep.m0;
        import ep.Exp;

        public interface Factory<FT> extends ep.Factory<FT> {
            public abstract Exp<FT> lit(Double value);
            public abstract Exp<FT> add(Exp<FT> left, Exp<FT> right);
            public abstract ep.m0.Exp<FT> convert(Exp<FT> other);
        }

   * ONLY need newly defined types -- no longer do you need all
   */
  def extendFactory(domain:GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._
    import polymorphics.TypeParameterContext

    def convertMethod(topLevelType:Type, paramType:Type) : Generator[MethodBodyContext, Option[Expression]] = {
      import ooParadigm.methodBodyCapabilities._
      for {
        _ <- makeConvertSignature(topLevelType, paramType)    // TODO: WORKS!
        _ <- setAbstract()
      } yield None
    }

    for {
      _ <- makeFactoryInterface(domain, domainSpecific, Some(expTypeParameter))   // this creates TYPE

      // This block (up to forEach...) ensures that we are properly assigning ep.Exp<V> for future discovery
      // in createFactorySignatureDataTypeCase
      genType <- getTypeArguments()

      parent <-  toTargetLanguageType(TypeRep.DataType(domain.baseDataType))
      _ <- resolveAndAddImport(parent)
      paramType <- applyType(parent, genType)
      _ <- registerLocally(domain.baseDataType, paramType)

      // Wow. Ok, so this is the derived interface which, for operations that involve parameters, will need
      // to have access to the ancestral type. This is patched into the regular (locally!) type mapping
      // by defining a special entry that refers to ep.Exp<V> -- this is used later by makeImplementation(...)
      cbt <- computedBaseType(domain.baseDataType)
      _ <- resolveAndAddImport(cbt)
      parameterizedBase <- applyType(cbt, genType)
      _ <- registerLocally(triviallyBaseDataType(domain.baseDataType), parameterizedBase)

      // paramType is now Exp<V>. Couldn't get type arguments?
      parent <- findClass(names.mangle(domain.name), names.mangle(domain.baseDataType.name))
      ft <- getTypeArguments().map(_.head)
      paramType <- applyType(parent, Seq(ft))

      tt <- computedBaseType(domain)
      topLevelType <- applyType(tt, Seq(ft))

      // add factory methods -- make sure to eliminate duplicates, in case there are merged branches.
      // these are factory signatures.
      //_ <- forEach (domain.flatten.typeCases.distinct) { tpe => {
      _ <- forEach (domain.typeCases.distinct) { tpe => {
        // make this a helper capability in SharedOO -- ability to take an existing methodBodyContext and make it abstract...
        val absMethod: Generator[MethodBodyContext, Option[Expression]] = {
          import ooParadigm.methodBodyCapabilities._
          for {
            // return value for all methods needs to be ep.Exp<FT> as was the case for parameters
            xf <- createFactorySignatureDataTypeCaseInFactory(domain, tpe, topLevelType)
            _ <- setAbstract()
          } yield xf
        }

        addMethod(names.mangle(names.instanceNameOf(tpe)), absMethod)
      }
      }

      // ONLY need when an operation is defined
      _ <- if (domain.ops.nonEmpty) {
        addMethodToClass(convert, convertMethod(topLevelType, paramType))
      } else {
        Command.skip[ClassContext]
      }
    } yield ()
  }

  /**
   * When factory-like methods need to be generated for a class based upon a dataTypeCase, this function
   * does most of the heavy lifting.
   *
   * Can't set as abstract because later one might have default methods which can't be both default/abstract.
   *
   * Might require generics for the class.
   * @param model
   * @param tpeCase
   * @return
   */
  def createCoCoFactorySignatureDataTypeCase(model:GenericModel, tpeCase:DataTypeCase): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._
    for {
      // might have to go back to EARLIER if no new operations defined
      // TODO: IF MULTIPLE PAST HAVE OPERATIONS CAN'T JUST GRAB FIRST AS I DO HERE. HELP! MIGHT HAVE TO GO BACK TO MERGE OR LAST OP
      selfExp <- if (model.ops.nonEmpty) {
        findClass(names.mangle(model.name), finalized, names.mangle(names.conceptNameOf(model.baseDataType)))
      } else {
        findClass(names.mangle(model.lastModelWithOperation.head.name), finalized, names.mangle(names.conceptNameOf(model.baseDataType)))
      }

      _ <- setReturnType(selfExp)
      //_ <- if (isStatic) { setStatic() } else { Command.skip[MethodBodyContext] }
      params <- forEach (tpeCase.attributes) { att: Attribute => {
        if (tpeCase.isRecursive(model)) {
          for {
            pName <- freshName(names.mangle(names.instanceNameOf(att)))
            rt <- toTargetLanguageType(att.tpe)
            _ <- resolveAndAddImport(rt)

            baseExp <- findClass(names.mangle(model.baseDataType.name))
            typedExp <- applyType(baseExp, Seq(selfExp))

          } yield (pName, typedExp)
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
   *??? lit(Double value) {
   *   return new Lit(value);
   * }
   *
   *??? (ep.Exp<???> left, ep.Exp<???> right) {
   *   return new Add(this.convert(left), this.convert(right));
   * }
   */
  def futureCreateFactoryDataTypeCase(model:GenericModel, tpeCase:DataTypeCase): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      _ <- createCoCoFactorySignatureDataTypeCase(model, tpeCase)

      opInst <- findClass(factoryInstanceDataTypeCase(Some(model), tpeCase): _*)    // should check!
      _ <- resolveAndAddImport(opInst)

      // set method invocation to 'convert' with these arguments
      self <- selfReference()
      convertMethod <- getMember(self, convert)
      argSeq <- getArguments()

      convertedArgSeq <- forEach(argSeq) { arg =>
        if (tpeCase.isRecursive(model)) {
          apply(convertMethod, Seq(arg._3))
        } else {
          Command.lift[MethodBodyContext,Expression](arg._3)
        }
      }

      res <- instantiateObject(opInst, convertedArgSeq)
    } yield Some(res)
  }

  /**
   * Go to the LAST Exp in which an operation was defined. This actually produces a sequence, especially since the capability
   * exists to merge from past
   *
   * Works for Exp (names.conceptNameOf(model.baseDataType)) or Factory (names.mangle(Factory)), for whichever is passed in.
   * In which case they will either be classes or interfaces
   * @param model
   * @return
   */
  def priorFinalized(model:GenericModel) : Seq[Name] = {
    model.lastModelWithOperation.map(m => names.mangle(m.name))
  }

  /**
   * Make Finalized factory which each finalized datatype implements. Now a class, something like:
   *
   * public interface Factory extends ep.m0.Factory<Exp> {
   *   public default Exp lit(Double value) { return new Lit(value); }
   *   public default Exp add(ep.Exp<Exp> left, ep.Exp<Exp> right) { return new Add(this.convert(left), this.convert(right)); }
   *   public default Exp convert(ep.Exp<Exp> other) { return other.getSelf(); }
   * }
   *
   * Have to chain together with last one
   *
   * public interface Factory extends ep.m0.finalized.Factory,  ep.m1.Factory<Exp> {
   *   public default Exp sub(ep.Exp<Exp> left, ep.Exp<Exp> right) { return new Sub(convert(left), convert(right)); }
   * }
   *
   * Only need an Exp if new operations are defined, otherwise can just reuse LAST one, which also means that
   * in the ep.m1.Factory<Exp> above, we need to use the most recent finalized Exp
   *
   */
  def makeFinalizedCoCoFactory(domain:GenericModel): Generator[ClassContext, Unit] = {

    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._

    // only need to generate constructors for (a) new data types when no op defined; or (b) all past ones when op defined
    val constructorsToGenerate = if (domain.ops.nonEmpty) {
      domain.flatten.typeCases.distinct
    } else {
      domain.typeCases
    }
    for {
      // even though this expressly calls for ep.m#.Exp it becomes just Exp so missing the import.
      resultTpe <- findClass(names.mangle(domain.baseDataType.name))
      factory <- findClass(names.mangle(domain.name), Factory)

      // this always gets the current one, but one might not have been defined...
      //selfExp <- findClass(names.mangle(domain.name), finalized, names.mangle(names.conceptNameOf(domain.baseDataType)))
      parents <- forEach (domain.lastModelWithOperation) { dm => finalizedBase(dm) }

      factoryType <- applyType(factory,parents)
      _ <- addParent(factoryType)

      // chain factories together ONLY when don't have an operation declared
      _ <- if (domain.ops.isEmpty) {
        forEach(domain.former) { former => {
          for {
            //formerFactory <- findClass(names.mangle(former.name), finalized, Factory)
            formerFactory <- finalizedFactory(former)
            _ <- if (former.isDomainBase) {
              Command.skip[ClassContext]
            } else {
              addParent(formerFactory)
            }
          } yield()
        }
        }
      } else {
        Command.skip[ClassContext]
      }

      // WE only need new ones now UNLESS there has been an operation
      // with multiple branches, there could be copies, so make sure to be distinct
      //_ <- forEach(domain.flatten.typeCases.distinct) { tpeCase => {
      _ <- forEach(constructorsToGenerate) { tpeCase => {
        for {
          // These methods with recursive values must call convert; in addition, they must be properly
          // defined to use appropriate ep.m#.Exp based on where the data type was defined... TRICK
          _ <- addMethod (names.mangle(names.instanceNameOf(tpeCase)), futureCreateFactoryDataTypeCase(domain, tpeCase))
        } yield ()
      }
      }

      _ <- if (domain.ops.nonEmpty) {
        addMethodToClass(convert, makeConvertImplementation(domain, resultTpe))
      } else {
        Command.skip[ClassContext]
      }

      _ <- setInterface()
    } yield ()
  }

  /**
   * public abstract class Exp implements Factory, ep.m0.Exp<Exp>  {
   *    public Exp getSelf() { return this; }
   * }
   *
   * Currently generates " public ep.Exp<ep.Exp> getSelf() {" but should generate "public Exp getSelf() { return this; }"
   * That is, it should refer to SELF
   */
  def makeFinalizedCoCoBase(domain:GenericModel): Generator[ClassContext, Unit] = {

    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._

    def getSelfMethod(expType:Type): Generator[MethodBodyContext, Option[Expression]] = {
      import ooParadigm.methodBodyCapabilities._
      import paradigm.methodBodyCapabilities._

      for {
        _ <- setReturnType(expType)
        selfR <- selfReference()
        returnVal <- Command.lift[MethodBodyContext,Expression](selfR)
      } yield Some(returnVal)
    }

    for {
      // even though this expressly calls for ep.m#.Exp it becomes just Exp so missing the import.
      resultTpe <- findClass(names.mangle(domain.baseDataType.name)) /// , names.mangle(domain.baseDataType.name))
      _ <- resolveAndAddImport(resultTpe)

      factory <- findClass(names.mangle(domain.name), Factory)
      _ <- addImplemented(factory)

      expType <- findClass(names.mangle(domain.name), names.mangle(domain.baseDataType.name))
      selfExp <- finalizedBase(domain)
      expImpl <- applyType(expType,Seq[Type](selfExp))

      _ <- addImplemented(expImpl)

      _ <- addMethodToClass(getSelf, getSelfMethod(selfExp))
      _ <- setAbstract()
    } yield ()
  }

  def baseFactoryName(domain: GenericModel): Seq[Name] = {
    Seq(names.mangle(domain.name), Factory)
  }

  override def implement(domain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import paradigm.projectContextCapabilities._
    import ooParadigm.projectCapabilities._

    println(domain.name + ":" + new java.util.Date().toString)

    for {
      _ <- debug("Processing CoCo")
      _ <- registerTypeMapping(domain)

      _ <- makeCoCoBase(domain.baseDataType, Seq.empty)
      _ <- makeCoCoFactory(domain.baseDataType, Seq.empty)

      // whenever new operation, this CREATES the capability of having intermediate interfaces
      // chronological order could just become Topological Ordering
      _ <- forEach(domain.inChronologicalOrder) { currentModel =>
        // for all PAST dataTypes that are already defined
        for {
          _ <- domainSpecific.initialize(this)
          // only if operations are defined
          _ <- if (currentModel.ops.nonEmpty) {
            for {
              _ <- addClassToProject(makeFinalizedCoCoBase(currentModel), names.mangle(currentModel.name), finalized, names.mangle(names.conceptNameOf(currentModel.baseDataType)))
              _ <- addClassToProject(extendIntermediateInterface(currentModel, domainSpecific), baseInterfaceNames(currentModel): _*)
              _ <- registerTypeMapping(currentModel)   // what if we skipped this....
            } yield()
          } else {
            Command.skip[ProjectContext]
          }

          _ <- addClassToProject(extendFactory(currentModel, domainSpecific), baseFactoryName(currentModel): _*)
          _ <- addClassToProject(makeFinalizedCoCoFactory(currentModel), names.mangle(currentModel.name), finalized, Factory)

          // only generate PAST data types if new operation is defined
          _ <- if (currentModel.ops.isEmpty) {
            forEach(currentModel.typeCases) { tpe =>
              for {
                _ <- makeDerivedInterfaces(tpe, currentModel, domainSpecific)
                _ <- makeFinalClass(currentModel, tpe)
              } yield ()
            }
          } else {
            forEach(currentModel.inChronologicalOrder) { modelDefiningTypes =>
              forEach(modelDefiningTypes.typeCases) { tpe =>
                for {
                  _ <- makeDerivedInterfaces(tpe, currentModel, domainSpecific)
                  _ <- makeFinalClass(currentModel, tpe)
                } yield ()
              }
            }
          }
        } yield ()
      }
    } yield ()
  }

  def finalBaseInterfaceNames(domain: GenericModel): Seq[Name] = {
    Seq(names.mangle(domain.name), finalized, names.mangle(domain.baseDataType.name))
  }

  def specifiedInterface(domain: GenericModel): Seq[Name] = {
    Seq(names.mangle(domain.name), names.mangle(domain.baseDataType.name))
  }

  /**
   * Test cases all need factory methods to work.
   *
   * Note: shouldn't have to copy entire thing. Better to provide ability to extend inner part into which
   * the factory methods are injected.
   * */
  override def implement(tests: Map[GenericModel, Seq[TestCase]], testImplementationProvider: TestImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    import projectContextCapabilities._
    import paradigm.compilationUnitCapabilities._
    import paradigm.testCapabilities._

    for {

      _ <- forEach(tests.toList) { case (model, tests) => {
        val testCode: Generator[MethodBodyContext, Seq[Expression]] =
          for {
            code <- forEach(tests) {
              test => testImplementationProvider.test(this)(test)
            }
          } yield code.flatten

        import ooParadigm.testCapabilities._
        val compUnit = for {

          // add test case first
          _ <- addTestCase(testCode, testName)
          factory <-  findClass(names.mangle(model.name), finalized, Factory)
          _ <- resolveAndAddImport(factory)
          _ <- addImplemented(factory)

        } yield ()

        val testSuite = for {
          _ <- addTestSuite(
            testCaseName(model),
            compUnit
          )
        } yield ()

        for {
          _ <- registerTypeMapping(model) // must come here since it registers mappings that exist in the ProjectContext
          _ <- addCompilationUnit(
            testSuite,
            testCaseName(model)
          )
        } yield None
      }
      }
    } yield ()
  }
}

object CoCo {
  type WithParadigm[P <: AnyParadigm] = CoCo { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   imp: Imperative.WithBase[base.MethodBodyContext, base.type],
   oo: ObjectOriented.WithBase[base.type],
   params: ParametricPolymorphism.WithBase[base.type])
  (generics: Generics.WithBase[base.type,oo.type,params.type]): CoCo.WithParadigm[base.type] =
    new CoCo {
      val paradigm: base.type = base
      val names: NameProvider[paradigm.syntax.Name] = nameProvider
      val ooParadigm: oo.type = oo
      val impParadigm: imp.type = imp
      val polymorphics: params.type = params
      val genericsParadigm: generics.type = generics
    }
}
