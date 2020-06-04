package org.combinators.ep.approach.oo

import org.combinators.ep.domain.{GenericModel, Model}
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._
import org.combinators.ep.generator.paradigm._
import org.combinators.ep.generator.paradigm.control.Imperative

/**
 * ViTA Framework as submitted for publication.
 *
 *  HACK HACK -- had to hack java/ObjectOriented.scala for some reason. HELP!
 *  Minor Nitpick - can we avoid (a) generating public methods in public interfaces; (b) in an interface,
 *  methods are "public abstract" which is redunant and can be removed
 */

trait ViTA extends OOApproachImplementationProvider with BaseDataTypeAsInterface with SharedOO with FutureVisitor with OperationInterfaceChain with FieldDefinition with FactoryConcepts {
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]

  import ooParadigm._
  import paradigm._
  import syntax._

  // lazy val finalName:Name = names.mangle("Final")
  lazy val finalized:Name = names.mangle("finalized")     // sub package within each evolution that contains final classes
  lazy val expTypeParameter:Name = names.mangle("V")
 // lazy val Factory:Name = names.mangle("Factory")

  /** Placeholder for the ancestral type ep.Exp so it can be registered separately within the type mapping */
  lazy val ancestralTypePrefix:String = "ancestor"

  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      method <- getMember(message.to, names.mangle(names.instanceNameOf(message.request.op)))
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
   * Defines an operation and must also insert parameters when they exist.
   *
   * public default Boolean equals(ep.Exp<V> other) {*
   *   return this.astree().equals(convert(other).astree());
   * }
   *
   * Be careful to call convert on recursive types
   *
   * @param tpe
   * @param tpeCase
   * @param op
   * @param domainSpecific
   * @return
   */
  override def makeImplementation(tpe: DataType,
                                  tpeCase: DataTypeCase,
                                  op: Operation,
                                  domainSpecific: EvolutionImplementationProvider[this.type]
                                 ): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._
    for {
      thisRef <- selfReference()
      attAccessors: Seq[Expression] <- forEach (tpeCase.attributes) { att =>
        for {
          getter <- getMember(thisRef, getterName(att))
          getterCall <- apply(getter, Seq.empty)
        } yield getterCall
      }

      _ <- triviallyMakeSignature(tpe, op)
      //      // must add a proper parameter to this method, using the most generic ep.Exp for recursive
      //      // types, and pass others through with native types.
      //      params <- forEach (op.parameters) { param =>
      //        for {
      //          //paramField <- getMember(thisRef, names.mangle(param.name))
      //          paramTpe <- if (param.tpe == TypeRep.DataType(tpe)) {
      //            toTargetLanguageType(TypeRep.DataType(triviallyBaseDataType(tpe)))
      //          } else {
      //            toTargetLanguageType(param.tpe)
      //        }
      //        } yield (names.mangle(param.name), paramTpe)
      //      }
      //
      //      _ <- setParameters(params)
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
      //      opType <- toTargetLanguageType(op.returnType)
      //      _ <- resolveAndAddImport(opType)
      //      _ <- setReturnType(opType)

      // TODO: must call convert(baseType) or pass literals through
      result <-
        domainSpecific.logic(this)(
          ReceivedRequest(
            tpe,
            tpeCase,
            thisRef,
            tpeCase.attributes.zip(attAccessors).toMap,
            Request(op, processedArgs.toMap)
          )
        )
    } yield result
  }

  def derivedInterfaceName(tpe: DataTypeCase): Name = {
    names.mangle(tpe.name)
  }

  /** Former derived interfaced for the tpe must be qualified with model package */
  def getFormerDerivedInterfaces(domainDefiningType: GenericModel, current:DataTypeCase): Generator[ClassContext, List[Type]] = {
    import classCapabilities._

    for {
      // only take those who are not the bottom
      group <- forEach(domainDefiningType.former.filter(p => !p.isDomainBase)) { prior =>
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
   *
   *     public abstract Exp getLeft();
   *
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
   *
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
   * @param tpe
   * @param tpeCase
   * @param domainSpecific
   * @return
   */
  def makeDerivedInterface(tpe: DataType, tpeCase: DataTypeCase, model:GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      // Either generate an implementation for every operation if data type is declared in this model, or just the new ones.
      val opsToGenerate =
        if (model.findTypeCase(tpeCase).contains(model)) {
          model.flatten.ops
        } else model.ops

      def producerConvert(op:Operation) : Generator[MethodBodyContext, Option[Expression]] = {
        import ooParadigm.methodBodyCapabilities._
        import paradigm.methodBodyCapabilities._
        for {
          _ <- triviallyMakeSignature(model.baseDataType, op)
          thisRef <- selfReference()
          convertMethod <- getMember(thisRef, convert)

          //args <- forEach (op.parameters) { param => freshName(names.mangle(param.name)) }
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
          oldOnes = model.former.filter(prior => prior.findTypeCase(tpeCase).isDefined)
          laterModelDef = oldOnes.head // anyone will do...
//          typeCaseDef = model.findTypeCase(tpeCase).get
//          opDef = model.findOperation(op).get
//          laterModelDef = opDef.later(typeCaseDef)

          // should work for merging?
          superRef <- superReference(names.mangle(laterModelDef.name), names.mangle(names.conceptNameOf(tpeCase)))  // TODO: HAVE TO FIX THIS
          opMethod <- getMember(superRef, names.mangle(names.instanceNameOf(op)))
          innerResult <- apply(opMethod, argSeq)
          result <- apply(convertMethod, Seq(innerResult))
        } yield Some(result)
      }

      def extendParents(genType:Seq[Type]):Generator[ClassContext,Unit] = {
        import genericsParadigm.classCapabilities._

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
      for {
        // in new solution, all Exp<> references are parameterized
        fv <- freshName(expTypeParameter)  // HACK: MOVE UP
        _ <- addTypeParameter(fv, Command.skip[TypeParameterContext])
        genType<- getTypeArguments()

        parent <-  toTargetLanguageType(TypeRep.DataType(tpe))
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
          parent <-  toTargetLanguageType(TypeRep.DataType(tpe))
          _ <- resolveAndAddImport(parent)
          paramType <- applyType(parent, genType)
          _ <- addParent(paramType)
        } yield ()

        _ <- forEach (tpeCase.attributes) { att => {
          for {
            mi <- makeGetter(att)
            _ <- setAbstract()
          } yield mi
        }
        }
        _ <- forEach (opsToGenerate) { op =>
          addMethod(names.mangle(names.instanceNameOf(op)), makeImplementation(tpe, tpeCase, op, domainSpecific))
        }

        _ <- if (model.former.nonEmpty) {
          for {
            // domain.former.flatMap(_.lastModelWithOperation.flatMap(_.flatten.ops).filter(_.isProducer(domain))).distinct

            _ <- forEach(model.former.flatMap(_.flatten.ops.filter(op => op.isProducer(model))).distinct) { op =>
              addMethod(names.mangle(names.instanceNameOf(op)), producerConvert(op))
            }
          }yield ()
        } else {
          Command.skip[ClassContext]
        }

        _ <- setInterface()  // do LAST because then methods with bodies are turned into default methods in interface
      } yield ()
    }

    makeClass
  }

  def makeBodyImpl(baseType:DataType, att:Attribute, parent:Type): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      pt <- toTargetLanguageType(att.tpe)   // PULL OUT of the context
      _ <- setReturnType(pt)

      // provide an implementation here. but how?
      self <- selfReference()
      result <- getMember(self, names.mangle(names.instanceNameOf(att)))
    } yield Some(result)
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
        // define based on the derivedInterface for that data type and operation based on
        finalVisitor <- findClass(names.mangle(model.name), finalized, visitorClass)
        parent <- findClass(names.mangle(model.name), names.mangle(model.baseDataType.name))   // names.mangle(names.conceptNameOf(tpeCase)))

        parentParam <- applyType(parent, Seq(finalVisitor))
        _ <- resolveAndAddImport(parentParam)   // hope this brings in ep.m#.Exp
        _ <- registerLocally(model.baseDataType, parentParam)

        parentTypeCase <- findClass(names.mangle(model.name), names.mangle(names.conceptNameOf(tpeCase)))
        parentTypeVisitor <- applyType(parentTypeCase, Seq(finalVisitor))
        _ <- resolveAndAddImport(parentTypeVisitor)
        _ <- addImplemented(parentTypeVisitor)                    // implements derived interface

        factory <- findClass(names.mangle(model.name), finalized, factoryClass)   // bring in Factory methods easily
        _ <- addParent(factory)  //now parent
        _ <- forEach(tpeCase.attributes) { att => makeField(att) }
        _ <- addConstructor(makeConstructor(tpeCase))
        _ <- forEach(tpeCase.attributes) { att =>
          addMethod(getterName(att), makeBodyImpl(model.baseDataType, att, parent))
        }

        visitorType <- findClass(names.mangle(model.name), finalized, visitorClass)
        baseType <-  findClass(names.mangle(model.baseDataType.name))
        _ <- resolveAndAddImport(visitorType)
        _ <- addAcceptMethod(makeAcceptImplementation(visitorType))
        _ <- addConvertMethod(makeConvertImplementation(model, visitorType, baseType))
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

    val baseInterface = baseInterfaceNames(model.standAlone)   // registers as m#.Exp   -- might not be necessary to convert standAlone
    val dtpeRep = TypeRep.DataType(model.baseDataType)

    for {
      _ <- addTypeLookupForMethods(dtpeRep, domainTypeLookup(baseInterface : _*))
      _ <- addTypeLookupForClasses(dtpeRep, domainTypeLookup(baseInterface : _*))
      _ <- addTypeLookupForConstructors(dtpeRep, domainTypeLookup(baseInterface : _*))

    } yield ()
  }

  /**
   * Instead of using the standard 'makeBase' we need to add accept and convert methods.
   *
   * Returns the fully qualified and not yet applied type to access this class, e.g. ep.Exp.
   */
  def makeViTABase(tpe: DataType, ops: Seq[Operation]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    def makeAbstractAcceptSignature(visitorType: Type): Generator[MethodBodyContext, Option[Expression]] = {
      import ooParadigm.methodBodyCapabilities._
      import paradigm.methodBodyCapabilities._

      for {
        _ <- makeAcceptSignature(visitorType)
        _ <- setAbstract()
      } yield None
    }

    def addVisitorTypeParameter(): Generator[ClassContext, Type] = {
      import classCapabilities._
      import genericsParadigm.classCapabilities._
      import polymorphics._
      for {
        visitorTypeParamName <- freshName(expTypeParameter)
        _ <- addTypeParameter(visitorTypeParamName, Command.skip[TypeParameterContext])
        visitorType <- getTypeArguments().map(_.head)
      } yield visitorType
    }

    // at this point, we only want ep.Exp not the most qualified ep.m4.Exp
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      import genericsParadigm.classCapabilities._
      import polymorphics.TypeParameterContext
      for {
        visitorType <- addVisitorTypeParameter()
        _ <- setInterface()
      //  selfClass <- findClass(names.mangle(names.conceptNameOf(tpe)))

        _ <- addAcceptMethod(makeAbstractAcceptSignature(visitorType))
        factory <- findClass(Factory)
        factoryParam <- applyType(factory, Seq(visitorType))
        _ <- addParent(factoryParam)
      } yield ()
    }
    addClassToProject(makeClass, names.mangle(names.conceptNameOf(tpe)))
  }

  /**
   * Factory now a stand-alone interface
   * Returns the fully qualified and not yet applied type to access this class, e.g. ep.Exp.
   */
  def makeViTAFactory(tpe: DataType, ops: Seq[Operation]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    def makeAbstractConvertSignature(selfClassWithVisitorType: Type): Generator[MethodBodyContext, Option[Expression]] = {
      import ooParadigm.methodBodyCapabilities._
      import paradigm.methodBodyCapabilities._
      for {
        _ <- makeConvertSignature(selfClassWithVisitorType, selfClassWithVisitorType)
        _ <- setAbstract()
      } yield None
    }

    def addVisitorTypeParameter(): Generator[ClassContext, Type] = {
      import classCapabilities._
      import genericsParadigm.classCapabilities._
      import polymorphics._
      for {
        visitorTypeParamName <- freshName(expTypeParameter)
        _ <- addTypeParameter(visitorTypeParamName, Command.skip[TypeParameterContext])
        visitorType <- getTypeArguments().map(_.head)
      } yield visitorType
    }

    // at this point, we only want ep.Exp not the most qualified ep.m4.Exp
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      import genericsParadigm.classCapabilities._
      import polymorphics.TypeParameterContext
      for {
        visitorType <- addVisitorTypeParameter()
        _ <- setInterface()
        selfClass <- findClass(names.mangle(names.conceptNameOf(tpe)))
        selfClassWithVisitorType <- applyType(selfClass, Seq(visitorType))
        _ <- addConvertMethod(makeAbstractConvertSignature(selfClassWithVisitorType))
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
  def triviallyMakeSignature(baseType:DataType, op: Operation): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._

    for {
      rt <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(rt)
      _ <- setReturnType(rt)

      params <- forEach (op.parameters) { param: Parameter =>
        for {
          // pt <- toTargetLanguageType(param.tpe)
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

//  // probilem: in past we were able to use registry to locate the appropriate "prevOne"
//  // extends Exp [first one] or ExpEval [previous one]
//  // Works for both Exp* interface declarations as well as DataTypeOp declarations
//  def getParentInterfaces(domain: GenericModel, tpe: DataType): Generator[ClassContext, Seq[Type]] = {
//    import classCapabilities._
//
//    if (domain.isEmpty || domain.lastModelWithOperation.isEmpty) {
//      for {
//        fc <- findClass(names.mangle(domain.baseDataType.name))
//      } yield (Seq(fc))
//    } else {
//      for {
//        group <- forEach (baseInterfaceNames(domain)) { iface => {
//
//          }
//        }
//      } yield (group)
////      findClass(baseInterfaceNames(domain) : _*)
//    }
//  }


  /** I had to copy this entire thing.
   *
   * is there any way to have one generator create a class and then another function can just ADD to it?
   *  */
  def makeViTAInterface(domain:GenericModel, domainSpecific: EvolutionImplementationProvider[this.type], typeParameter:Option[Name] = None): Generator[ClassContext, Unit] = {
    // create class which is an interface containing abstract methods
    import classCapabilities._
    import genericsParadigm.classCapabilities._

    def extendParents:Generator[ClassContext, Unit] = {
      val formers:Seq[Seq[Name]] = domain.former.map(dom => baseInterfaceNames(dom))
      for {
        // get formers,filter all of those that "know" about the DataType, and then extend those
        //parent <- getParentInterfaces(domain.former, domain.baseDataType)
        _ <- forEach(formers) { former => {

          for {
            parent <- findClass(former: _*)
            _ <- resolveAndAddImport(parent)
            justV <- getTypeArguments().map(_.head)
            paramType <- applyType(parent, Seq(justV))

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
        for {
          parent <- findClass(names.mangle(domain.baseDataType.name))
          justV <- getTypeArguments().map(_.head)
          paramType <- applyType(parent, Seq(justV))

          //  public void accept(V visitor);
          //    public Exp<V> convert(Exp<V> value);
          //////////_ <- addConvertMethod(makeConvertSignature(paramType, paramType))
          _ <- addAcceptMethod(makeAcceptSignature(paramType))
        } yield ()

        Command.skip[ClassContext]
      }

      // must be moved OUTSIDE because only the extended interfaces will have methods defined....
      //      _ <- forEach (domain.ops) { op => addAbstractMethod(names.mangle(names.instanceNameOf(op)), triviallyMakeSignature(domain.baseDataType, op)) }
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

          //  public void accept(V visitor);
          //    public Exp<V> convert(Exp<V> value);
          _ <- addConvertMethod(makeConvertSignature(paramType, paramType))
          _ <- addAcceptMethod(makeAcceptSignature(paramType))
        } yield ()

        Command.skip[ClassContext]
      }

      // must be moved OUTSIDE because only the extended interfaces will have methods defined....
      //      _ <- forEach (domain.ops) { op => addAbstractMethod(names.mangle(names.instanceNameOf(op)), triviallyMakeSignature(domain.baseDataType, op)) }
    } yield ()
  }

  /**
   * Need to be sure return types and parameters become specialized to current/most recent model Exp
   *
   *  public abstract ep.m0.Exp<V> add(ep.Exp<V> left, ep.Exp<V> right);
   *
   * where
   * @param model
   * @param tpeCase
   * @param opClass
   * @param isStatic
   * @param typeParameters
   * @return
   */
  def createLocalizedFactorySignatureDataTypeCase(model:GenericModel, tpeCase:DataTypeCase, opClass:Type, isStatic:Boolean = false, typeParameters:Seq[Type] = Seq.empty): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._
    for {
      //      opClass <- toTargetLanguageType(TypeRep.DataType(model.baseDataType))   // should check!
      //      _ <- resolveAndAddImport(opClass)
      _ <- setReturnType(opClass)
      _ <- if (isStatic) { setStatic() } else { Command.skip[MethodBodyContext] }
      params <- forEach (tpeCase.attributes) { att: Attribute =>
        if (tpeCase.isRecursive(model)) {
          for {
            at <- toTargetLanguageType(att.tpe)

            pat <- applyType(at, typeParameters)
            pName <- freshName(names.mangle(names.instanceNameOf(att)))
          } yield (pName, pat)
        } else {  // HACK: non parameterized for non-recursive types
          for {
            at <- toTargetLanguageType(att.tpe)

            pName <- freshName(names.mangle(names.instanceNameOf(att)))
          } yield (pName, at)
        }
      }

      _ <- setParameters(params)
    } yield None
  }

  /**
   * Starting with the operation chain and add the requisite interfaces for known factories.
   *
   * package ep.m0;
   *
   * public interface Exp<V> extends ep.Exp<V> {
   *   public abstract Double eval();   // operation
   *   public abstract ep.m0.Exp<V> lit(Double value);
   *   public abstract ep.m0.Exp<V> add(ep.Exp<V> left, ep.Exp<V> right);
   *
   *   public abstract Boolean equals(ep.Exp<V> other);    // binary operation MUST refer to ancestral type
   *
   *   public abstract ep.m5.Exp<V> simplify();   // any producer methods IN PAST MUST ALWAYS OVERLOAD
   *   // conversion
   *   public abstract ep.m0.Exp<V> convert(ep.Exp<V> toConvert);
   *
   * }
   */
  def extendIntermediateInterface(domain:GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._
    import polymorphics.TypeParameterContext

//    def convertMethod(topLevelType:Type, paramType:Type) : Generator[MethodBodyContext, Option[Expression]] = {
//      import ooParadigm.methodBodyCapabilities._
//      for {
//        _ <- makeConvertSignature(topLevelType, paramType)
//        _ <- setAbstract()
//      } yield None
//    }

    for {
      _ <- makeViTAInterface(domain, domainSpecific, Some(expTypeParameter))   // this creates TYPE

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
      _ <- forEach (domain.ops) { op => addAbstractMethod(names.mangle(names.instanceNameOf(op)), triviallyMakeSignature(domain.baseDataType, op)) }
      _ <- if (domain.former.nonEmpty) {
        forEach(domain.former.flatMap(_.lastModelWithOperation.flatMap(_.flatten.ops).filter(_.isProducer(domain))).distinct) {
          op => addAbstractMethod(names.mangle(names.instanceNameOf(op)), triviallyMakeSignature(domain.baseDataType, op))
        }
      } else {
        Command.skip[ClassContext]
      }

    } yield ()
  }

  /** Same as below but works without domain, providing you pass in the base type. */
  def computedFactoryType[Context]()(implicit canFindClass: Understands[Context, FindClass[Name, Type]]): Generator[Context, Type] = {
    FindClass(Seq(Factory)).interpret(canFindClass)
  }

  /**
   * Starting with the operation chain and add the requisite interfaces for known factories.
   *
   * package ep.m0;
   *
   * public interface Exp<V> extends ep.Exp<V> {
   *   public abstract Double eval();   // operation
   *   public abstract ep.m0.Exp<V> lit(Double value);
   *   public abstract ep.m0.Exp<V> add(ep.Exp<V> left, ep.Exp<V> right);
   *
   *   public abstract Boolean equals(ep.Exp<V> other);    // binary operation MUST refer to ancestral type
   *
   *   public abstract ep.m5.Exp<V> simplify();   // any producer methods IN PAST MUST ALWAYS OVERLOAD
   *   // conversion
   *   public abstract ep.m0.Exp<V> convert(ep.Exp<V> toConvert);
   *
   * }
   */
  def extendFactory(domain:GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._
    import polymorphics.TypeParameterContext

    def convertMethod(topLevelType:Type, paramType:Type) : Generator[MethodBodyContext, Option[Expression]] = {
      import ooParadigm.methodBodyCapabilities._
      for {
        _ <- makeConvertSignature(topLevelType, paramType)
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
      //parent <- findClass(names.mangle(domain.baseDataType.name))
      parent <- findClass(names.mangle(domain.name), names.mangle(domain.baseDataType.name))
      justV <- getTypeArguments().map(_.head)
      paramType <- applyType(parent, Seq(justV))

      tt <- computedBaseType(domain)
      topLevelType <- applyType(tt, Seq(justV))

      // add factory methods
      // these are factory signatures.
      _ <- forEach (domain.flatten.typeCases) { tpe => {
        // make this a helper capability in SharedOO -- ability to take an existing methodBodyContext and make it abstract...
        val absMethod: Generator[MethodBodyContext, Option[Expression]] = {
          import ooParadigm.methodBodyCapabilities._
          for {
            xf <- createFactorySignatureDataTypeCase(domain, tpe, topLevelType, paramType, false)
            _ <- setAbstract()

          } yield xf
        }

        addMethod(names.mangle(names.instanceNameOf(tpe)), absMethod)
      }
      }

      //      parent <- findClass(names.mangle(domain.baseDataType.name))
      //      justV <- getTypeArguments().map(_.head)
      //      paramType <- applyType(parent, Seq(justV))

      //  public void accept(V visitor);
      //    public Exp<V> convert(Exp<V> value);
      _ <- addConvertMethod(convertMethod(topLevelType, paramType))
      //_ <- addConvertMethod(makeConvertSignature(topLevelType, paramType))
    } yield ()
  }


  /** I had to encapsulate since varargs approach didn't work... */
  def finalizedVisitorName(domain: GenericModel): Seq[Name] = {
    Seq(names.mangle(domain.name), finalized, visitorClass)
  }

  def baseFactoryName(domain: GenericModel): Seq[Name] = {
    Seq(names.mangle(domain.name), Factory)
  }

//   def implement(domain: Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
//     implementGeneric(domain, domainSpecific)
//   }

    override def implement(domain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import paradigm.projectContextCapabilities._
    import ooParadigm.projectCapabilities._

    for {
      _ <- debug("Processing ViTA")
      _ <- registerTypeMapping(domain)

      _ <- makeViTABase(domain.baseDataType, Seq.empty)
      _ <- makeViTAFactory(domain.baseDataType, Seq.empty)

      // whenever new operation, this CREATES the capability of having intermediate interfaces
      // chronlogical order could just become Topological Ordering
      _ <- forEach(domain.inChronologicalOrder) { currentModel =>
        // for all PAST dataTypes that are already defined
        for {
          _ <- domainSpecific.initialize(this)
          _ <- registerTypeMapping(currentModel)

          _ <- addClassToProject(makeFinalizedVisitor(currentModel), finalizedVisitorName(currentModel): _*)
          _ <- addClassToProject(extendIntermediateInterface(currentModel, domainSpecific), baseInterfaceNames(currentModel): _*)
          _ <- addClassToProject(extendFactory(currentModel, domainSpecific), baseFactoryName(currentModel): _*)
          _ <- addFactoryToProject(currentModel, makeFinalizedViTAFactory(currentModel))

          _ <- forEach(currentModel.inChronologicalOrder) { modelDefiningTypes =>
            forEach(modelDefiningTypes.typeCases) { tpe =>
              for {
                _ <- makeDerivedInterfaces(tpe, currentModel, domainSpecific)
                _ <- makeFinalClass(currentModel, tpe)
              } yield ()
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

//  override def implement(tests: Map[Model, Seq[TestCase]], testImplementationProvider: TestImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
//    implementTestGeneric(tests.map({ case (k,v) => (k,v)}), testImplementationProvider)
//  }

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

    def factoryMethod(model:GenericModel, tpeCase:DataTypeCase) : Generator[MethodBodyContext, Option[Expression]] = {

      import ooParadigm.methodBodyCapabilities._
      import polymorphics.methodBodyCapabilities._
      import paradigm.methodBodyCapabilities._
      for {
        // ep.m4.Exp<ep.m4.finalized.Visitor>
        paramBaseType <- toTargetLanguageType(TypeRep.DataType(model.baseDataType))
        _ <- resolveAndAddImport(paramBaseType)
        visitorType <- findClass(names.mangle(model.name), finalized, visitorClass)
        _ <- resolveAndAddImport(visitorType)

        returnType <- findClass(names.mangle(model.name), finalized, names.mangle(names.conceptNameOf(tpeCase)))
        _ <- resolveAndAddImport(returnType)

        appliedType <- applyType(paramBaseType, Seq(visitorType))
        //

        facMethod <- createFactoryDataTypeCase(model, tpeCase, appliedType, returnType)
      } yield facMethod
    }

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

          // no longer necessary with finalized classes??
          // get list of all operations and MAP to the most recent model
          _ <- forEach(model.flatten.typeCases) { tpeCase => {
            for {
              _ <- addMethod(names.mangle(names.instanceNameOf(tpeCase)), factoryMethod(model, tpeCase))
            } yield (None)
          }
          }

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

object ViTA {
  type WithParadigm[P <: AnyParadigm] = ViTA { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   imp: Imperative.WithBase[base.MethodBodyContext, base.type],
   oo: ObjectOriented.WithBase[base.type],
   params: ParametricPolymorphism.WithBase[base.type])
  (generics: Generics.WithBase[base.type,oo.type,params.type]): ViTA.WithParadigm[base.type] =
    new ViTA {
      val paradigm: base.type = base
      val names: NameProvider[paradigm.syntax.Name] = nameProvider
      val ooParadigm: oo.type = oo
      val impParadigm: imp.type = imp
      val polymorphics: params.type = params
      val genericsParadigm: generics.type = generics
    }
}
