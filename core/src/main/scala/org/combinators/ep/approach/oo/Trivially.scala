package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._
import org.combinators.ep.generator.paradigm._
import org.combinators.ep.generator.paradigm.control.Imperative

/**
 *

 Producer methods must include factory methods for all known types so far.

 */

trait Trivially extends OOApproachImplementationProvider with BaseDataTypeAsInterface with SharedOO with FutureVisitor with OperationInterfaceChain with FieldDefinition with FactoryConcepts {
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]

  import ooParadigm._
  import paradigm._
  import syntax._

 // lazy val finalName:Name = names.mangle("Final")
  lazy val finalized:Name = names.mangle("finalized")     // sub package within each evolution that contains final classes
  lazy val expTypeParameter:Name = names.mangle("V")

  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      method <- getMember(message.to, names.mangle(names.instanceNameOf(message.request.op)))
      result <- apply(method, message.request.op.parameters.map(message.request.arguments))
    } yield result

  }

  /**
   * new Sub((FinalI) (new Lit(1.0)), (FinalI) (new Lit(2.0))).prettyp()); was how we did in old code
   *
   * this becomes
   *
   * Sub(Lit(1.0), Lit(2.0)).prettyp());
   *
   * Relies on factory methods to instantiate based on latest classes generated
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
      factory <- getMember(thisRef, names.mangle(names.conceptNameOf(tpeCase)))
      res <- apply(factory, args)
    } yield res
  }

  override def factoryInstanceDataTypeCase(model:Option[Model] = None, tpeCase:DataTypeCase) : Seq[Name] = {
    model.map(m => names.mangle(m.name)).toSeq :+ finalized :+ names.mangle(names.conceptNameOf(tpeCase))
  }

  override def makeImplementation(tpe: DataType,
                                   tpeCase: DataTypeCase,
                                   op: Operation,
                                   domainSpecific: EvolutionImplementationProvider[this.type]
                                 ): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      thisRef <- selfReference()
      attAccessors: Seq[Expression] <- forEach (tpeCase.attributes) { att =>
        for {
          getter <- getMember(thisRef, getterName(att))
          getterCall <- apply(getter, Seq.empty)
        } yield getterCall
      }

      args <- forEach (op.parameters) { param =>
        for {
          paramField <- getMember(thisRef, names.mangle(param.name))
        } yield (param, paramField)
      }

      opType <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(opType)
      _ <- setReturnType(opType)

      result <-
        domainSpecific.logic(this)(
          ReceivedRequest(
            tpe,
            tpeCase,
            thisRef,
            tpeCase.attributes.zip(attAccessors).toMap,
            Request(op, args.toMap)
          )
        )
    } yield result
  }

  def derivedInterfaceName(tpe: DataTypeCase): Name = {
    names.mangle(tpe.name)
  }

  /** Former derived interfaced for the tpe must be qualified with model package */
  def getFormerDerivedInterface(domainDefiningType: Model, current:DataTypeCase): Generator[ClassContext, Type] = {
    import classCapabilities._

    findClass(names.mangle(domainDefiningType.last.get.name), derivedInterfaceName(current))
  }

  /**
   * For any datatype that *could* have been defined in domain or perhaps was defined earlier
   */
  def makeDerivedInterfaces(tpeCase:DataTypeCase, currentModel:Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
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
   *   package m1
   *   public interface Sub<V> extends ep.m1.Exp<V> {
   *
   *     ep.m1.Exp getLeft();
   *
   *     ep.m1.Exp getRight();
   *
   *     default Double eval() {
   *         return getLeft().eval() - getRight().eval();
   *     }
   * }
   * }}}
   *
   *  add another parent IF there is a prior operation defined before this model.
   *
   *  These are all defined in the "current" model. Note that if the operation is a producer method, then
   *  you need factory methods for all known data types.
   *
   * @param tpe
   * @param tpeCase
   * @param domainSpecific
   * @return
   */
  def makeDerivedInterface(tpe: DataType, tpeCase: DataTypeCase, model:Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      // Either generate an implementation for every operation if data type is declared in this model, or just the new ones.
      val opsToGenerate =
        if (model.findTypeCase(tpeCase).contains(model)) {
          model.flatten.ops
        } else model.ops

      import genericsParadigm.classCapabilities._
      import polymorphics.TypeParameterContext
      for {
        // in new solution, all Exp<> references are parameterized
        fv <- freshName(expTypeParameter)  // HACK: MOVE UP
        _ <- addTypeParameter(fv, Command.skip[TypeParameterContext])
        genType <- getTypeArguments()

        parent <-  toTargetLanguageType(TypeRep.DataType(tpe))
        _ <- resolveAndAddImport(parent)
        paramType <- applyType(parent, genType)
        _ <- registerLocally(tpe, paramType)

        // only add parent to the chain if NOT the first one
        _ <- if (model.last.isDefined && model.last.get.findTypeCase(tpeCase).isDefined) {
          for {
            parentFormer <- getFormerDerivedInterface(model, tpeCase)
            _ <- resolveAndAddImport(parentFormer)
            paramType <- applyType(parentFormer, genType)
            _ <- addParent(paramType)
          } yield ()
        } else {
            for {
              // first one needs to extend Exp
              parent <-  toTargetLanguageType(TypeRep.DataType(tpe))
              _ <- resolveAndAddImport(parent)
              paramType <- applyType(parent, genType)
              _ <- addParent(paramType)
            } yield ()
        }

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
        // these are factory signatures. Moved to the Exp class
//        _ <- forEach (model.flatten.typeCases) { tpe =>
//          addAbstractMethod(names.mangle(names.instanceNameOf(tpe)), convertOptionToUnit(createFactorySignatureDataTypeCase(model, tpe, paramType)))
//        }

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
     * HACK: Just duplicate and embed; figure out later.
     * @param model
     * @param tpeCase
     * @return
     */
    def makeFinalClass(model:Model, tpeCase: DataTypeCase): Generator[ProjectContext,Unit] = {
      import ooParadigm.projectCapabilities._

      val makeClass: Generator[ClassContext, Unit] = {
        import ooParadigm.classCapabilities._
        import genericsParadigm.classCapabilities._

        for {
          // define based on the derivedInterface for that data type and operation based on
          finalVisitor <- findClass(names.mangle(model.name), finalized, visitorClass)
          parent <- findClass(names.mangle(model.name), names.mangle(model.baseDataType.name))   // names.mangle(names.conceptNameOf(tpeCase)))
          _ <- resolveAndAddImport(parent)   // hope this brings in ep.m#.Exp
          parentParam <- applyType(parent, Seq(finalVisitor))
          _ <- registerLocally(model.baseDataType, parentParam)

          parentTypeCase <- findClass(names.mangle(model.name), names.mangle(names.conceptNameOf(tpeCase)))
          parentTypeVisitor <- applyType(parentTypeCase, Seq(finalVisitor))
          _ <- resolveAndAddImport(parentTypeVisitor)
          _ <- addImplemented(parentTypeVisitor)                    // implements derived interface

          factory <- findClass(names.mangle(model.name), finalized, factoryClass)   // bring in Factory methods easily
          _ <- addImplemented(factory)
          _ <- forEach(tpeCase.attributes) { att => makeField(att) }
          _ <- addConstructor(makeConstructor(tpeCase))
          _ <- forEach(tpeCase.attributes) { att =>
            addMethod(getterName(att), makeBodyImpl(model.baseDataType, att, parent))
          }

          visitorType <- findClass(names.mangle(model.name), finalized, visitorClass)
          baseType <-  findClass(names.mangle(model.baseDataType.name))
          _ <- resolveAndAddImport(visitorType)
          _ <- justAddAcceptMethod(makeAcceptImplementation(visitorType))
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

    /** What model is delivered has operations which is essential for the mapping. */
    override def registerTypeMapping(model: Model): Generator[ProjectContext, Unit] = {
      import ooParadigm.projectCapabilities._
      import ooParadigm.classCapabilities.canFindClassInClass
      import ooParadigm.constructorCapabilities.canFindClassInConstructor
      import ooParadigm.methodBodyCapabilities.canFindClassInMethod
      import paradigm.projectContextCapabilities._

      val baseInterface = baseInterfaceNames(model)
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
   * This insists upon accessing registry for Exp, which unfortunately grabs ep.m4.Exp when
   * I only want this top-level one. Cna't
   */
  def makeTriviallyBase(tpe: DataType, ops: Seq[Operation]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    def abstractMethod(baseExp:Type): Generator[MethodBodyContext, Option[Expression]] = {
      import ooParadigm.methodBodyCapabilities._
      import paradigm.methodBodyCapabilities._

      for {
        _ <- makeConvertSignature(baseExp, baseExp)
        _ <- setAbstract()
      } yield None
    }

    // at this point, we only want ep.Exp not the most qualified ep.m4.Exp
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._

      import genericsParadigm.classCapabilities._
      import polymorphics.TypeParameterContext
      for {
        // in new solution, all Exp<> references are parameterized
        fv <- freshName(expTypeParameter)
        _ <- addTypeParameter(fv, Command.skip[TypeParameterContext])
        justV <- getTypeArguments().map(_.head)

        _ <- setInterface()
        baseExp <- findClass(names.mangle(names.conceptNameOf(tpe)))     //toTargetLanguageType(TypeRep.DataType(tpe))
        _ <- addAcceptMethod(tpe, justV)

        paramType <- applyType(baseExp, Seq(justV))
        _ <- addConvertMethod(abstractMethod(paramType))  // was baseExp
      } yield ()
    }

    addClassToProject(makeClass, names.mangle(names.conceptNameOf(tpe)))
  }

  /** I had to copy this entire thing.
   *
   * is there any way to have one generator create a class and then another function can just ADD to it?
   *  */
  override def makeInterface(domain:Model, domainSpecific: EvolutionImplementationProvider[this.type], typeParameter:Option[Name] = None): Generator[ClassContext, Unit] = {
    // create class which is an interface containing abstract methods
    import classCapabilities._
    import genericsParadigm.classCapabilities._

    for {
      _ <- setInterface()

      _ <- if (typeParameter.isDefined) {
        for {
          _ <- addTypeParameter(typeParameter.get, Command.skip)
        } yield ()
      } else {
        Command.skip[ClassContext]
      }

      _ <- if (domain.last.isDefined) {
        for {
          parent <- getParentInterface(domain.last.get, domain.baseDataType)

          // AWKWARD! Have to grab the type parameter from the current class since I can't seem
          // to just convert a string like "V" into a Type... That would be useful!
          _ <- if (typeParameter.isDefined) {
            for {
              justV <- getTypeArguments().map(_.head)
              paramType <- applyType(parent, Seq(justV))
              _ <- resolveAndAddImport(paramType)

              _ <- addParent(paramType)

              // don't forget to add accept and convert methods
            } yield ()
          } else {
            for {
              _ <- resolveAndAddImport(parent)
              justV <- getTypeArguments().map(_.head)
              paramType <- applyType(parent, Seq(justV))
              _ <- resolveAndAddImport(paramType)
              _ <- addParent(paramType)
            } yield ()
          }

        } yield ()
      } else {
        for {
          parent <- findClass(names.mangle(domain.baseDataType.name))
          justV <- getTypeArguments().map(_.head)
          paramType <- applyType(parent, Seq(justV))

          //  public void accept(V visitor);
          //    public Exp<V> convert(Exp<V> value);
          _ <- addConvertMethod(makeConvertSignature(paramType, paramType))
          _ <- addAcceptMethod(domain.baseDataType, justV)
        } yield ()

        Command.skip[ClassContext]
      }

      _ <- forEach (domain.ops) { op => addAbstractMethod(names.mangle(names.instanceNameOf(op)), makeSignature(op)) }
    } yield ()
  }

  /**
   * Need to be sure return types and parameters become specialized to current/most recent model Exp
   *
   *  public abstract ep.m0.Exp<V> add(ep.m0.Exp<V> left, ep.m0.Exp<V> right);
   *
   * where
   * @param model
   * @param tpeCase
   * @param opClass
   * @param isStatic
   * @param typeParameters
   * @return
   */
  def createLocalizedFactorySignatureDataTypeCase(model:Model, tpeCase:DataTypeCase, opClass:Type, isStatic:Boolean = false, typeParameters:Seq[Type] = Seq.empty): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._
    for {
      //      opClass <- toTargetLanguageType(TypeRep.DataType(model.baseDataType))   // should check!
      //      _ <- resolveAndAddImport(opClass)
      _ <- setReturnType(opClass)
      _ <- if (isStatic) { setStatic() } else { Command.skip[MethodBodyContext] }
      params <- forEach (tpeCase.attributes) { att: Attribute => {
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
      }

      _ <- setParameters(params)

    } yield None
  }

  /**
   * Starting with the operation chain and add the requisite interfaces for known factories
   */
  def extendIntermediateInterface(domain:Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._
    import polymorphics.TypeParameterContext

    for {
      _ <- makeInterface(domain, domainSpecific, Some(expTypeParameter))   // this creates TYPE

//      fv <- freshName(expTypeParameter)  // HACK: MOVE UP
//      _ <- addTypeParameter(fv, Command.skip[TypeParameterContext])

      // paramType is now Exp<V>. Couldn't get type arguments?
      //parent <- findClass(names.mangle(domain.baseDataType.name))
      parent <- findClass(names.mangle(domain.name), names.mangle(domain.baseDataType.name))
      justV <- getTypeArguments().map(_.head)
      paramType <- applyType(parent, Seq(justV))

      // add factory methods
      // these are factory signatures.
      _ <- forEach (domain.flatten.typeCases) { tpe => {
        // make this a helper capability in SharedOO -- ability to take an existing methodBodyContext and make it abstract...
        val absMethod: Generator[MethodBodyContext, Option[Expression]] = {
          import ooParadigm.methodBodyCapabilities._
          for {
            xf <- createLocalizedFactorySignatureDataTypeCase(domain, tpe, paramType, false, Seq(justV))
            _ <- setAbstract()

          } yield xf
        }

        addMethod(names.mangle(names.instanceNameOf(tpe)), absMethod)
      }
      }
    } yield ()
  }

  /** I had to encapsulate since varargs approach didn't work... */
  def finalizedVisitorName(domain: Model): Seq[Name] = {
    Seq(names.mangle(domain.name), finalized, visitorClass)
  }

    def implement(domain: Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
      import paradigm.projectContextCapabilities._
      import ooParadigm.projectCapabilities._
      for {
        _ <- debug("Processing Trivially")
        _ <- registerTypeMapping(domain)
        _ <- makeTriviallyBase(domain.baseDataType, Seq.empty)

        // whenever new operation, this CREATES the capability of having intermediate interfaces
        _ <- forEach(domain.inChronologicalOrder) { currentModel =>
          // for all PAST dataTypes that are already defined
          for {
            _ <- domainSpecific.initialize(this)
            _ <- registerTypeMapping(currentModel)

            _ <- addClassToProject(makeFinalizedVisitor(currentModel),  finalizedVisitorName(currentModel) : _*)
            _ <- addClassToProject(extendIntermediateInterface(currentModel, domainSpecific), baseInterfaceNames(currentModel) : _*)

            _ <- addFactoryToProject(currentModel, makeFinalizedFactory(currentModel))

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

  def finalBaseInterfaceNames(domain: Model): Seq[Name] = {
    Seq(names.mangle(domain.name), finalized, names.mangle(domain.baseDataType.name))
  }

  def specifiedInterface(domain: Model): Seq[Name] = {
    Seq(names.mangle(domain.name), names.mangle(domain.baseDataType.name))
  }

  /**
   * Test cases all need factory methods to work.
   *
   * Note: shouldn't have to copy entire thing. Better to provide ability to extend inner part into which
   * the factory methods are injected.
   * */
   override def implement(tests: Map[Model, Seq[TestCase]], testImplementationProvider: TestImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
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

           // no longer necessary with finalized classes??
//           // get list of all operations and MAP to the most recent model
//           _ <- forEach(model.flatten.typeCases) { tpeCase =>
//             // must ensure
//             addMethod(names.mangle(names.conceptNameOf(tpeCase)), createFactoryDataTypeCase(model, tpeCase))
//           }

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

  object Trivially {
    type WithParadigm[P <: AnyParadigm] = Trivially { val paradigm: P }
    type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

    def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
    (base: P)
    (nameProvider: NameProvider[base.syntax.Name],
     imp: Imperative.WithBase[base.MethodBodyContext, base.type],
     oo: ObjectOriented.WithBase[base.type],
     params: ParametricPolymorphism.WithBase[base.type])
    (generics: Generics.WithBase[base.type,oo.type,params.type]): Trivially.WithParadigm[base.type] =
      new Trivially {
        val paradigm: base.type = base
        val names: NameProvider[paradigm.syntax.Name] = nameProvider
        val ooParadigm: oo.type = oo
        val impParadigm: imp.type = imp
        val polymorphics: params.type = params
        val genericsParadigm: generics.type = generics
      }
  }

/**\

 Monday phone call topics

 1. How/why am I missing import for ep.m0.Exp in core classes that are generated?

 2. When making the FinalFactory, you want to map Exp to the location where each data type
    was defined. For example, for M2 we should have:

       ep.m0.Exp<Visitor> lit(Double value)
       ep.m1.Exp<Visitor> sub(ep.m2.Exp<Visitor> left, ep.m2.Exp<Visitor> right)

 */