package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._
import org.combinators.ep.generator.paradigm._

/**
 *

 Producer methods must include factory methods for all known types so far.

 */

trait Trivially extends OOApproachImplementationProvider with BaseDataTypeAsInterface with SharedOO with OperationInterfaceChain with FieldDefinition with FactoryConcepts {
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]

  import ooParadigm._
  import paradigm._
  import syntax._

 // lazy val finalName:Name = names.mangle("Final")
  lazy val finalized:Name = names.mangle("finalized")     // sub package within each evolution that contains final classes

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
   *   public interface Sub extends ep.m1.Exp {
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

      for {
        parent <-  toTargetLanguageType(TypeRep.DataType(tpe))
        _ <- resolveAndAddImport(parent)
        _ <- addParent(parent)

        // only add parent to the chain if NOT the first one
        _ <- if (model.last.isDefined && model.last.get.findTypeCase(tpeCase).isDefined) {
          for {
            parentFormer <- getFormerDerivedInterface(model, tpeCase)
            _ <- resolveAndAddImport(parentFormer)
            _ <- addParent(parentFormer)
          } yield ()
        } else {
          Command.skip[ClassContext]
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
        // these are factory methods in case they are needed for producer methods. Could generate on demand...
        _ <- forEach (model.flatten.typeCases) { tpe =>
          addMethod(names.mangle(names.conceptNameOf(tpe)), createFactoryDataTypeCase(model, tpe))
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
     * HACK: Just duplicate and embed; figure out later.
     * @param model
     * @param tpeCase
     * @return
     */
    def makeFinalClass(model:Model, tpeCase: DataTypeCase): Generator[ProjectContext,Unit] = {
      import ooParadigm.projectCapabilities._

      val makeClass: Generator[ClassContext, Unit] = {
        import ooParadigm.classCapabilities._

        val parentType = derivedInterfaceName(tpeCase)

        for {
          // define based on the derivedInterface for that data type and operation based on
          parent <- findClass(names.mangle(model.name), parentType)
          _ <- resolveAndAddImport(parent)
          _ <- addImplemented(parent)                    // implements derived interface
          _ <- forEach(tpeCase.attributes) { att => makeField(att) }
          _ <- addConstructor(makeConstructor(tpeCase))
          _ <- forEach(tpeCase.attributes) { att =>
            addMethod(getterName(att), makeBodyImpl(model.baseDataType, att, parent))
          }
        } yield ()
      }
      addClassToProject(makeClass, factoryInstanceDataTypeCase(Some(model), tpeCase): _*)
    }

    /** For Trivially, the covariant type needs to be selected whenever a BaseType in the domain is expressed. */
    def domainTypeLookup[Ctxt](covariantType: Name*)(implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]]): Generator[Ctxt, Type] = {
      FindClass(covariantType).interpret(canFindClass)
    }

    /** What model is delivered has operations which is essential for the mapping. */
    override def registerTypeMapping(model: Model): Generator[ProjectContext, Unit] = {
      import ooParadigm.projectCapabilities._
      import paradigm.projectContextCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import ooParadigm.classCapabilities._
      import ooParadigm.constructorCapabilities._
      import paradigm.testCapabilities._
      import ooParadigm.testCapabilities._

      val baseInterface = baseInterfaceNames(model)
      val dtpeRep = TypeRep.DataType(model.baseDataType)
      for {
        _ <- addTypeLookupForMethods(dtpeRep, domainTypeLookup(baseInterface : _*))
        _ <- addTypeLookupForClasses(dtpeRep, domainTypeLookup(baseInterface : _*))
        _ <- addTypeLookupForConstructors(dtpeRep, domainTypeLookup(baseInterface : _*))
      } yield ()
    }

    def implement(domain: Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
      import paradigm.projectContextCapabilities._
      for {
        _ <- debug("Processing Trivially")
        _ <- registerTypeMapping(domain)
        _ <- makeBase(domain.baseDataType, Seq.empty) // Marker interface -- ignores operations

        // whenever new operation, this CREATES the capability of having intermediate interfaces
        _ <- forEach(domain.inChronologicalOrder) { currentModel =>
          // for all PAST dataTypes that are already defined
          for {
            _ <- domainSpecific.initialize(this)
            _ <- registerTypeMapping(currentModel)
            _ <- makeIntermediateInterface(currentModel, domainSpecific)

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

           // get list of all operations and MAP to the most recent model
           _ <- forEach(model.flatten.typeCases) { tpeCase =>
             // must ensure
             addMethod(names.mangle(names.conceptNameOf(tpeCase)), createFactoryDataTypeCase(model, tpeCase))
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

  object Trivially {
    type WithParadigm[P <: AnyParadigm] = Trivially { val paradigm: P }
    type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

    def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
    (base: P)
    (nameProvider: NameProvider[base.syntax.Name],
     oo: ObjectOriented.WithBase[base.type],
     params: ParametricPolymorphism.WithBase[base.type])
    (generics: Generics.WithBase[base.type,oo.type,params.type]): Trivially.WithParadigm[base.type] =
      new Trivially {
        val paradigm: base.type = base
        val names: NameProvider[paradigm.syntax.Name] = nameProvider
        val ooParadigm: oo.type = oo
        val polymorphics: params.type = params
        val genericsParadigm: generics.type = generics
      }
  }

