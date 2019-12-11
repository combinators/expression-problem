package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._
import org.combinators.ep.generator.paradigm._

trait Trivially extends OOApproachImplementationProvider with BaseDataTypeAsInterface with SharedOO with OperationInterfaceChain with FieldDefinition with FactoryConcepts {
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]

  import ooParadigm._
  import paradigm._
  import syntax._

  lazy val finalName:Name = names.mangle("Final")

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
  override def factoryInstanceDataTypeCase(model:Option[Model] = None, tpeCase:DataTypeCase) : Name = {
    val binp = baseInterfaceNamesPrefix(model.get.lastModelWithOperation.get.ops, finalName)
    names.addPrefix(names.conceptNameOf(tpeCase), binp)
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

  def derivedInterfaceName(tpe: DataTypeCase, ops: Seq[Operation]): Name = {
    names.addSuffix(names.mangle(ops.sortWith(_.name < _.name).map(op => names.conceptNameOf(op)).mkString("")), tpe.name)
  }

  /** Former derived interfaced  for the tpe based on its last defined operation. */
  def getFormerDerivedInterface(domain: Model, current:DataTypeCase): Generator[ClassContext, Type] = {
    import classCapabilities._

    findClass(derivedInterfaceName(current, domain.last.get.lastModelWithOperation.get.ops))
  }

  /**
   * For any datatype that *could* have been defined in domain or perhaps was defined earlier
   */
  def makeDerivedInterfaces(domain:Model, tpeCase:DataTypeCase, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val ddn = derivedInterfaceName (tpeCase, domain.ops)
    addClassToProject(ddn, makeDerivedInterface(domain.baseDataType, tpeCase, domain, domainSpecific))
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
   *   public interface AddEval extends ExpEval {
   *
   *     ExpEval getLeft();
   *
   *     ExpEval getRight();
   *
   *     default Double eval() {
   *         return getLeft().eval() + getRight().eval();
   *     }
   * }
   * }}}
   *
   *  add another parent IF there is a prior operation defined before this model.
   * @param tpe
   * @param tpeCase
   * @param domainSpecific
   * @return
   */
  def makeDerivedInterface(tpe: DataType, tpeCase: DataTypeCase, model:Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        parent <- getParentInterface(model, tpe)
        _ <- resolveAndAddImport(parent)
        _ <- addParent(parent)

        _ <- if (model.last.get.flatten.ops.nonEmpty) {  // NOTE: EXAMPLE OF IF ON RIGHT SIDE OF ARROW
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
        _ <- forEach (model.ops) { op =>
          addMethod(names.mangle(names.instanceNameOf(op)), makeImplementation(tpe, tpeCase, op, domainSpecific))
        }

        _ <- setInterface()  // do LAST because then methods with bodies are turned into default methods in interface
      } yield ()
    }

    makeClass
  }

  def makeBodyImpl(baseType:DataType, att:Attribute, parent:Type, covariantType:Name): Generator[MethodBodyContext, Option[Expression]] = {
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
    def makeFinalClass(model:Model, modelDefiningOps:Model, tpeCase: DataTypeCase): Generator[ProjectContext,Unit] = {
      import ooParadigm.projectCapabilities._

      val binp = baseInterfaceNamesPrefix(modelDefiningOps.ops, finalName)
      val actualName = names.addPrefix(names.conceptNameOf(tpeCase), binp)

      val makeClass: Generator[ClassContext, Unit] = {
        import ooParadigm.classCapabilities._

        val parentType = derivedInterfaceName(tpeCase, modelDefiningOps.ops)
        val baseInterface = baseInterfaceNames(model, modelDefiningOps.ops)
        for {
          // define based on the derivedInterface for that data type and operation
          parent <- findClass(parentType)
          _ <- resolveAndAddImport(parent)
          _ <- addImplemented(parent)                    // implements derived interface
          _ <- forEach(tpeCase.attributes) { att => makeField(att) }
          _ <- addConstructor(makeConstructor(tpeCase))
          _ <- forEach(tpeCase.attributes) { att =>
            addMethod(getterName(att), makeBodyImpl(model.baseDataType, att, parent, baseInterface))
          }
        } yield ()
      }
      addClassToProject(actualName, makeClass)
    }

    /** For Trivially, the covariant type needs to be selected whenever a BaseType in the domain is expressed. */
    def domainTypeLookup[Ctxt](covariantType: Name)(implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]]): Generator[Ctxt, Type] = {
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

      val baseInterface = baseInterfaceNames(model, model.ops)
      val dtpeRep = TypeRep.DataType(model.baseDataType)
      for {
        _ <- addTypeLookupForMethods(dtpeRep, domainTypeLookup(baseInterface))
        _ <- addTypeLookupForClasses(dtpeRep, domainTypeLookup(baseInterface))
        _ <- addTypeLookupForConstructors(dtpeRep, domainTypeLookup(baseInterface))
      } yield ()
    }

    def implement(domain: Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
      import paradigm.projectContextCapabilities._
      for {
        _ <- debug ("Processing Trivially")
        _ <- registerTypeMapping(domain)
        _ <- domainSpecific.initialize(this)
        _ <- makeBase(domain.baseDataType, Seq.empty)     // Marker interface -- ignores operations

        // whenever new operation, this CREATES the capability of having intermediate interfaces
        _ <- forEach (domain.inChronologicalOrder.filter(m => m.ops.nonEmpty)) { model =>
          // for all PAST dataTypes that are already defined
          for {
            _ <- registerTypeMapping(model)
            _ <- makeIntermediateInterface(model, domainSpecific)

            _ <- forEach(model.flatten.typeCases) { tpe =>
              makeDerivedInterfaces(model, tpe, domainSpecific)
            }
          } yield ()
        }

        // if current state doesn't declare an operation then you need to iterate over everything in the past
        // that *did* declare an operation [i.e., find those models] and create derived types for these new types
        // that *do* declare operations and need to create derived data types
        _ <- forEach (domain.inChronologicalOrder.filter(m => m.ops.isEmpty && m.typeCases.nonEmpty)) { modelDeclaringTypeCase =>
          forEach (modelDeclaringTypeCase.typeCases) { tpe =>
            forEach(modelDeclaringTypeCase.inChronologicalOrder.filter(m => m.ops.nonEmpty)) { modelDeclaringOp =>
              for {
                _ <- registerTypeMapping(modelDeclaringOp)
                _ <- makeDerivedInterfaces(modelDeclaringOp, tpe, domainSpecific)
              } yield ()
            }
          }
        }

        /** For each DataType AND [cross-product] Operation you need a final Class. */
        _ <- forEach (domain.flatten.typeCases) { tpeCase => {
            val modelDefiningType = domain.findTypeCase(tpeCase).get
            forEach (domain.flatten.ops) { op => {
              val modelDefiningOp = domain.findOperation(op).get
              for {
                _ <- registerTypeMapping(modelDefiningOp)
                _ <- makeFinalClass(modelDefiningType, modelDefiningOp, tpeCase)
              } yield ()
            }
            }
          }
        }
      } yield ()
    }

  /**
   * Override standard factory name for a dataTypeCase.
   *
   * {{{
   *   AddPrettypFinal
   * }}}
   *
   * Model is passed in should it become necessary to be overridden more specifically
   *
   * @param tpeCase    DataTypeCase for which a factory is desired.
   * @return
   */
  override def factoryNameDataTypeCase(model:Option[Model], tpeCase:DataTypeCase) : Name = {
   val lastModelWithOp = model.get.lastModelWithOperation.get

    val binp = baseInterfaceNamesPrefix(lastModelWithOp.ops, finalName)
    names.addPrefix(names.conceptNameOf(tpeCase), binp)
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

      _ <-
        forEach(tests.toList) { case (model, tests) => {
          val testCode: Generator[MethodBodyContext, Seq[Expression]] =
            for {
              code <- forEach(tests) {
                test => testImplementationProvider.test(this)(test)
              }
            } yield code.flatten

          import ooParadigm.testCapabilities._

          val compUnit = for {

            // add test case first
            _ <- addTestCase(testName, testCode)

            // get list of all operations and MAP to the most recent model
            _ <- forEach(model.flatten.typeCases) { tpeCase =>
              // must ensure
              addMethod(names.mangle(names.conceptNameOf(tpeCase)), createFactoryDataTypeCase(model, tpeCase))
            }

          } yield()

          val testSuite = for {
            _ <- addTestSuite(
              testCaseName(model),
              compUnit
            )
          } yield ()

          for {
            _ <- registerTypeMapping(model.lastModelWithOperation.get)   // must come here since it registers mappings that exist in the ProjectContext
            _ <- addCompilationUnit(
                    testCaseName(model),
                    testSuite
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
