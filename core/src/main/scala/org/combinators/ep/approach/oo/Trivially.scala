package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._
import org.combinators.ep.generator.paradigm._

trait Trivially extends ApproachImplementationProvider with SharedOO {
  //val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]

  import ooParadigm._
  import paradigm._
  import syntax._

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
      //rt <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      //_ <- resolveAndAddImport(rt)
      //res <- instantiateObject(rt, args)
      thisRef <- selfReference()
      factory <- getMember(thisRef, names.mangle(names.conceptNameOf(tpeCase)))
      res <- apply(factory, args)
    } yield res
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
          getter <- getMember(thisRef, names.addPrefix("get", names.mangle(names.conceptNameOf(att))))
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

  /**
   * Base Exp interface with no methods (for now).
   *
   * {{{
   *   public interface Exp {
   *
   *     public tree.Tree astree();    // only when needed
   * }
   * }}}
   * Eventually will have some here for producer/binary methods
   *
   * Override traditional OO use where this is a class; here it is an interface
   *
   * @param tpe
   * @param ops -- ignored in this overridden capability
   * @return
   */
  override def makeBase(tpe: DataType, ops: Seq[Operation]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- setInterface()
      } yield ()
    }
    addClassToProject(names.mangle(names.conceptNameOf(tpe)), makeClass)
  }

  def baseInterfaceNamesPrefix(ops: Seq[Operation], suffix:Name): Name = {
    // Note: foldLeft requires swap of comparison operation because....
    ops.sortWith(_.name > _.name).map(op => names.conceptNameOf(op)).foldLeft(suffix){ case (n,s) => names.addPrefix(s, n) }
  }

  def baseInterfaceNames(domain: Model, ops: Seq[Operation]): Name = {
    baseInterfaceNamesPrefix(ops, names.mangle(domain.baseDataType.name))
  }

  def derivedInterfaceName(tpe: DataTypeCase, ops: Seq[Operation]): Name = {
    names.addSuffix(names.mangle(ops.sortWith(_.name < _.name).map(op => names.conceptNameOf(op)).mkString("")), tpe.name)
  }

  // extends Exp [first one] or ExpEval [previous one]
  // Works for both Exp* interface declarations as well as DataTypeOp declarations
  def getParentInterface(domain: Model, tpe: DataType): Generator[ClassContext, Type] = {
    import classCapabilities._

    if (domain.isEmpty || domain.lastModelWithOperation.isEmpty) {
      toTargetLanguageType(TypeRep.DataType(tpe))
    } else {
      findClass(baseInterfaceNames(domain, domain.lastModelWithOperation.get.ops))
    }
  }

  /** Former derived interfaced  for the tpe based on its last defined operation. */
  def getFormerDerivedInterface(domain: Model, current:DataTypeCase): Generator[ClassContext, Type] = {
    import classCapabilities._

    findClass(derivedInterfaceName(current, domain.last.get.lastModelWithOperation.get.ops))
  }

  /**
   * Create intermediate interfaces, using the default implementation capability of Java 1.8
   *
   * {{{
   *  public interface ExpEval extends Exp {
   *   public Double eval();
   * }
   *
   * public interface ExpPrettyp extends ExpEval {
   *     public String prettyp();
   * }
   * }}}
   *
   * NOTE: a DataType there can exist only one so you don't have to pass in IF you have a Model instance
   *
   * @param tpe
   * @param domainSpecific
   * @return
   */
  def makeIntermediateInterface(domain:Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    // create class which is an interface containing abstract methods
    val makeInterface: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- setInterface()
        parent <- getParentInterface(domain.last.get, domain.baseDataType)
        _ <- resolveAndAddImport(parent)
        _ <- addParent(parent)
        _ <- forEach (domain.ops) { op => addAbstractMethod(names.mangle(names.instanceNameOf(op)), makeSignature(op)) }
      } yield ()
    }

    addClassToProject(baseInterfaceNames(domain, domain.ops), makeInterface)
  }

  /**
   * For any datatype that *could* have been defined in domain or perhaps was defined earlier
   */
  def makeDerivedInterfaces(domain:Model, tpeCase:DataTypeCase, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val ddn = derivedInterfaceName (tpeCase, domain.ops)
    addClassToProject(ddn, makeDerivedInterface(domain.baseDataType, tpeCase, domain, domainSpecific))
  }

  /** Make a single getter method SIGNATURE ONLY for the 'att' attribute, such as:
   * {{{
   * public abstract ExpEval getRight();
   * }}}
   *
   * Need to identify appropriate derived interface from existing ones based on new operations
   * @param att
   * @return
   */
  def makeGetterInterface(baseType:DataType, att:Attribute, parent:Type): Generator[ClassContext, Unit] = {
    val makeBody: Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._

      // TODO: How to get working TypeRep from a baseType:DataType
      val realParent:Generator[MethodBodyContext, Type] = if (att.tpe.equals(TypeRep.DataType(baseType)))  {
        Command.lift(parent)
      } else {
        for {
          pt <- toTargetLanguageType(att.tpe)
          _ <- resolveAndAddImport(pt)
        } yield pt
      }

      for {
        pt <- realParent   // PULL OUT of the context
        _ <- setReturnType(pt)
        _ <- setAbstract()
      } yield (None)
    }

    import ooParadigm.classCapabilities._
    addMethod(names.addPrefix("get", names.mangle(names.conceptNameOf(att))), makeBody)
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
    import ooParadigm.projectCapabilities._

    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        parent <- getParentInterface(model, tpe)
        _ <- debug("parent:" + parent)
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
        _ <- forEach (tpeCase.attributes) { att => makeGetterInterface(model.baseDataType, att, parent) }
        _ <- forEach (model.ops) { op =>
          addMethod(names.mangle(names.instanceNameOf(op)), makeImplementation(tpe, tpeCase, op, domainSpecific))
        }

        _ <- setInterface()  // do LAST because then methods with bodies are turned into default methods in interface
      } yield ()
    }
    println("  makeDerivedInterface:" + tpeCase.name + "," + model.name)
    makeClass
  }

  def covariantTypeOrPrimitive(baseType:DataType, att:Attribute, covariantType:Name):Generator[MethodBodyContext, Type] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    if (att.tpe.equals(TypeRep.DataType(baseType)))  {
        for {
          pt <- findClass(covariantType)
          _ <- resolveAndAddImport(pt)
        } yield pt
      } else {
        for {
          pt <- toTargetLanguageType(att.tpe)
          _ <- resolveAndAddImport(pt)
        } yield pt
      }
    }


    def makeBodyImpl(baseType:DataType, att:Attribute, parent:Type, covariantType:Name): Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._

//      // TODO: How to get working TypeRep from a baseType:DataType
//      val realParent:Generator[MethodBodyContext, Type] = {
//        if (att.tpe.equals(TypeRep.DataType(baseType)))  {
//          for {
//            pt <- findClass(covariantType)
//            _ <- resolveAndAddImport(pt)
//          } yield pt
//      } else {
//          for {
//            pt <- toTargetLanguageType(att.tpe)
//            _ <- resolveAndAddImport(pt)
//          } yield pt
//        }
//      }

      for {
        pt <- covariantTypeOrPrimitive(baseType, att, covariantType)   // PULL OUT of the context
        _ <- setReturnType(pt)

        // provide an implementation here. but how?
        self <- selfReference()
        result <- getMember(self, names.mangle(names.instanceNameOf(att)))
      } yield Some(result)
    }

  /** Trivially fields are based on closest derived interface. TODO: UGLY CLEAN UP! */
  def makeTriviallyField(baseType: DataType, att: Attribute, covariantType:Name): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    if (att.tpe.equals(TypeRep.DataType(baseType))) {
      for {
        ft <- findClass(covariantType)
        _ <- resolveAndAddImport(ft)
        _ <- addField(names.mangle(names.instanceNameOf(att)), ft)
      } yield ()
    } else {
      for {
        ft <- toTargetLanguageType(att.tpe)
        _ <- resolveAndAddImport(ft)
        _ <- addField(names.mangle(names.instanceNameOf(att)), ft)
      } yield ()
    }
  }

  def makeTriviallyConstructor(baseType: DataType, tpeCase: DataTypeCase, covariantType:Name): Generator[ConstructorContext, Unit] = {
    import ooParadigm.constructorCapabilities._
    for {
      params <- forEach (tpeCase.attributes) { att: Attribute =>
        if (att.tpe.equals(TypeRep.DataType(baseType))) {
          for {
            at <- findClass(covariantType)
            _ <- resolveAndAddImport(at)
            pName <- freshName(names.mangle(names.instanceNameOf(att)))
          } yield (pName, at)
        } else {
          for {
            at <- toTargetLanguageType(att.tpe)
            _ <- resolveAndAddImport(at)
            pName <- freshName(names.mangle(names.instanceNameOf(att)))
          } yield (pName, at)
        }
      }
      _ <- setParameters(params)
      args <- getArguments()
      _ <- forEach(tpeCase.attributes.zip(args)) { case (att, (_, _, exp)) =>
        initializeField(names.mangle(names.instanceNameOf(att)), exp)
      }
    } yield ()
  }


  /**
     * HACK: Just duplicate and embed; figure out later.
     * @param model
     * @param tpeCase
     * @return
     */
    def makeFinalClass(model:Model, modelDefiningOps:Model, tpeCase: DataTypeCase): Generator[ProjectContext,Unit] = {
      import ooParadigm.projectCapabilities._

      val binp = baseInterfaceNamesPrefix(modelDefiningOps.ops, names.mangle("Final"))
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
          _ <- forEach(tpeCase.attributes) { att => makeTriviallyField(model.baseDataType, att, baseInterface) }
          _ <- addConstructor(makeTriviallyConstructor(model.baseDataType, tpeCase, baseInterface))
          _ <- forEach(tpeCase.attributes) { att =>
            addMethod(names.addPrefix("get", names.mangle(names.conceptNameOf(att))), makeBodyImpl(model.baseDataType, att, parent, baseInterface))
          }
        } yield ()
      }
      addClassToProject(actualName, makeClass)
    }

    def initializeApproach(domain: Model): Generator[ProjectContext, Unit] = {
      import ooParadigm.projectCapabilities._
      import paradigm.projectContextCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import ooParadigm.classCapabilities._
      import ooParadigm.constructorCapabilities._

      val dtpeRep = TypeRep.DataType(domain.baseDataType)
      for {
        _ <- addTypeLookupForMethods(dtpeRep, domainTypeLookup(domain.baseDataType))
        _ <- addTypeLookupForClasses(dtpeRep, domainTypeLookup(domain.baseDataType))
        _ <- addTypeLookupForConstructors(dtpeRep, domainTypeLookup(domain.baseDataType))
      } yield ()
    }

    def implement(domain: Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {

      for {
        _ <- initializeApproach(domain)
        _ <- domainSpecific.initialize(this)
        _ <- makeBase(domain.baseDataType, Seq.empty)     // Marker interface -- ignores operations

        // whenever new operation, this CREATES the capability of having intermediate interfaces
        _ <- forEach (domain.inChronologicalOrder.filter(m => m.ops.nonEmpty)) { model =>

          // for all PAST dataTypes that are already defined
          for {
            _ <- makeIntermediateInterface(model, domainSpecific)

            _ <- forEach (model.flatten.typeCases)  { tpe =>
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
              makeDerivedInterfaces(modelDeclaringOp, tpe, domainSpecific)
            }
          }
        }

        /** For each DataType AND [cross-product] Operation you need a final Class. */
        _ <- forEach (domain.flatten.typeCases) { tpeCase => {
          val modelDefiningType = domain.findTypeCase(tpeCase).get
          forEach (domain.flatten.ops) { op =>
            makeFinalClass(modelDefiningType, domain.findOperation(op).get, tpeCase)
          }
        }
        }
      } yield ()
    }

  /**
   * AddPrettypFinal Add(PrettypExp left, PrettypExp right) {
   *   return new AddPrettypFinal(left, right);
   * }
   *
   * LitPrettypFinal Lit(Double v) {
   *    return new LitPrettypFinal(v);
   * }
   *
   * @param model
   * @param op
   * @return
   */
  def makeFactoryMethod(model:Model, tpeCase:DataTypeCase): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    // find last operation
    val lastModelWithOp = model.lastModelWithOperation.get

    val binp = baseInterfaceNamesPrefix(lastModelWithOp.ops, names.mangle("Final"))
    val actualName = names.addPrefix(names.conceptNameOf(tpeCase), binp)
    val baseType = model.baseDataType
    val paramType = baseInterfaceNames(lastModelWithOp, lastModelWithOp.ops)  // was model

    for {
      opClass <- findClass(actualName)    // should check!
      _ <- resolveAndAddImport(opClass)
      _ <- setReturnType(opClass)

     params <- forEach (tpeCase.attributes) { att: Attribute =>
      if (att.tpe.equals(TypeRep.DataType(baseType))) {
        for {
          at <- findClass(paramType)
          _ <- resolveAndAddImport(at)
          pName <- freshName(names.mangle(names.instanceNameOf(att)))
        } yield (pName, at)
      } else {
        for {
          at <- toTargetLanguageType(att.tpe)
          _ <- resolveAndAddImport(at)
          pName <- freshName(names.mangle(names.instanceNameOf(att)))
        } yield (pName, at)
      }
    }
    _ <- setParameters(params)

      // HACK. TODO: FIX this up
      argSeq <- getArguments().map( args => { args.map(triple => triple._3) })
     // ctor <- getConstructor(opClass)

      res <- instantiateObject(opClass, argSeq)

      //justArgs <- forEach(tpeCase.attributes.zip(args)) { case (att, (_, _, exp)) => exp }
      //res <- apply(ctor, argSeq)
      //res <- instantiateObject(opClass, Seq.empty)
    } yield Some(res)
  }

  /** Test cases all need factory methods to work.  */
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

//          val pastModelsWithOps = model.inChronologicalOrder.filter(m => m.ops.nonEmpty).flatMap(m =>
//            m.ops.map(op => {
//              val targetModelForOp = model.toSeq.find(m => m.ops.contains(op) || m.typeCases.nonEmpty).get
//              (names.addPrefix("make", names.mangle(names.conceptNameOf(op))), makeFactoryMethod(targetModelForOp, op))
//            })
//          )
          import ooParadigm.testCapabilities._

          val compUnit = for {
            // add test case first
            _ <- addTestCase(names.mangle("Test"), testCode)

            // get list of all operations and MAP to the most recent model
            _ <- forEach(model.flatten.typeCases) { tpeCase =>
              // must ensure
              addMethod(names.mangle(names.conceptNameOf(tpeCase)), makeFactoryMethod(model, tpeCase))
            }

          } yield()

          val testSuite = for {
            _ <- addTestSuite(
              names.addSuffix(names.mangle(names.conceptNameOf(model)), "Test"),
              compUnit
            )
          } yield ()

          addCompilationUnit(
            names.addSuffix(names.mangle(names.conceptNameOf(model)), "Test"),
            testSuite
          )
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

