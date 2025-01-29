package org.combinators.ep.approach.oo    /*DI:LI:AD*/

import org.combinators.ep.domain.GenericModel
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.{forEach, _}
import org.combinators.ep.generator.paradigm._

sealed trait Interpreter extends SharedOO {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]

  import ooParadigm._
  import paradigm._
  import syntax._

  /**
   * Extended Intermediate Interface (i.e., ep.m3.Exp) is only created when
   * needed, specifically: (a) a new operation is being defined, and this interface will host the default
   * implementation; or (b) a branch is being merged from branches in which new Exp had been defined
   * useful when determining merging
   *
   * Also found in CoCo
   *
   * @param Domain under investigation
   */
  def ancestorsDefiningNewTypeInterfaces(domain: GenericModel): Set[GenericModel] = {
    val ancestorsWithNewTypeInterfaces = domain.former.map(ancestor => latestModelDefiningNewTypeInterface(ancestor))

    ancestorsWithNewTypeInterfaces.distinct.filterNot { ancestor =>
      // get rid of everything that has an antecedent
      ancestorsWithNewTypeInterfaces.exists(otherAncestor => ancestor.before(otherAncestor))
    }.toSet
  }

  def updatedImplementationCurrentDomain(domain: GenericModel): Option[GenericModel] = {

    // current domain might have implementation that overrides an existing implementation, and that has
    // to be captured. Go through all past operations and past data types, and see if the domainSpecific dependencies
    var returnVal:Option[GenericModel] = None

    domain.pastOperations.foreach(op =>
      domain.pastDataTypes.foreach(tpe =>
          // cannot just invoke 'haveImplementation' since that covers cases where a new data type has been defined,
          // and so a past operation needs to be implemented. ONLY deal with optimizations
          if (domain.optimizations.contains((tpe, op))) {
            returnVal = Some(domain)
          }
        )
      )

    returnVal
  }

  def updatedImplementationCurrentDomainByType(domain: GenericModel, tpe:DataTypeCase): Option[GenericModel] = {

    // current domain might have implementation that overrides an existing implementation, and that has
    // to be captured. Go through all past operations for this data type, and see if the domainSpecific dependencies
    var returnVal:Option[GenericModel] = None

    domain.pastOperations.foreach(op =>
        if (domain.haveImplementation(PotentialRequest(domain.baseDataType, tpe, op)).contains(domain)) {
          returnVal = Some(domain)
        }
    )

    returnVal
  }

  def latestModelDefiningNewTypeInterface(domain: GenericModel): GenericModel = {
    if (domain.isDomainBase || domain.ops.nonEmpty) {
      domain
    } else {
      // is there a single type that can represent the "least upper bound" of all prior branches.
      val ancestorsWithTypeInterfaces = ancestorsDefiningNewTypeInterfaces(domain)
      if (ancestorsWithTypeInterfaces.size == 1 && !ancestorsWithTypeInterfaces.head.isDomainBase) { // take care to avoid falling below "floor"
        ancestorsWithTypeInterfaces.head
      } else {
        domain // we have to do merge
      }
    }
  }

  def mostSpecificBaseInterfaceType[Context](domain: GenericModel)(implicit
      canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
      canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
      canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]],
      canApplyType: Understands[Context, Apply[paradigm.syntax.Type, paradigm.syntax.Type, paradigm.syntax.Type]],
  ): Generator[Context, paradigm.syntax.Type] = {

    // might have to use current because of revised implementation
    val updateCurrent = updatedImplementationCurrentDomain(domain)
    val resultModelStage = if (updateCurrent.isDefined) {
      latestModelDefiningNewTypeInterface(domain).later(updateCurrent.get)
    } else {
      latestModelDefiningNewTypeInterface(domain)
    }

    for {
      baseInterfaceType <- FindClass[paradigm.syntax.Name, paradigm.syntax.Type](qualifiedBaseDataType(resultModelStage)).interpret(canFindClass)
      _ <- resolveAndAddImport(baseInterfaceType)

    } yield baseInterfaceType
  }

  /** Qualify the Exp properly. */
  def qualifiedBaseDataType(domain: GenericModel): Seq[Name] = {
    if (domain.isDomainBase) {
      Seq(names.mangle(names.conceptNameOf(domain.baseDataType)))
    } else {
      Seq(names.mangle(names.instanceNameOf(domain)), names.mangle(names.conceptNameOf(domain.baseDataType)))
    }
  }

  /** Place dataType classes in appropriate package. */
  def qualifiedDataTypeCase(domain: GenericModel, tpe:DataTypeCase): Seq[Name] = {
    Seq(names.mangle(names.instanceNameOf(domain)), names.mangle(names.conceptNameOf(tpe)))
  }

  //// ----- Base Data Type As Instance

  /**
   * Base Exp interface with no methods (for now).
   *
   * {{{
   *   public interface Exp {
   *     public tree.Tree astree();    // only when needed
   * }
   * }}}
   *
   * Eventually will have some work here for producer/binary methods
   *
   * Override traditional OO use where this is a class; here it is an interface
   *
   * @param tpe -- data type case for which Base is needed
   * @param ops -- ignored in this overridden capability
   * @return
   */
  def makeBase(tpe: DataType, ops: Seq[Operation]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- setInterface()
      } yield ()
    }

    addClassToProject(makeClass, names.mangle(names.conceptNameOf(tpe)))
  }

  /// ------------------------------------------------------ Operator As Chain Moved Here ---------------

  // TODO: should be revised to pass in the base, and not presume it
  def baseInterfaceNames(domain: GenericModel): Seq[Name] = {
    if (domain.isDomainBase) {
      // ignore base Domain, for example, and just grab name...
      Seq(names.mangle(domain.baseDataType.name))
    } else {
      Seq(names.mangle(domain.name), names.mangle(domain.baseDataType.name))
    }
  }

  /**
   * Make sure to include past producer methods as well...
   */
  def makeInterface(domain:GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    // create class which is an interface containing abstract methods
    import classCapabilities._
    import genericsParadigm.classCapabilities._

    val producers = domain.pastOperations.filter(op => op.isProducer(domain))

    // Find any operation that acts on a current-or-past type which has a new dependency
    val opsToBuild = domain.ops.filter(op =>
      domain.pastDataTypes.exists(tpe => domainSpecific.dependencies(PotentialRequest(domain.baseDataType, tpe, op)).isDefined))

    for {
      _ <- setInterface()

      // former merge points need to be included, so  past.lastModelWithOperation) is changed to below. Make
      // sure to 'distinct' these models to avoid adding same interface multiple time (can happen with 3-way merge)
      _ <- forEach(domain.former.map(past => latestModelDefiningNewTypeInterface(past)).distinct) { m => for {
           /** Interpreter has to go back to the former Model which had defined an operation */
           parent <- findClass(qualifiedBaseDataType(m) : _ *)
           _ <- resolveAndAddImport(parent)
           _ <- addParent(parent)
         } yield ()
        }

      // grab current operations AND all producers. Do together to eliminate duplicates.
      _ <- forEach ((domain.ops ++ opsToBuild ++ producers).distinct) { op => addAbstractMethod(names.mangle(names.instanceNameOf(op)), makeInterpreterSignature(domain, op)) }
    } yield ()
  }

  /**
   * Create intermediate interfaces that form a chain of operation definitions.
   * Now this is created EVEN when an evolution doesn't create an operation.
   *
   * Make sure producer methods are subsequently copied...
   */
  def addIntermediateInterfaceToProject(domain:GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    addClassToProject(makeInterface(domain, domainSpecific), baseInterfaceNames(domain) : _*)  }

  /// -------------------------------------^^^^^^^^^^^^^^^^^^^^^^^^^^^^^-----------------------------------------------

  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      method <- getMember(message.to, names.mangle(names.instanceNameOf(message.request.op)))
      result <- apply(method, message.request.op.parameters.map(message.request.arguments))
    } yield result
  }

  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      // goal is to find the class, i.e., "Sub", and have that become "ep.m4.Sub"
      // Using 'toTargetLanguage' brings in the type from where it had been registered using registerTypes
      rt <- toTargetLanguageType(TypeRep.DataType(DataType(tpeCase.name)))
      _ <- resolveAndAddImport(rt)

      res <- instantiateObject(rt, args)
    } yield res
  }

  /** Create signature using contravariant baseDomain type from past Exps.
   *
   * {{{
   *   public Double OPERATION(PARAM...)
   * }}}
   * @param op      operation for which method needs to be written
   * @param domain  current domain for the operation.
   * @return
   */
  def makeInterpreterSignature(domain:GenericModel, op: Operation): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      rt <- toTargetLanguageType(op.returnType)   // covariant overriding is still feasible
      _ <- resolveAndAddImport(rt)
      _ <- setReturnType(rt)
      params <- forEach (op.parameters) { param: Parameter =>
        for {
          pName <- freshName(names.mangle(param.name))
          // must handle case where a particular parameter is not recursive (i.e., is double)
          pt <- if (param.tpe.isModelBase(domain)) {
            findClass(qualifiedBaseDataType(domain.findOperation(op).get) : _ *)
          } else {
            toTargetLanguageType(param.tpe)
          }

          _ <- resolveAndAddImport(pt)
        } yield (pName, pt)
      }
      _ <- setParameters(params)
    } yield ()
  }

  /** Determines whether it is necessary within DataTypeCase to cast inherited attributes.
   *
   * If a new operation is defined, then a local DataType Case Class is created, and it will inherit
   * baseDomain attributes that need to be cast to current domain
   */
  def mustCastToAccess(domain:GenericModel, op:Operation, tpeCase:DataTypeCase) : Boolean = {
    val definingModel = domain.findTypeCase(tpeCase).get

    !latestModelDefiningNewTypeInterface(domain).beforeOrEqual(definingModel)
  }

  def makeInterpreterImplementation(domain: GenericModel,
                          tpe: DataType,
                          tpeCase: DataTypeCase,
                          op: Operation,
                          domainSpecific: EvolutionImplementationProvider[this.type]
                        ): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._
    val properModel = latestModelDefiningOperatorClass(domain, tpeCase, op, domainSpecific).get
    if (properModel != domain) {
      println ("Interpreter::makeInterpreterImplementation chooses " + properModel.name + " over " + domain.name + " for (" + op.name + "," + tpeCase.name + ")")
    }
    for {
      _ <- makeInterpreterSignature(domain, op)
      _ <- if (domain.operationsPresentEarlier(tpeCase).contains(op)) {
          setOverride()
        } else {
          Command.skip[MethodBodyContext]
        }
      thisRef <- selfReference()
      attAccessors: Seq[Expression] <- forEach (tpeCase.attributes) { att =>
        for {
          att_member <- getMember(thisRef, names.mangle(names.instanceNameOf(att)))
          mostSpecificExp <- mostSpecificBaseInterfaceType(domain)
          casted <- if (att.tpe.isModelBase(domain) && mustCastToAccess(domain, op, tpeCase)) {    // only cast if Exp AND comes from older Exp
            castObject(mostSpecificExp, att_member)
          } else {
            Command.lift[MethodBodyContext, Expression](att_member)
          }
        } yield casted
      }

      atts = tpeCase.attributes.zip(attAccessors).toMap

      allArgs <- getArguments()
      castedArgs <- forEach(op.parameters.zip(allArgs)) { case (param,arg) =>
        for {
          mostSpecificExp <- mostSpecificBaseInterfaceType(domain)
          casted <- if (param.tpe.isModelBase(domain)) {    // only cast if Exp
            castObject(mostSpecificExp, arg._3)
          } else {
            Command.lift[MethodBodyContext, Expression](arg._3)
          }
        } yield (param,casted)
      }

      castedArgsMap = castedArgs.toMap

      result <-
        domainSpecific.logic(this)(
          ReceivedRequest(
            tpe,
            tpeCase,
            thisRef,
            atts,
            Request(op, castedArgsMap),
            Some(properModel)   // scala implementation for j8 needed this
          )
        )
    } yield result
  }

  /** Generate class for each DataTypeCase and Operation. Be sure to keep extension chain going when there are prior classes available.
   *
   * package ep.m2;
   *
   * public class Sub extends ep.m1.Sub implements Exp {
   *    ...
   * }
   *
   * @return
   */
  def makeClassForCase(model: GenericModel, ops: Seq[Operation], tpeCase: DataTypeCase,
           domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    import classCapabilities._

    // if an ancestor branch doesn't define parent then add all operations

    // Even though there *may* be multiple formers (due to a merge) we can't actually add multiple
    // parents. If there are multiple, then must find which branch defines the tpeCase and choose
    // that one -- if both do, then it doesn't matter which one you choose, so to be consistent
    // always choose the first one.
    def addPrimaryParent(): Generator[ClassContext, Option[Type]] = {
      if (!shouldAddParent) {
       Command.lift[ClassContext, Option[Type]](Option.empty)
      } else {
        // go to last model which had defined operation *OR* last model in which this tpeCase is defined.
        val chosenModel = primaryParent(model, tpeCase)
        for {
          priorType <- findClass(qualifiedDataTypeCase(chosenModel, tpeCase) : _ *)
            _ <- resolveAndAddImport(priorType)
            _ <- addParent(priorType)
          } yield Some(priorType)
        }
      }

    // add a parent IF type defined earlier, in ANY of its formers..
    def shouldAddParent: Boolean = {
      model.former.exists(m => m.findTypeCase(tpeCase).isDefined)
    }

    for {  // find latest model with operation, since that will be the proper one to use
      pt <- findClass(qualifiedBaseDataType(latestModelDefiningNewTypeInterface(model)) : _ *)
       _ <- resolveAndAddImport(pt)
       _ <- addImplemented(pt)

       primaryParent <- addPrimaryParent()
       _ <- if (!shouldAddParent) {
         for {
           _ <- forEach(tpeCase.attributes) { att => makeField(att) }
         } yield ()
       } else {
         Command.skip[ClassContext]
       }

       // if super is being used, then you need to cast to Exp
       _ <- addConstructor(makeConstructor(tpeCase, !shouldAddParent, useSuper = primaryParent))
       _ <- forEach (ops) { op => addMethod(names.mangle(names.instanceNameOf(op)), makeInterpreterImplementation(model, model.baseDataType, tpeCase, op, domainSpecific))
       }
    } yield ()
  }

  def generateForOpForType(ops:Seq[Operation], defining:GenericModel, tpeCase:DataTypeCase, domainSpecific: EvolutionImplementationProvider[this.type]) : Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    for {
      _ <- addClassToProject(makeClassForCase(defining, ops, tpeCase, domainSpecific), qualifiedDataTypeCase(defining, tpeCase) : _ *)
    } yield ()
  }

  /** Class is generated into 'defining' namespace package.  */
  def generateForOp(defining:GenericModel, allTypes:Seq[DataTypeCase], domainSpecific: EvolutionImplementationProvider[this.type]) : Generator[ProjectContext, Unit] = {
    for {
        _ <- forEach (allTypes) { tpe => {
          val prime = primaryParent(defining, tpe)
          val necessaryOps = if (defining.typeCases.contains(tpe)) { defining.flatten.ops.distinct} else { defining.ops }
          val missing = defining.flatten.ops.distinct.filter(op => (! prime.supports(op)) || op.isProducer(defining))
          val overridden = defining.flatten.ops.distinct.filter(op => {
            val selectMap = domainSpecific.evolutionSpecificDependencies(PotentialRequest(defining.baseDataType, tpe, op))
            val hasDependency = domainSpecific.dependencies(PotentialRequest(defining.baseDataType, tpe, op)).isDefined
            hasDependency && selectMap.contains(defining)
          })
          if ((missing ++ necessaryOps ++ overridden).isEmpty) {
            Command.skip[ProjectContext]
          } else {
            generateForOpForType((missing ++ necessaryOps ++ overridden).distinct, defining, tpe, domainSpecific)   // don't forget to include self (defining)
          }
        }
      }
    } yield ()
  }

  /** For Interpreter, the covariant type needs to be selected whenever a BaseType in the domain is expressed. */
  def domainTypeLookup[Ctxt](covariantType: Name*)(implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]]): Generator[Ctxt, Type] = {
    FindClass(covariantType).interpret(canFindClass)
  }

  def paramType[Context](domain:GenericModel, tpe:DataTypeCase)(implicit
         canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
         canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
         canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]]
        ) : Generator[Context, Type] = {
    for {
      cname <- FindClass[paradigm.syntax.Name, paradigm.syntax.Type](qualifiedDataTypeCase(domain, tpe)).interpret(canFindClass)
      _ <- resolveAndAddImport(cname)
    } yield cname
  }

  def latestModelDefiningNewTypeInterface(domain: GenericModel, tpe:DataTypeCase): GenericModel = {
    val latestDefiningInterface = latestModelDefiningNewTypeInterface(domain)
    if (domain == latestDefiningInterface) {   // handle merge case as well
      domain
    } else {
      // find where tpe was defined
      val tpeDefined = domain.findTypeCase(tpe).get
      val past = domain.inChronologicalOrder
      val pastWithTpeAndInterfaceDefined = past.filter(p => tpeDefined.beforeOrEqual(p) && latestDefiningInterface.beforeOrEqual(p))
      pastWithTpeAndInterfaceDefined.head
     }
  }

  /** Enables mapping each DataType to the designated ep.?.DT class. */
  def registerTypeCases(domain:GenericModel) : Generator[ProjectContext, Unit] = {
    import ooParadigm.classCapabilities.{addTypeLookupForMethods => _, addTypeLookupForClasses => _, addTypeLookupForConstructors => _,_}
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import ooParadigm.constructorCapabilities._
    import ooParadigm.projectCapabilities._
    import paradigm.projectCapabilities._
     for {
      _ <- forEach(domain.flatten.typeCases) { tpe => {

          val updateCurrent = updatedImplementationCurrentDomainByType(domain, tpe)
          val tpeDomain = if (updateCurrent.isDefined) {
            latestModelDefiningNewTypeInterface(domain, tpe).later(updateCurrent.get)
          } else {
            latestModelDefiningNewTypeInterface(domain, tpe)
          }

        for {
            _ <- addTypeLookupForMethods(TypeRep.DataType(DataType(tpe.name)), paramType[MethodBodyContext](tpeDomain, tpe))
            _ <- addTypeLookupForClasses(TypeRep.DataType(DataType(tpe.name)), paramType[ClassContext](tpeDomain, tpe))
            _ <- addTypeLookupForConstructors(TypeRep.DataType(DataType(tpe.name)), paramType[ConstructorContext](tpeDomain, tpe))
          } yield ()
        }
      }
    } yield()
  }

  /** What model is delivered has operations which is essential for the mapping. */
  override def registerTypeMapping(model: GenericModel): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    import ooParadigm.classCapabilities.canFindClassInClass
    import ooParadigm.constructorCapabilities.canFindClassInConstructor
    import ooParadigm.methodBodyCapabilities.canFindClassInMethod
    import paradigm.projectCapabilities._

    val baseInterface = baseInterfaceNames(latestModelDefiningNewTypeInterface(model))
    val dtpeRep = TypeRep.DataType(model.baseDataType)
    for {
      _ <- addTypeLookupForMethods(dtpeRep, domainTypeLookup(baseInterface : _*))
      _ <- addTypeLookupForClasses(dtpeRep, domainTypeLookup(baseInterface : _*))
      _ <- addTypeLookupForConstructors(dtpeRep, domainTypeLookup(baseInterface : _*))
    } yield ()
  }

  /**
   * Find the (possibly older) model that actually defines this type case. Now, if some former model defines
   * an operation and the model defining the type exists, then return the last.
   *
   * Look at all past branches and see if any is behind you and select the latest one of those.
   */
  def primaryParent(model:GenericModel, tpeCase:DataTypeCase): GenericModel = {
    val modelDefiningType = model.findTypeCase(tpeCase)
    val pastModels = model.former.filter(m => modelDefiningType.getOrElse(m).before(m)).map(m => latestModelDefiningNewTypeInterface(m, tpeCase))
    pastModels.foldLeft(modelDefiningType.get)((latest,m) => latest.later(m))
  }

  // Have to process models in chronological order to get the Exp mappings properly done.
  // make sure that LATER merges do not generate.
  def implement(model: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    def implementInner(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
      val typeCasesNotGeneratedYet = domain.flatten.typeCases.distinct.filter(tpeCase => latestModelDefiningNewTypeInterface(domain, tpeCase) == domain)

      val createLevel = (domain == latestModelDefiningNewTypeInterface(domain)) ||
        updatedImplementationCurrentDomain(domain).isDefined
      if (domain.isDomainBase) {
        Command.skip[paradigm.ProjectContext]
      } else {
        for {
          _ <- registerTypeMapping(domain)       // this must be first SO Exp is properly bound within interfaces
          _ <- registerTypeCases(domain)         // handle DataType classes as well for interpreter
          _ <- if (createLevel) {
            for {
             _ <- addIntermediateInterfaceToProject(domain, domainSpecific)   // Exp for each evolution that needs one
             _ <- generateForOp(domain, domain.flatten.typeCases, domainSpecific)
            } yield ()
          } else {
           for {
             _ <- generateForOp(domain, typeCasesNotGeneratedYet, domainSpecific)
           } yield ()
          }

        } yield()
      }
    }

    for {
      _ <- domainSpecific.initialize(this)
      _ <- makeBase(model.baseDataType, Seq.empty)
      _ <- forEach(model.inChronologicalOrder) { m => implementInner(m) }
    } yield ()
  }

  /** Need to register type mappings appropriately, in order. */
  override def implement(tests: Map[GenericModel, Seq[TestCase]], testImplementationProvider: TestImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    import paradigm.projectCapabilities._
    import paradigm.compilationUnitCapabilities._
    import paradigm.testCapabilities._

    def implementInner(domain: GenericModel, tests:Seq[TestCase]): Generator[paradigm.ProjectContext, Unit] = {
      val testCode: Generator[MethodBodyContext, Seq[Expression]] =
        for {
          code <- forEach(tests) {
            test => testImplementationProvider.test(this)(test)
          }
        } yield code.flatten

      val compUnit = for {
        _ <- addTestCase(testCode, testName)
      } yield()

      val testSuite = for {
        _ <- addTestSuite(testCaseName(domain), compUnit)
      } yield ()

      for {
        _ <- addCompilationUnit(testSuite, testCaseName(domain))
      } yield()
    }

    for {
      _ <- forEach(tests.toList) { case (model, tests) =>
        for {
          _ <- registerTypeMapping(model)    // this must be first SO Exp is properly bound within interfaces
          _ <- registerTypeCases(model)      // handle DataType classes as well for interpreter
          _ <- implementInner(model, tests)
        } yield ()
      }
    } yield ()
  }
}

object Interpreter {
    type WithParadigm[P <: AnyParadigm] = Interpreter { val paradigm: P }
    type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

    def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
    (base: P)
    (nameProvider: NameProvider[base.syntax.Name],
     oo: ObjectOriented.WithBase[base.type],
     parametricPolymorphism: ParametricPolymorphism.WithBase[base.type])
    (generics: Generics.WithBase[base.type, oo.type, parametricPolymorphism.type]): Interpreter.WithParadigm[base.type] =
      new Interpreter {
        val paradigm: base.type = base
        val names: NameProvider[paradigm.syntax.Name] = nameProvider
        val ooParadigm: oo.type = oo
        val polymorphics: parametricPolymorphism.type = parametricPolymorphism
        val genericsParadigm: generics.type = generics
      }
  }
