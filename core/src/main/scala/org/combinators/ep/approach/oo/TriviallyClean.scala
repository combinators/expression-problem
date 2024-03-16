package org.combinators.ep.approach.oo     /*DI:LI:AD*/

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.{GenericModel, abstractions}
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._
import org.combinators.ep.generator.paradigm._

import scala.annotation.tailrec

trait TriviallyClean extends ApproachImplementationProvider {
  val paradigm: AnyParadigm
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val names: NameProvider[paradigm.syntax.Name]

  import paradigm._
  import syntax._

  object ComponentNames {
    val finalizedPackage = names.mangle("finalized")

    def getter(attribute: abstractions.Attribute): paradigm.syntax.Name = {
      names.addPrefix("get", names.mangle(names.conceptNameOf(attribute)))
    }
  }

  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      method <- getMember(message.to, names.mangle(names.instanceNameOf(message.request.op)))
      result <- apply(method, message.request.op.parameters.map(message.request.arguments))
    } yield result
  }

  /** Before instantiate is called, use this to register finalized classes. */
  case class FinalizedDataTypeCase(tpeCase: DataTypeCase) extends TypeRep

  /**
   * Instantiating an instance of the data type.
   *
   * public void testTest() {
   *   org.junit.Assert.assertTrue("", Double.valueOf(new finalized.Add(new Lit(1.0), new Lit(2.0)).eval()).equals(3.0));
   *   org.junit.Assert.assertTrue("", Double.valueOf(new Lit(5.0).eval()).equals(5.0));
   * }
   *
   * @param baseTpe  Top-level Exp
   * @param tpeCase  Specific data type case for object to be instantiated
   * @param args     necessary arguments to the constructor
   * @return
   */
  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      finalizedClass <- toTargetLanguageType(FinalizedDataTypeCase(tpeCase))
      _ <- resolveAndAddImport(finalizedClass)
      res <- instantiateObject(finalizedClass, args)
    } yield res
  }

  /** Find the most specific Exp class currently at play, given the current domain. */
  def mostSpecificBaseInterfaceType[Context](domain: GenericModel)(implicit
          canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
          canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
          canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]]
  ): Generator[Context, paradigm.syntax.Type] = {

    val _latestDomainDefiningInterface = latestModelDefiningInterface(domain)
    for {
      baseInterfaceType <-
        FindClass[paradigm.syntax.Name, paradigm.syntax.Type](Seq(names.mangle(names.instanceNameOf(_latestDomainDefiningInterface)), names.mangle(names.conceptNameOf(_latestDomainDefiningInterface.baseDataType)))).interpret(canFindClass)
      _ <- resolveAndAddImport(baseInterfaceType)
    } yield baseInterfaceType
  }

  /** Find the most specific Exp class currently at play, given the current domain. */
  def givenBaseInterfaceType[Context](domain: GenericModel, tpe:DataTypeCase)(implicit
         canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
         canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
         canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]]
  ): Generator[Context, paradigm.syntax.Type] = {

    for {
      baseInterfaceType <-
        FindClass[paradigm.syntax.Name, paradigm.syntax.Type](Seq(names.mangle(names.instanceNameOf(domain)), names.mangle(names.conceptNameOf(tpe)))).interpret(canFindClass)
      _ <- resolveAndAddImport(baseInterfaceType)
    } yield baseInterfaceType
  }

  /** Find the last evolution that requires its own Exp definition. */
  def latestModelDefiningInterface(domain: GenericModel): GenericModel = {

    if (domain.isDomainBase || domain.ops.nonEmpty || domain.former.length > 1) {   // handle merge case as well
      domain
    } else {
      // find where tpe was defined and also where last operation was defined and choose later of the two
      // will only have one, since merge case handled above.
      // could be one of our ancestors is a merge point

      // is there a single type that can represent the "least upper bound" of all prior branches. (TAKEN FROM COCO)
      val ancestorsWithTypeInterfaces = domain.former.map(ancestor => latestModelDefiningInterface(ancestor)).distinct
      // To validate this works, need multiple branches where NEITHER defines operators
      if (ancestorsWithTypeInterfaces.size == 1 && !ancestorsWithTypeInterfaces.head.isDomainBase) { // take care to avoid falling below "floor"
        ancestorsWithTypeInterfaces.head
      } else {
        domain // we have to do merge
      }
    }

  }

  def newerTypeCasesSinceInterface(domain:GenericModel) : Seq[DataTypeCase] = {
    val lastExp = latestModelDefiningInterface(domain)
    val toRemove = lastExp.flatten.typeCases

    // will have to allow those IN the interface extension as well
    domain.flatten.typeCases.filterNot(tpe => toRemove.contains(tpe)).seq
  }

  // binary methods need fixing since EARLIER evolutions would use EARLIER types
  def setOperationMethodSignature(domain: GenericModel, operation: Operation): Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      returnTypeInterface <- mostSpecificBaseInterfaceType(domain)
      parameterTypeInterface <- mostSpecificBaseInterfaceType(domain.findOperation(operation).get)

      // Translate result and parameter types
      signature <- forEach(operation.parameters.map(_.tpe)) { signatureTypeRep =>
        if (signatureTypeRep.isModelBase(domain)) {
          Command.lift[paradigm.MethodBodyContext, paradigm.syntax.Type](parameterTypeInterface)
        } else {
          for {
            signatureType <- toTargetLanguageType(signatureTypeRep)
            _ <- resolveAndAddImport(signatureType)
          } yield signatureType
        }
      }
      _ <- setParameters(operation.parameters.map(param => names.mangle(param.name)).zip(signature))

     returnType <-  if (operation.returnType.isModelBase(domain)) {
        Command.lift[paradigm.MethodBodyContext, paradigm.syntax.Type](returnTypeInterface)
      } else {
          for {
          signatureType <- toTargetLanguageType(operation.returnType)
          _ <- resolveAndAddImport(signatureType)
        } yield signatureType
    }
      _ <- setReturnType(returnType)
    } yield ()
  }

  def addNewTypeInterface(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
    def makeNewTypeInterface(): Generator[ooParadigm.ClassContext, Unit] = {
      import ooParadigm.classCapabilities._

      // because of merging there could be duplication
      val existingParentDomains =
        domain.former.filterNot(p => p.isDomainBase).map(ancestor => latestModelDefiningInterface(ancestor)).distinct

      for {
        // add all parents (now made unique)
        _<- forEach(existingParentDomains) { ancestor =>
          for {
            parentTypeInterface <- mostSpecificBaseInterfaceType(ancestor)
            _ <- addParent(parentTypeInterface)
          } yield ()
        }

        producerOperations = domain.flatten.ops.filter(op => op.isProducer(domain))

        // Add methods for new operations
        _ <- forEach((domain.ops ++ producerOperations).distinct) { operation =>
          ooParadigm.classCapabilities.addAbstractMethod(
            names.mangle(names.instanceNameOf(operation)),
            setOperationMethodSignature(domain, operation)
          )
        }

        _ <- setInterface()
      } yield ()
    }

     import ooParadigm.projectCapabilities._
     addClassToProject(makeNewTypeInterface(), names.mangle(names.instanceNameOf(domain)), names.mangle(names.conceptNameOf(domain.baseDataType)))
  }

  def mostSpecificModel(domain:GenericModel, dataTypeCase:DataTypeCase, domainSpecific:EvolutionImplementationProvider[this.type]) : Option[GenericModel] = {
    val potential = dataTypeCasesWithNewOperations(domain, domainSpecific)
    val haveChanged = potential.exists(pair => pair._1 == dataTypeCase && pair._2.nonEmpty)
    val _latestModelDefiningDataTypeCaseInterface = if (domain.findTypeCase(dataTypeCase).isDefined) {
        if (haveChanged) {
          Some(domain)
        } else {
          //Some(latestModelDefiningInterface(domain).later(domain.findTypeCase(dataTypeCase).get))
          domain.findTypeCase(dataTypeCase)
        }
      } else {
        None
      }
    _latestModelDefiningDataTypeCaseInterface
  }

  def mostSpecificTypeCaseInterface[Context](domain: GenericModel, dataTypeCase:DataTypeCase, domainSpecific: EvolutionImplementationProvider[this.type])(implicit
         canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
         canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
         canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]],
  ): Generator[Context, Option[paradigm.syntax.Type]] = { // this should be if DEFINED here...

    val potential = dataTypeCasesWithNewOperations(domain, domainSpecific)
    val haveChanged = potential.exists(pair => pair._1 == dataTypeCase && pair._2.nonEmpty)
    val _latestModelDefiningDataTypeCaseInterface = if (domain.findTypeCase(dataTypeCase).isDefined) {
      val definedIn = domain.findTypeCase(dataTypeCase).get
      if (haveChanged) {
        Some(domain)
      } else {
        // Domain where data type was defined is one option
        // But make sure there isn't an interface that was defined afterwards.
        // NOTE: LatestModelDefiningInterface is useful for existence of Exp.java but does not
        //       work when, for example, a merge of two branches makes an Exp but that merge doesn't
        //       have the dataTypeCase. A new DataTypeCase is added for each new operation, so
        //       STARTING from where type was defined, find all subsequent models with ops and take last one, or just where defined if none exist
        val updated = domain.inChronologicalOrder.filter(m => m.ops.nonEmpty && definedIn.before(m))
        if (updated.isEmpty) {
          domain.findTypeCase(dataTypeCase)
        } else {
          Some(updated.last)
        }
        //WORKED FOR MAIN but not for Producers Some(latestModelDefiningInterface(domain).later(definedIn))
        //domain.findTypeCase(dataTypeCase)
      }
    } else {
      None
    }

    if (_latestModelDefiningDataTypeCaseInterface.isEmpty) {
      Command.lift[Context, Option[paradigm.syntax.Type]](Option.empty)
    } else {
        for {
          dataTypeCaseInterface <- FindClass[paradigm.syntax.Name, paradigm.syntax.Type](Seq(names.mangle(names.instanceNameOf(_latestModelDefiningDataTypeCaseInterface.get)), names.mangle(names.conceptNameOf(dataTypeCase)))).interpret(canFindClass)
          _ <- resolveAndAddImport(dataTypeCaseInterface)
        } yield Some(dataTypeCaseInterface)
      }
  }

  def finalizedTypeCaseClass[Context](domain: GenericModel, dataTypeCase:DataTypeCase)(implicit
        canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
        canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
        canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]],
  ): Generator[Context, paradigm.syntax.Type] = {
    @tailrec
    def latestDeclaringTypeCase(model:GenericModel): GenericModel = {
      if (model.typeCases.contains(dataTypeCase)) {
        model
      } else {
        if (model == latestModelDefiningInterface(model)) {
          model
        } else {
          latestDeclaringTypeCase(model.former.head) //   can be sure there is only one, since merge is handled above
        }
      }
    }

    // either domain directly defines type case or you need latest one defining interface
    // latest one defining type case
    val _latestModelDefiningDataTypeCaseInterface = latestDeclaringTypeCase(domain)

    val optimized = domain.optimizations.exists(pair => pair._1 == dataTypeCase)   // at some point in the past for this tpe; MIGHT BE TOO BROAD

    val toUse= if (optimized) {
      domain
    } else {
      _latestModelDefiningDataTypeCaseInterface
    }
    for {
        dataTypeCaseInterface <- FindClass[paradigm.syntax.Name, paradigm.syntax.Type](Seq(names.mangle(names.instanceNameOf(toUse)), ComponentNames.finalizedPackage, names.mangle(names.conceptNameOf(dataTypeCase)))).interpret(canFindClass)
        _ <- resolveAndAddImport(dataTypeCaseInterface)
      } yield dataTypeCaseInterface
  }

  def setAttributeGetterSignature(domain: GenericModel, attribute: abstractions.Attribute): Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      returnType <-
        if (attribute.tpe.isModelBase(domain)) {
          mostSpecificBaseInterfaceType(domain)
        } else {
          toTargetLanguageType(attribute.tpe)
        }
      _ <- setReturnType(returnType)
    } yield ()
  }

  def makeOperationImplementation(
         domainSpecific: EvolutionImplementationProvider[this.type],
         domain: GenericModel,
         dataTypeCase: DataTypeCase,
         operation: Operation
       ): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      _ <- setOperationMethodSignature(domain, operation)
      _ <- if (domain.operationsPresentEarlier(dataTypeCase).contains(operation)) {
          setOverride()
        } else {
          Command.skip[paradigm.MethodBodyContext]
        }
      argumentsRaw <- getArguments()
      baseInterfaceType <- mostSpecificBaseInterfaceType(domain)
      arguments <- forEach(argumentsRaw.zip(operation.parameters)) { case (arg,param) =>
        if (param.tpe.isModelBase(domain)) {
          // dynamic case
          castObject(baseInterfaceType, arg._3)
        } else{
          Command.lift[paradigm.MethodBodyContext,paradigm.syntax.Expression](arg._3)
        }
      }
      self <- selfReference()
      attributes <- forEach(dataTypeCase.attributes) { attribute =>
        for {
          getterMethod <- getMember(self, ComponentNames.getter(attribute))
          getterCall <- apply(getterMethod, Seq.empty)
        } yield (attribute, getterCall)
      }
      receivedRequest =
        ReceivedRequest(
          onType = domain.baseDataType,
          tpeCase = dataTypeCase,
          selfReference = self,
          attributes = attributes.toMap,
          request = Request(
            op = operation,
            arguments = operation.parameters.zip(arguments).toMap
          ),
          Some(domain)
        )
      result <- domainSpecific.logic(this)(receivedRequest)
    } yield result
  }

  /** Map data type cases to operations that require a new implementation in the given domain model.
   * Will only contain data type cases which have been newly introduced in at least one of the ancestor branches
   * or require an update because of missing/overwritten operations or merging of multiple branches.
   */
  def dataTypeCasesWithNewOperations(domain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Map[DataTypeCase, Set[Operation]] = {
    val flatDomain = domain.flatten

    val allDataTypeCases = flatDomain.typeCases.toSet  // idea from coco
    val allOperations = flatDomain.ops.toSet

    allDataTypeCases.foldLeft(Map.empty[DataTypeCase, Set[Operation]]) { (resultMap, tpeCase) =>

      val presentOperations = domain.operationsPresentEarlier(tpeCase)    // idea from coco
      val overwrittenOperations = allOperations.filter { operation =>
        val potentials = domainSpecific.evolutionSpecificDependencies(PotentialRequest(domain.baseDataType, tpeCase, operation))
        potentials.contains(domain)
      }

      val producerOperations = allOperations.filter(op => op.isProducer(domain))

      // always want to insert (even if empty set) unlike in CoCoClean where more care is required. ALSO
      // producer operations substantially copy logic because Demanded by Trivially approach.
      val updatedOperations = (allOperations -- presentOperations) ++ overwrittenOperations ++ producerOperations
      resultMap.updated(tpeCase, updatedOperations)
    }
  }

  def makeNewTypeCaseInterface(domain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type], newDataTypeCase:DataTypeCase, newOperations:Set[Operation]) : Generator[ooParadigm.ClassContext, Unit] = {
    import ooParadigm.classCapabilities._

    for {
      // Set parent data type interface
      parentBaseInterfaceType <- mostSpecificBaseInterfaceType(domain)
      _ <- addParent(parentBaseInterfaceType)

      // Inherit previous data type case implementations
      _ <- forEach(domain.former) { ancestor =>
        for {
          parentDataTypeCaseInterface <- mostSpecificTypeCaseInterface(ancestor, newDataTypeCase, domainSpecific)
          _ <- if (parentDataTypeCaseInterface.nonEmpty) {
            addParent(parentDataTypeCaseInterface.get)
          } else {
            Command.skip[ooParadigm.ClassContext]
          }
        } yield()
      }

      // Add abstract getters if defined here
      _ <- forEach(newDataTypeCase.attributes) { attribute =>
        addAbstractMethod(ComponentNames.getter(attribute), setAttributeGetterSignature(domain, attribute))
      }

      _ <- forEach (newerTypeCasesSinceInterface(domain)) { tpeCase =>
        addTypeLookupForMethods(FinalizedDataTypeCase(tpeCase), finalizedTypeCaseClass(domain, tpeCase)(
          canFindClass = ooParadigm.methodBodyCapabilities.canFindClassInMethod,
          canResolveImport = paradigm.methodBodyCapabilities.canResolveImportInMethod,
          canAddImport = paradigm.methodBodyCapabilities.canAddImportInMethodBody
        ))
      }

      // Add methods for new operations
      // In Methods we use the least specific type (ep.Exp<FT>) to refer to the domain base type.
      _ <- addTypeLookupForMethods(TypeRep.DataType(domain.baseDataType), mostSpecificBaseInterfaceType(domain)(
        canFindClass = ooParadigm.methodBodyCapabilities.canFindClassInMethod,
        canResolveImport = paradigm.methodBodyCapabilities.canResolveImportInMethod,
        canAddImport = paradigm.methodBodyCapabilities.canAddImportInMethodBody
      ))

      _ <- forEach(newOperations.toList) { newOperation =>
        addMethod(
          names.mangle(names.instanceNameOf(newOperation)),
          makeOperationImplementation(
            domainSpecific = domainSpecific,
            domain = domain,
            dataTypeCase = newDataTypeCase,
            operation = newOperation
          )
        )
      }

      _ <- setInterface()
    } yield ()
  }

  def addDataTypeCaseInterfaces(domain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {

    import ooParadigm.projectCapabilities._
    for {
      _ <- forEach(dataTypeCasesWithNewOperations(domain, domainSpecific).toList) { case (dataTypeCase,newOperations) =>
        if (newOperations.nonEmpty) {
          addClassToProject(makeNewTypeCaseInterface(domain, domainSpecific, dataTypeCase, newOperations), names.mangle(names.instanceNameOf(domain)), names.mangle(names.conceptNameOf(dataTypeCase)))
        } else {
          Command.skip[ProjectContext]
        }
      }
    } yield ()
  }

  def makeAttributeGetter(domain: GenericModel, attribute: abstractions.Attribute): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import ooParadigm.methodBodyCapabilities._
    for {
      _ <- setAttributeGetterSignature(domain, attribute)
      self <- selfReference()
      result <- getMember(self, names.mangle(names.instanceNameOf(attribute)))
    } yield Some(result)
  }

  def makeFinalizedDataTypeCaseConstructor(domain: GenericModel, baseTypeInterface: paradigm.syntax.Type, dataTypeCase: DataTypeCase): Generator[ooParadigm.ConstructorContext, Unit] = {
    import ooParadigm.constructorCapabilities._
    for {
      parameters <- forEach(dataTypeCase.attributes) { attribute =>
        if (attribute.tpe.isModelBase(domain)) {
          for {
            name <- freshName(names.mangle(names.instanceNameOf(attribute)))
          } yield (name, baseTypeInterface)
        } else {
          for {
            parameterType <- toTargetLanguageType(attribute.tpe)
            _ <- resolveAndAddImport(parameterType)
            name <- freshName(names.mangle(names.instanceNameOf(attribute)))
          } yield (name, parameterType)
        }
      }
      _ <- setParameters(parameters)

      arguments <- getArguments()
      _ <- forEach(dataTypeCase.attributes.zip(arguments)) { case (attribute, argument) =>
        initializeField(names.mangle(names.instanceNameOf(attribute)), argument._3)
      }
    } yield ()
  }

  def addFinalizedTypeCaseClassesIfNecessary(domain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {

    def makeNewFinalizedTypeCaseClass(newDataTypeCase: DataTypeCase) : Generator[ooParadigm.ClassContext, Unit] = {
      import ooParadigm.classCapabilities._

      for {
        // Inherit non finalized data type case implementation
        nonFinalizedDataTypeCaseInterface <- mostSpecificTypeCaseInterface(domain, newDataTypeCase, domainSpecific)
        _ <- if (nonFinalizedDataTypeCaseInterface.isDefined) {
          addImplemented(nonFinalizedDataTypeCaseInterface.get)
        } else {
          Command.skip[ooParadigm.ClassContext]
        }

        // TODO: WILL need to add additional IMPLEMENTED if the data type case (from any ancestor branch) if that data type interface exists
        _ <- forEach(domain.former) { former =>
            val latest = mostSpecificModel(domain, newDataTypeCase, domainSpecific).getOrElse(former)  // fall back to former if not present
            val here = former.lastModelWithOperation.foldLeft(latest)((defined, model) => defined.later(model))
            for {
              // find where it was last defined (can only be one), and that's the one to import *OR* if a new operation was defined in between.

              formerInterface <- mostSpecificTypeCaseInterface(here, newDataTypeCase, domainSpecific)
              _ <- if (formerInterface.isDefined) {
                addImplemented(formerInterface.get)
              } else {
                Command.skip[ooParadigm.ClassContext]
              }
            } yield ()
          }

        // Add fields and their getters
        baseTypeInterface <- mostSpecificBaseInterfaceType(domain)
        _ <- forEach(newDataTypeCase.attributes) { attribute =>
          for {
            attributeType <- if (!attribute.tpe.isModelBase(domain)) {
              toTargetLanguageType(attribute.tpe)
            } else Command.lift[ooParadigm.ClassContext, paradigm.syntax.Type](baseTypeInterface)
            _ <- addField(names.mangle(names.instanceNameOf(attribute)), attributeType)
            _ <- addMethod(ComponentNames.getter(attribute), makeAttributeGetter(domain, attribute))
          } yield ()
        }

        // Add constructor
        _ <- addConstructor(makeFinalizedDataTypeCaseConstructor(domain, baseTypeInterface, newDataTypeCase))
      } yield ()
    }

    // only chose those with new operations, even those which have had overwritten implementations of existing operations
    val haveChanged = dataTypeCasesWithNewOperations(domain, domainSpecific).filter(pair => pair._2.nonEmpty).keys.toSeq

    import ooParadigm.projectCapabilities._
    for {
      _ <- forEach(haveChanged) { tpeCase =>
        addClassToProject(makeNewFinalizedTypeCaseClass(tpeCase), names.mangle(names.instanceNameOf(domain)), ComponentNames.finalizedPackage, names.mangle(names.conceptNameOf(tpeCase)))
      }
    } yield ()
  }

  def implement(domain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    def implementRecursive(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {

      for {
        _ <- registerTypeMapping(domain, domainSpecific)                         // handle DataType classes as well for interpreter
        _ <- if (domain == latestModelDefiningInterface(domain)) {
          for {
            _ <- addNewTypeInterface(domain)                                     // Exp for each evolution that needs one
          } yield ()
        } else {
          Command.skip[paradigm.ProjectContext]
        }

        _ <- addDataTypeCaseInterfaces(domain, domainSpecific)                   // DataTypeCase interfaces as needed
        _ <- addFinalizedTypeCaseClassesIfNecessary(domain, domainSpecific)      // Finalized classes

        _ <- forEach(domain.former.filterNot(p => p.isDomainBase)) { ancestor => implementRecursive(ancestor) }
      } yield ()
    }

    for {
      _ <- domainSpecific.initialize(this)
      _ <- implementRecursive(domain)
    } yield ()
  }

  def registerTypeMapping(domain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    import ooParadigm.classCapabilities.canFindClassInClass
    import ooParadigm.constructorCapabilities.canFindClassInConstructor
    import ooParadigm.methodBodyCapabilities.canFindClassInMethod
    import paradigm.projectCapabilities._
    import org.combinators.ep.generator.Understands
    import org.combinators.ep.generator.paradigm.FindClass

    // all type cases that were defined AFTER last Exp need to be registered
    val _latest = latestModelDefiningInterface(domain)
    val flat = domain.flatten
    val seqs = flat.typeCases.map(tpe => {
      val haveChanged = dataTypeCasesWithNewOperations(domain, domainSpecific).exists(pair => pair._1 == tpe && pair._2.nonEmpty)
      val optimized = flat.optimizations.exists(pair => pair._1 == tpe)   // at some point in the past for this tpe; MIGHT BE TOO BROAD

      if (haveChanged || optimized) {
        (tpe, domain.later(_latest))   // always be sure to get latest
      } else {
        (tpe, domain.findTypeCase(tpe).get.later(_latest)) // either find where it was defined OR a later interface
      }
    })

    val dtpeRep = TypeRep.DataType(domain.baseDataType)
    for {
       _ <- forEach (seqs) { case (tpeCase, model) =>   // passes on capabilities so it knows which generators to use...
         for {
          _ <- addTypeLookupForMethods(FinalizedDataTypeCase(tpeCase), finalizedTypeCaseClass(model, tpeCase)(canFindClass = ooParadigm.methodBodyCapabilities.canFindClassInMethod, canAddImport = paradigm.methodBodyCapabilities.canAddImportInMethodBody, canResolveImport = paradigm.methodBodyCapabilities.canResolveImportInMethod))
          _ <- addTypeLookupForClasses(FinalizedDataTypeCase(tpeCase), finalizedTypeCaseClass(model, tpeCase)(canFindClass = ooParadigm.classCapabilities.canFindClassInClass, canAddImport = ooParadigm.classCapabilities.canAddImportInClass, canResolveImport = ooParadigm.classCapabilities.canResolveImportInClass))
          _ <- addTypeLookupForConstructors(FinalizedDataTypeCase(tpeCase), finalizedTypeCaseClass(model, tpeCase)(canFindClass = ooParadigm.constructorCapabilities.canFindClassInConstructor, canAddImport = ooParadigm.constructorCapabilities.canAddImportInConstructor, canResolveImport = ooParadigm.constructorCapabilities.canResolveImportInConstructor))
        } yield ()
      }

      _ <- addTypeLookupForMethods(dtpeRep, mostSpecificBaseInterfaceType(domain)(canFindClass=ooParadigm.methodBodyCapabilities.canFindClassInMethod, canAddImport =paradigm.methodBodyCapabilities.canAddImportInMethodBody, canResolveImport = paradigm.methodBodyCapabilities.canResolveImportInMethod))
      _ <- addTypeLookupForClasses(dtpeRep, mostSpecificBaseInterfaceType(domain)(canFindClass=ooParadigm.classCapabilities.canFindClassInClass, canAddImport = ooParadigm.classCapabilities.canAddImportInClass, canResolveImport = ooParadigm.classCapabilities.canResolveImportInClass))
      _ <- addTypeLookupForConstructors(dtpeRep, mostSpecificBaseInterfaceType(domain)(canFindClass=ooParadigm.constructorCapabilities.canFindClassInConstructor, canAddImport = ooParadigm.constructorCapabilities.canAddImportInConstructor, canResolveImport = ooParadigm.constructorCapabilities.canResolveImportInConstructor))
    } yield ()
  }

  def registerTypeMapping(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    import ooParadigm.classCapabilities.canFindClassInClass
    import ooParadigm.constructorCapabilities.canFindClassInConstructor
    import ooParadigm.methodBodyCapabilities.canFindClassInMethod
    import paradigm.projectCapabilities._
    import org.combinators.ep.generator.Understands
    import org.combinators.ep.generator.paradigm.FindClass

    // all type cases that were defined AFTER last Exp need to be registered
    val _latest = latestModelDefiningInterface(domain)
    val flat = domain.flatten
    val seqs = flat.typeCases.map(tpe => {
      val optimized = flat.optimizations.exists(pair => pair._1 == tpe)   // at some point in the past for this tpe; MIGHT BE TOO BROAD

      if (optimized) {
        (tpe, domain.later(_latest))   // always be sure to get latest
      } else {
        (tpe, domain.findTypeCase(tpe).get.later(_latest)) // either find where it was defined OR a later interface
      }
    })

    val dtpeRep = TypeRep.DataType(domain.baseDataType)
    for {
      _ <- forEach (seqs) { case (tpeCase, model) =>   // passes on capabilities so it knows which generators to use...
        for {
          _ <- addTypeLookupForMethods(FinalizedDataTypeCase(tpeCase), finalizedTypeCaseClass(model, tpeCase)(canFindClass = ooParadigm.methodBodyCapabilities.canFindClassInMethod, canAddImport = paradigm.methodBodyCapabilities.canAddImportInMethodBody, canResolveImport = paradigm.methodBodyCapabilities.canResolveImportInMethod))
          _ <- addTypeLookupForClasses(FinalizedDataTypeCase(tpeCase), finalizedTypeCaseClass(model, tpeCase)(canFindClass = ooParadigm.classCapabilities.canFindClassInClass, canAddImport = ooParadigm.classCapabilities.canAddImportInClass, canResolveImport = ooParadigm.classCapabilities.canResolveImportInClass))
          _ <- addTypeLookupForConstructors(FinalizedDataTypeCase(tpeCase), finalizedTypeCaseClass(model, tpeCase)(canFindClass = ooParadigm.constructorCapabilities.canFindClassInConstructor, canAddImport = ooParadigm.constructorCapabilities.canAddImportInConstructor, canResolveImport = ooParadigm.constructorCapabilities.canResolveImportInConstructor))
        } yield ()
      }

      _ <- addTypeLookupForMethods(dtpeRep, mostSpecificBaseInterfaceType(domain)(canFindClass=ooParadigm.methodBodyCapabilities.canFindClassInMethod, canAddImport =paradigm.methodBodyCapabilities.canAddImportInMethodBody, canResolveImport = paradigm.methodBodyCapabilities.canResolveImportInMethod))
      _ <- addTypeLookupForClasses(dtpeRep, mostSpecificBaseInterfaceType(domain)(canFindClass=ooParadigm.classCapabilities.canFindClassInClass, canAddImport = ooParadigm.classCapabilities.canAddImportInClass, canResolveImport = ooParadigm.classCapabilities.canResolveImportInClass))
      _ <- addTypeLookupForConstructors(dtpeRep, mostSpecificBaseInterfaceType(domain)(canFindClass=ooParadigm.constructorCapabilities.canFindClassInConstructor, canAddImport = ooParadigm.constructorCapabilities.canAddImportInConstructor, canResolveImport = ooParadigm.constructorCapabilities.canResolveImportInConstructor))
    } yield ()
  }

  override def implement(tests: Map[GenericModel, Seq[TestCase]], testImplementationProvider: TestImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    import paradigm.projectCapabilities._
    import paradigm.compilationUnitCapabilities._
    import paradigm.testCapabilities._

    for {
      _ <- forEach(tests.toList) { case (model, itests) =>
        val testCode: Generator[paradigm.MethodBodyContext, Seq[paradigm.syntax.Expression]] =
          for {
            code <- forEach(itests) {
              test => testImplementationProvider.test(this)(test)
            }
          } yield code.flatten

        val compUnit = addTestCase(testCode, testName)
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
    } yield ()
  }
}

object TriviallyClean {
  type WithParadigm[P <: AnyParadigm] = TriviallyClean { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   oo: ObjectOriented.WithBase[base.type]) : TriviallyClean.WithParadigm[base.type] =
    new TriviallyClean {
      val paradigm: base.type = base
      val names: NameProvider[paradigm.syntax.Name] = nameProvider
      val ooParadigm: oo.type = oo
    }
}
