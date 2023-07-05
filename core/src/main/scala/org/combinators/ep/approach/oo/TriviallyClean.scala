package org.combinators.ep.approach.oo     /*DI:LI:AD*/

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.{GenericModel, abstractions}
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._
import org.combinators.ep.generator.paradigm._

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
   * @param baseTpe
   * @param tpeCase
   * @param args
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

  /** Find the last evolution that requires its own Exp definition. */
  def latestModelDefiningInterface(domain: GenericModel): GenericModel = {
    if (domain.isDomainBase || domain.ops.nonEmpty || domain.former.length > 1) {   // handle merge case as well
      domain
    } else {
      // find where tpe was defined and also where last operation was defined and choose later of the two
      // will only have one, since merge case handled above.
      // could be one of our ancestors is a merge point
      latestModelDefiningInterface(domain.former.head)
    }
  }

  def newerTypeCasesSinceInterface(domain:GenericModel) : Seq[DataTypeCase] = {
    val lastExp = latestModelDefiningInterface(domain)
    val toRemove = lastExp.flatten.typeCases

    // will have to allow those IN the interface extension as well
    domain.flatten.typeCases.filterNot(tpe => toRemove.contains(tpe)).seq
  }

  def newerTypeCasesIncludingInterface(domain:GenericModel) : Seq[DataTypeCase] = {
    if (domain == latestModelDefiningInterface(domain)) {
      domain.flatten.typeCases
    } else {
      domain.typeCases
    }
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
      for {

        // Set parent type interfaces (may have multiple with merging)
        _ <- forEach(domain.former.filterNot(p => p.isDomainBase)) { ancestor =>
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

  def mostSpecificTypeCaseInterface[Context](domain: GenericModel, dataTypeCase:DataTypeCase)(implicit
         canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
         canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
         canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]],
  ): Generator[Context, Option[paradigm.syntax.Type]] = { // this should be if DEFINED here...

    val _latestModelDefiningDataTypeCaseInterface = if (domain.findTypeCase(dataTypeCase).isDefined) {
      Some(latestModelDefiningInterface(domain).later(domain.findTypeCase(dataTypeCase).get))
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

    for {
        dataTypeCaseInterface <- FindClass[paradigm.syntax.Name, paradigm.syntax.Type](Seq(names.mangle(names.instanceNameOf(_latestModelDefiningDataTypeCaseInterface)), ComponentNames.finalizedPackage, names.mangle(names.conceptNameOf(dataTypeCase)))).interpret(canFindClass)
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
         evolutionImplementationProvider: EvolutionImplementationProvider[this.type],
         domain: GenericModel,
         dataTypeCase: DataTypeCase,
         operation: Operation
       ): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      _ <- setOperationMethodSignature(domain, operation)
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
        )
      )
      result <- evolutionImplementationProvider.logic(this)(receivedRequest)
    } yield result
  }

  /** Map data type cases to operations that require a new implementation in the given domain model.
   * Will only contain data type cases which have been newly introduced in at least one of the ancestor branches
   * or require an update because of missing/overwritten operations or merging of multiple branches.
   */
  def dataTypeCasesWithNewOperations(evolutionImplementationProvider: EvolutionImplementationProvider[this.type], domain: GenericModel): Map[DataTypeCase, Set[Operation]] = {
    val flatDomain = domain.flatten
    val allDataTypeCases = if (domain == latestModelDefiningInterface(domain)) {
      flatDomain.typeCases.toSet
    } else {
      domain.typeCases
    }
    val allOperations = flatDomain.ops.toSet

    allDataTypeCases.foldLeft(Map.empty[DataTypeCase, Set[Operation]]) { (resultMap, tpeCase) =>
      val presentOperations = domain.former.flatMap(ancestor => {
        if (ancestor.supports(tpeCase)) {
          ancestor.flatten.ops.toSet
        } else {
          Set.empty[Operation]
        }
      })

      val overwrittenOperations = allOperations.filter { operation =>
        val lastOverwritingDomain =
          evolutionImplementationProvider.applicableIn(
            forApproach = this,
            potentialRequest = PotentialRequest(domain.baseDataType, tpeCase, operation),
            currentModel = domain
          )
        lastOverwritingDomain.contains(domain)
      }

      val producerOperations = allOperations.filter(op => op.isProducer(domain))

      // always want to insert (even if empty set) unlike in CoCoClean where more care is required. ALSO
      // producer operations substantially copy logic because Demanded by Trivially approach.
      val updatedOperations = (allOperations -- presentOperations) ++ overwrittenOperations ++ producerOperations
      resultMap.updated(tpeCase, updatedOperations)
    }
  }

  def addDataTypeCaseInterfaces(evolutionImplementationProvider: EvolutionImplementationProvider[this.type], domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {

    def makeNewTypeCaseInterface(newDataTypeCase:DataTypeCase, newOperations:Set[Operation]) : Generator[ooParadigm.ClassContext, Unit] = {
      import ooParadigm.classCapabilities._

      for {
        // Set parent data type interface
        parentBaseInterfaceType <- mostSpecificBaseInterfaceType(domain)
        _ <- addParent(parentBaseInterfaceType)

        // Inherit previous data type case implementations
        _ <- forEach(domain.former) { ancestor =>
          for {
            parentDataTypeCaseInterface <- mostSpecificTypeCaseInterface(ancestor, newDataTypeCase)
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

        _ <- forEach (newerTypeCasesSinceInterface(domain)) { tpeCase =>   // was domain.flatten.typeCases
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
              evolutionImplementationProvider = evolutionImplementationProvider,
              domain = domain,
              dataTypeCase = newDataTypeCase,
              operation = newOperation
            )
          )
        }

        _ <- setInterface()
      } yield ()
    }

    import ooParadigm.projectCapabilities._
    for {
      _ <- forEach(dataTypeCasesWithNewOperations(evolutionImplementationProvider, domain).toList) { case (dataTypeCase,newOperations) =>
        addClassToProject(makeNewTypeCaseInterface(dataTypeCase, newOperations), names.mangle(names.instanceNameOf(domain)), names.mangle(names.conceptNameOf(dataTypeCase)))
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

  def addFinalizedTypeCaseClasses(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {

    def makeNewFinalizedTypeCaseClass(newDataTypeCase: DataTypeCase) : Generator[ooParadigm.ClassContext, Unit] = {
      import ooParadigm.classCapabilities._

      for {
        // Inherit non finalized data type case implementation
        nonFinalizedDataTypeCaseInterface <- mostSpecificTypeCaseInterface(domain, newDataTypeCase)
        _ <- if (nonFinalizedDataTypeCaseInterface.isDefined) {
          addImplemented(nonFinalizedDataTypeCaseInterface.get)
        } else {
          Command.skip[ooParadigm.ClassContext]
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

    import ooParadigm.projectCapabilities._
    for {
      _ <- forEach(newerTypeCasesIncludingInterface(domain)) { tpeCase =>   // was domain.flatten.typeCases
        addClassToProject(makeNewFinalizedTypeCaseClass(tpeCase), names.mangle(names.instanceNameOf(domain)), ComponentNames.finalizedPackage, names.mangle(names.conceptNameOf(tpeCase)))
      }
    } yield ()
  }

  def implement(domain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    def implementRecursive(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {

      for {
        _ <- registerTypeMapping(domain)                              // handle DataType classes as well for interpreter
        _ <- if (domain == latestModelDefiningInterface(domain)) {
          for {
            _ <- addNewTypeInterface(domain)                          // Exp for each evolution that needs one
          } yield ()
        } else {
          Command.skip[paradigm.ProjectContext]
        }

        _ <- addDataTypeCaseInterfaces(domainSpecific, domain)        // DataTypeCase interfaces as needed
        _ <- addFinalizedTypeCaseClasses(domain)                      // Finalized classes

        _ <- forEach(domain.former.filterNot(p => p.isDomainBase)) { ancestor => implementRecursive(ancestor) }
      } yield ()

    }
    for {
      _ <- domainSpecific.initialize(this)
      _ <- implementRecursive(domain)
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
    val seqs = domain.flatten.typeCases.map(tpe => {
      (tpe, domain.findTypeCase(tpe).get.later(_latest))   // either find where it was defined OR a later interface
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
      _ <- forEach(tests.toList) { case (model, tests) => {
        val testCode: Generator[paradigm.MethodBodyContext, Seq[paradigm.syntax.Expression]] =
          for {
            code <- forEach(tests) {
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
