package org.combinators.ep.approach.oo    /*DI:LI:AD*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation, TestCase, TypeRep}
import org.combinators.ep.domain.{GenericModel, abstractions}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm._
import org.combinators.ep.generator._

// j7 mistakenly generates lit/multBy and lit/powBy
// might be in newDataTypeCasesWithNewOperations...
trait ObjectAlgebras extends ApproachImplementationProvider {
  val paradigm: AnyParadigm
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]
  val names: NameProvider[paradigm.syntax.Name]

  object ComponentNames {
    val algebra = names.mangle("Algebra")
    val algebraAtt = names.mangle("algebra")

    val signature = names.mangle("Signature")
    val carrier = names.mangle("Carrier")
    val returnTypeParameter = names.mangle("C")

    val pkgAlgebra = names.mangle("algebra")
    val pkgCarrier = names.mangle("carrier")
    val pkgInstance = names.mangle("instance")

    val getSelf = names.mangle("getSelf")

    val value = names.mangle("value")

    def constructor(tpeCase: abstractions.DataTypeCase): paradigm.syntax.Name = {
      names.mangle(names.instanceNameOf(tpeCase))
    }
  }

  def leastSpecialBaseInterfaceType[Context](domain: GenericModel, finalizedType: paradigm.syntax.Type)(implicit
                                                                                                        canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
                                                                                                        canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
                                                                                                        canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]],
                                                                                                        canApplyType: Understands[Context, Apply[paradigm.syntax.Type, paradigm.syntax.Type, paradigm.syntax.Type]],
  ): Generator[Context, paradigm.syntax.Type] = {
    for {
      baseInterfaceType <- FindClass[paradigm.syntax.Name, paradigm.syntax.Type](Seq(names.mangle(names.conceptNameOf(domain.baseDataType)))).interpret(canFindClass)
      _ <- resolveAndAddImport(baseInterfaceType)
      resultType <- Apply[paradigm.syntax.Type, paradigm.syntax.Type, paradigm.syntax.Type](baseInterfaceType, Seq(finalizedType)).interpret(canApplyType)
    } yield resultType
  }

  // Critical aspect of CoCo is that the Extended Intermediate Interface (i.e., ep.m3.Exp) is only created when
  // needed, specifically: (a) a new operation is being defined, and this interface will host the default
  // implementation; or (b) a branch is being merged from branches in which new Exp had been defined
  // useful when determining merging
  def ancestorsDefiningNewTypeInterfaces(domain: GenericModel): Set[GenericModel] = {
    val ancestorsWithNewTypeInterfaces = domain.former.map(ancestor => latestModelDefiningNewTypeInterface(ancestor))
    ancestorsWithNewTypeInterfaces.distinct.filterNot { ancestor =>
      // get rid of everything that has an antecedent
      ancestorsWithNewTypeInterfaces.exists(otherAncestor => ancestor.before(otherAncestor))
    }.toSet
  }

  def latestModelDefiningNewTypeInterface(domain: GenericModel): GenericModel = {
    if (domain.isDomainBase || domain.ops.nonEmpty) {
      domain
    } else {
      // is there a single type that can represent the "least upper bound" of all prior branches.
      val ancestorsWithTypeInterfaces = domain.former.map(ancestor => latestModelDefiningNewTypeInterface(ancestor)).distinct
      // To validate this works, need multiple branches where NEITHER defines operators
      if (ancestorsWithTypeInterfaces.size == 1 && !ancestorsWithTypeInterfaces.head.isDomainBase) { // take care to avoid falling below "floor"
        ancestorsWithTypeInterfaces.head
      } else {
        domain // we have to do merge
      }
    }
  }

  def mostSpecificBaseInterfaceType[Context](domain: GenericModel, finalizedType: paradigm.syntax.Type)(implicit
                                                                                                        canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
                                                                                                        canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
                                                                                                        canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]],
                                                                                                        canApplyType: Understands[Context, Apply[paradigm.syntax.Type, paradigm.syntax.Type, paradigm.syntax.Type]],
  ): Generator[Context, paradigm.syntax.Type] = {

    val resultModelStage = latestModelDefiningNewTypeInterface(domain)

    for {
      baseInterfaceType <-
        if (resultModelStage.isDomainBase) {
          FindClass[paradigm.syntax.Name, paradigm.syntax.Type](Seq(names.mangle(names.conceptNameOf(domain.baseDataType)))).interpret(canFindClass)
        } else {
          FindClass[paradigm.syntax.Name, paradigm.syntax.Type](Seq(names.mangle(names.instanceNameOf(resultModelStage)), names.mangle(names.conceptNameOf(domain.baseDataType)))).interpret(canFindClass)
        }
      _ <- resolveAndAddImport(baseInterfaceType)
      resultType <- Apply[paradigm.syntax.Type, paradigm.syntax.Type, paradigm.syntax.Type](baseInterfaceType, Seq(finalizedType)).interpret(canApplyType)
    } yield resultType
  }


  /*
  def addBasicFactoryInterface(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
    def makeBasicFactoryInterface(): Generator[ooParadigm.ClassContext, Unit] = {
      import genericsParadigm.classCapabilities._
      import ooParadigm.classCapabilities._
      for {
        _ <- addTypeParameter(ComponentNames.finalizedTypeParameter, Command.skip)
        finalizedType <- getTypeArguments().map(tpeArgs => tpeArgs.head)
        _ <- ooParadigm.classCapabilities.addAbstractMethod(ComponentNames.convertMethod, setConvertMethodSignature(domain, finalizedType))
        _ <- setInterface()
      } yield ()
    }

    import ooParadigm.projectCapabilities._
    addClassToProject(makeBasicFactoryInterface(), ComponentNames.factory)
  }

  def appliedFactoryInterfaceType(domain: GenericModel, finalizedType: paradigm.syntax.Type): Generator[ooParadigm.ClassContext, paradigm.syntax.Type] = {
    import genericsParadigm.classCapabilities._
    import ooParadigm.classCapabilities._
    for {
      factoryInterface <-
        if (domain.isDomainBase) {
          findClass(ComponentNames.factory)
        } else {
          findClass(names.mangle(names.instanceNameOf(domain)), ComponentNames.factory)
        }

      _ <- resolveAndAddImport(factoryInterface)
      resultType <- applyType(factoryInterface, Seq(finalizedType))
    } yield resultType
  }

  def setGetSelfMethodSignature(finalizedType: paradigm.syntax.Type): Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    for {
      _ <- setReturnType(finalizedType)
      _ <- setParameters(Seq())
    } yield ()
  }

  def addBasicTypeInterface(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
    def makeBasicTypeInterface(): Generator[ooParadigm.ClassContext, Unit] = {
      import genericsParadigm.classCapabilities._
      import ooParadigm.classCapabilities._
      for {
        _ <- addTypeParameter(ComponentNames.finalizedTypeParameter, Command.skip)
        finalizedType <- getTypeArguments().map(tpeArgs => tpeArgs.head)
        baseFactoryInterface <- appliedFactoryInterfaceType(domain.base, finalizedType)
        _ <- addParent(baseFactoryInterface)
        _ <- addAbstractMethod(ComponentNames.getSelfMethod, setGetSelfMethodSignature(finalizedType))
        _ <- setInterface()
      } yield ()
    }

    import ooParadigm.projectCapabilities._
    addClassToProject(makeBasicTypeInterface(), names.mangle(names.conceptNameOf(domain.baseDataType)))
  }

  def setFactoryMethodSignature(domain: GenericModel, finalizedType: paradigm.syntax.Type, tpeCase: DataTypeCase): Generator[paradigm.MethodBodyContext, Unit] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._
    for {
      typeInterface <- leastSpecialBaseInterfaceType(domain, finalizedType)
      parameters <- forEach(tpeCase.attributes) { attribute =>
        if (attribute.tpe.isModelBase(domain)) {
          Command.lift[paradigm.MethodBodyContext, (paradigm.syntax.Name, paradigm.syntax.Type)]((names.mangle(names.instanceNameOf(attribute)), typeInterface))
        } else {
          for {
            parameterType <- toTargetLanguageType(attribute.tpe)
            _ <- resolveAndAddImport(parameterType)
          } yield (names.mangle(names.instanceNameOf(attribute)), parameterType)
        }
      }
      _ <- setParameters(parameters)
      _ <- setReturnType(typeInterface)
    } yield ()
  }

  def addNewFactoryInterface(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
    def makeNewFactoryInterface(): Generator[ooParadigm.ClassContext, Unit] = {
      import genericsParadigm.classCapabilities._
      import ooParadigm.classCapabilities._
      for {
        // Add type parameter for the finalized type
        _ <- addTypeParameter(ComponentNames.finalizedTypeParameter, Command.skip)
        finalizedType <- getTypeArguments().map(tpeArgs => tpeArgs.head)

        // Set parent factory types
        _ <- forEach(domain.former) { ancestor =>
          for {
            parentFactoryType <- appliedFactoryInterfaceType(ancestor, finalizedType)
            _ <- addParent(parentFactoryType)
          } yield ()
        }

        // Add factory methods
        _ <- forEach(domain.typeCases) { tpeCase =>
          addAbstractMethod(names.mangle(names.instanceNameOf(tpeCase)), setFactoryMethodSignature(domain, finalizedType, tpeCase))
        }

        // Add convert method
        _ <- addAbstractMethod(ComponentNames.convertMethod, setConvertMethodSignature(domain, finalizedType))

        _ <- setInterface()
      } yield ()
    }

    import ooParadigm.projectCapabilities._
    addClassToProject(makeNewFactoryInterface(), names.mangle(names.instanceNameOf(domain)), ComponentNames.factory)
  }

  def setOperationMethodSignature(domain: GenericModel, finalizedType: paradigm.syntax.Type, operation: Operation): Generator[paradigm.MethodBodyContext, Unit] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._
    for {
      typeInterface <- leastSpecialBaseInterfaceType(domain, finalizedType)
      // Translate result and parameter types
      signature <- forEach(operation.returnType +: operation.parameters.map(_.tpe)) { signatureTypeRep =>
        if (signatureTypeRep.isModelBase(domain)) {
          Command.lift[paradigm.MethodBodyContext, paradigm.syntax.Type](typeInterface)
        } else {
          for {
            signatureType <- toTargetLanguageType(signatureTypeRep)
            _ <- resolveAndAddImport(signatureType)
          } yield signatureType
        }
      }
      _ <- setParameters(operation.parameters.map(param => names.mangle(param.name)).zip(signature.tail))
      _ <- setReturnType(signature.head)
    } yield ()
  }

  def addNewTypeInterfaceIfNecessary(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
    def makeNewTypeInterface(): Generator[ooParadigm.ClassContext, Unit] = {
      import genericsParadigm.classCapabilities._
      import ooParadigm.classCapabilities._
      for {
        // Add type parameter for the finalized type
        _ <- addTypeParameter(ComponentNames.finalizedTypeParameter, Command.skip)
        finalizedType <- getTypeArguments().map(tpeArgs => tpeArgs.head)

        // Set parent factory type
        parentFactoryType <- appliedFactoryInterfaceType(domain, finalizedType)
        _ <- addParent(parentFactoryType)

        // Set parent type interfaces
        _ <- forEach(ancestorsDefiningNewTypeInterfaces(domain).toList) { ancestor =>
          for {
            parentTypeInterface <- mostSpecificBaseInterfaceType(ancestor, finalizedType)
            _ <- addParent(parentTypeInterface)
          } yield ()
        }

        // Add methods for new operations
        _ <- forEach(domain.ops) { operation =>
          ooParadigm.classCapabilities.addAbstractMethod(
            names.mangle(names.instanceNameOf(operation)),
            setOperationMethodSignature(domain, finalizedType, operation)
          )
        }

        _ <- setInterface()
      } yield ()
    }

    if (domain != latestModelDefiningNewTypeInterface(domain)) {
      Command.skip
    } else {
      import ooParadigm.projectCapabilities._
      addClassToProject(makeNewTypeInterface(), names.mangle(names.instanceNameOf(domain)), names.mangle(names.conceptNameOf(domain.baseDataType)))
    }
  }

  /** Map data type cases to operations that require a new implementation in the given domain model.
    * Will only contain data type cases which have been newly introduced in at least one of the ancestor branches
    * or require an update because of missing/overwritten operations or merging of multiple branches.
    */
  def newDataTypeCasesWithNewOperations(evolutionImplementationProvider: EvolutionImplementationProvider[this.type], domain: GenericModel): Map[DataTypeCase, Set[Operation]] = {
    val flatDomain = domain.flatten
    val allDataTypeCases = flatDomain.typeCases.toSet
    val allOperations = flatDomain.ops.toSet

    allDataTypeCases.foldLeft(Map.empty[DataTypeCase, Set[Operation]]) { (resultMap, tpeCase) =>
      // Remembers all operations that are already supported
      val presentOperations = domain.former.flatMap(ancestor => {
        if (ancestor.supports(tpeCase)) {
          ancestor.flatten.ops.toSet
        } else {
          Set.empty[Operation]
        }
      })

      val overwrittenOperations = allOperations.filter { operation =>
        // Are we applicable based on EIP? Tells us in which domain EIP is applicable
        val lastOverwritingDomain =
          evolutionImplementationProvider.applicableIn(
            forApproach = this,
            potentialRequest = PotentialRequest(domain.baseDataType, tpeCase, operation),
            currentModel = domain
          )
        lastOverwritingDomain.contains(domain)
      }
      val updatedOperations = (allOperations -- presentOperations) ++ overwrittenOperations
      // If we have any updated operations, if we have a former one that doesn't support the current type case, or if we are in a merge.
      if (updatedOperations.nonEmpty || domain.former.exists(ancestor => !ancestor.supports(tpeCase)) || domain.former.size > 1) {
        resultMap.updated(tpeCase, updatedOperations)
      } else {
        resultMap
      }
    }
  }

  def latestModelDefiningDataTypeCaseInterface(domain: GenericModel, dataTypeCase: DataTypeCase): Option[GenericModel] = {
    // Either this is the current domain (new Exp interface, then we know dataTypeCase is being redeclared anyway because of new operation); or
    // it is a prior domain (using a former Exp interface, we are currently freshly declaring the data type case interface either because our
    // branch has never seen this data type case OR if it has seen it, since ancestor had added data type and we need to find latest point where
    // it was seen)
    val _latestModelDefiningNewTypeInterface = latestModelDefiningNewTypeInterface(domain)
    val modelDeclaringTypeCase = domain.findTypeCase(dataTypeCase)
    if (modelDeclaringTypeCase.isEmpty) {
      // wasn't declared at all, so it can't be found
      Option.empty
    } else if (_latestModelDefiningNewTypeInterface.before(modelDeclaringTypeCase.get)) {
      // type case had been declared on a model between us and this one, so take intervening one
      modelDeclaringTypeCase
    } else {
      Some(_latestModelDefiningNewTypeInterface)
    }
  }

  def mostSpecificTypeCaseInterface[Context](domain: GenericModel, finalizedType: paradigm.syntax.Type, dataTypeCase:DataTypeCase)(implicit
          canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
          canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
          canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]],
          canApplyType: Understands[Context, Apply[paradigm.syntax.Type, paradigm.syntax.Type, paradigm.syntax.Type]],
  ): Generator[Context, Option[paradigm.syntax.Type]] = {
    val _latestModelDefiningDataTypeCaseInterface = latestModelDefiningDataTypeCaseInterface(domain, dataTypeCase)

    if (_latestModelDefiningDataTypeCaseInterface.isEmpty) {
      Command.lift[Context, Option[paradigm.syntax.Type]](Option.empty)
    } else {
      for {
        dataTypeCaseInterface <- FindClass[paradigm.syntax.Name, paradigm.syntax.Type](Seq(names.mangle(names.instanceNameOf(_latestModelDefiningDataTypeCaseInterface.get)), names.mangle(names.conceptNameOf(dataTypeCase)))).interpret(canFindClass)
        _ <- resolveAndAddImport(dataTypeCaseInterface)
        resultType <- Apply[paradigm.syntax.Type, paradigm.syntax.Type, paradigm.syntax.Type](dataTypeCaseInterface, Seq(finalizedType)).interpret(canApplyType)
      } yield Some(resultType)
    }
  }

  def setAttributeGetterSignature(domain: GenericModel, finalizedType: paradigm.syntax.Type, attribute: abstractions.Attribute): Generator[paradigm.MethodBodyContext, Unit] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._
    for {
      returnType <-
        if (attribute.tpe.isModelBase(domain)) {
          leastSpecialBaseInterfaceType(domain, finalizedType)
        } else {
          toTargetLanguageType(attribute.tpe)
        }
      _ <- setReturnType(returnType)
    } yield ()
  }

  def makeOperationImplementation(
      evolutionImplementationProvider: EvolutionImplementationProvider[this.type],
      domain: GenericModel,
      finalizedType: paradigm.syntax.Type,
      dataTypeCase: DataTypeCase,
      operation: Operation
    ): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      _ <- setOperationMethodSignature(domain, finalizedType, operation)
      arguments <- getArguments()
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
            arguments = operation.parameters.zip(arguments.map(argument => argument._3)).toMap
          )
        )
      result <- evolutionImplementationProvider.logic(this)(receivedRequest)
    } yield result
  }

  def addDataTypeCaseInterfaces(evolutionImplementationProvider: EvolutionImplementationProvider[this.type], domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
    val _newDataTypeCasesWithNewOperations = newDataTypeCasesWithNewOperations(evolutionImplementationProvider, domain)

    def makeNewTypeCaseInterface(newDataTypeCase:DataTypeCase, newOperations:Set[Operation]) : Generator[ooParadigm.ClassContext, Unit] = {
      import genericsParadigm.classCapabilities._
      import ooParadigm.classCapabilities._

      for {
        // Add type parameter for the finalized type
        _ <- addTypeParameter(ComponentNames.finalizedTypeParameter, Command.skip)
        finalizedType <- getTypeArguments().map(tpeArgs => tpeArgs.head)

        // Set parent data type interface
        parentBaseInterfaceType <- mostSpecificBaseInterfaceType(domain, finalizedType)
        _ <- addParent(parentBaseInterfaceType)

        // Also inherit from latest factory if necessary because no new base interface was declared
        _ <- if (latestModelDefiningNewTypeInterface(domain) != domain) {
          for {
            parentFactoryInterfaceType <- appliedFactoryInterfaceType(domain, finalizedType)
            _ <- addParent(parentFactoryInterfaceType)
          } yield ()
        } else Command.skip[ooParadigm.ClassContext]

        // Inherit previous data type case implementations
        _ <- forEach(domain.former) { ancestor =>
          for {
             parentDataTypeCaseInterface <- mostSpecificTypeCaseInterface(ancestor, finalizedType, newDataTypeCase)
            _ <- if (parentDataTypeCaseInterface.nonEmpty) {
              addParent(parentDataTypeCaseInterface.get)
            } else {
              Command.skip[ooParadigm.ClassContext]
            }
          } yield()
        }

        // Add abstract getters if defined here
        _ <- forEach(if (domain.typeCases.contains(newDataTypeCase)) newDataTypeCase.attributes else List.empty) { attribute =>
          addAbstractMethod(ComponentNames.getter(attribute), setAttributeGetterSignature(domain, finalizedType, attribute))
        }

        // Add methods for new operations
        // In Methods we use the least specific type (ep.Exp<FT>) to refer to the domain base type.
        _ <- addTypeLookupForMethods(TypeRep.DataType(domain.baseDataType), leastSpecialBaseInterfaceType(domain, finalizedType)(
          canFindClass = ooParadigm.methodBodyCapabilities.canFindClassInMethod,
          canResolveImport = paradigm.methodBodyCapabilities.canResolveImportInMethod,
          canAddImport = paradigm.methodBodyCapabilities.canAddImportInMethodBody,
          canApplyType = polymorphics.methodBodyCapabilities.canApplyTypeInMethod
        ))
        _ <- forEach(newOperations.toList) { newOperation =>
          addMethod(
            names.mangle(names.instanceNameOf(newOperation)),
            makeOperationImplementation(
              evolutionImplementationProvider = evolutionImplementationProvider,
              domain = domain,
              finalizedType = finalizedType,
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
      _ <- forEach(_newDataTypeCasesWithNewOperations.toList) { case (newDataTypeCase, newOperations) =>
        addClassToProject(makeNewTypeCaseInterface(newDataTypeCase, newOperations), names.mangle(names.instanceNameOf(domain)), names.mangle(names.conceptNameOf(newDataTypeCase)))
      }
    } yield ()
  }

  def finalizedBaseInterfaceType(domain: GenericModel): Generator[ooParadigm.ClassContext, paradigm.syntax.Type] = {
    import ooParadigm.classCapabilities._
    val _latestModelDefiningNewTypeInterface = latestModelDefiningNewTypeInterface(domain)
    for {
      finalizedBaseInterfaceType <-
        findClass(names.mangle(names.instanceNameOf(_latestModelDefiningNewTypeInterface)), ComponentNames.finalizedPackage, names.mangle(names.conceptNameOf(domain.baseDataType)))
      _ <- resolveAndAddImport(finalizedBaseInterfaceType)
    } yield finalizedBaseInterfaceType
  }

  def finalizedFactoryType(domain: GenericModel): Generator[ooParadigm.ClassContext, paradigm.syntax.Type] = {
    import ooParadigm.classCapabilities._
    for {
      finalizedFactoryType <-
        findClass(names.mangle(names.instanceNameOf(domain)), ComponentNames.finalizedPackage, ComponentNames.factory)
      _ <- resolveAndAddImport(finalizedFactoryType)
    } yield finalizedFactoryType
  }

  def mostSpecificTypeCaseClass[Context](domain: GenericModel, finalizedType: paradigm.syntax.Type, dataTypeCase: DataTypeCase)(implicit
    canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
    canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
    canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]]
  ): Generator[Context, Option[paradigm.syntax.Type]] = {
    val _latestModelDefiningDataTypeCaseInterface = latestModelDefiningDataTypeCaseInterface(domain, dataTypeCase)

    if (_latestModelDefiningDataTypeCaseInterface.isEmpty) {
      Command.lift[Context, Option[paradigm.syntax.Type]](Option.empty)
    } else {
      for {
        resultType <- FindClass[paradigm.syntax.Name, paradigm.syntax.Type](Seq(names.mangle(names.instanceNameOf(_latestModelDefiningDataTypeCaseInterface.get)), ComponentNames.finalizedPackage, names.mangle(names.conceptNameOf(dataTypeCase)))).interpret(canFindClass)
        _ <- resolveAndAddImport(resultType)
      } yield Some(resultType)
    }
  }

  def makeFinalizedFactoryMethod(domain: GenericModel, finalizedType: paradigm.syntax.Type, dataTypeCase: DataTypeCase): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      _ <- setFactoryMethodSignature(domain, finalizedType, dataTypeCase)
      attributeValues <- getArguments()
      typeToInstantiate <- mostSpecificTypeCaseClass(domain, finalizedType, dataTypeCase)
      result <- instantiateObject(typeToInstantiate.get, attributeValues.map(_._3))
    } yield Some(result)
  }

  def makeFinalizedConvertMethod(domain: GenericModel, finalizedType: paradigm.syntax.Type): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      _ <- setConvertMethodSignature(domain, finalizedType)
      arguments <- getArguments()
      toConvert = arguments.head._3
      getSelfMethod <- getMember(toConvert, ComponentNames.getSelfMethod)
      result <- apply(getSelfMethod, Seq.empty)
    } yield Some(result)
  }

  def addFinalizedFactoryInterface(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
    def makeNewFinalizedFactoryInterface(): Generator[ooParadigm.ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      val _latestModelDefiningNewTypeInterface = latestModelDefiningNewTypeInterface(domain)
      val currentDomainDefinesNewTypeInterface = _latestModelDefiningNewTypeInterface == domain
      for {
        finalizedType <- finalizedBaseInterfaceType(domain)

        // Set parent factory types
        nonFinalizedParentFactoryType <- appliedFactoryInterfaceType(domain, finalizedType)
        _ <- addParent(nonFinalizedParentFactoryType)

        _ <- if (!currentDomainDefinesNewTypeInterface) {
          forEach(domain.former) { ancestor =>
            for {
              parentFactoryType <- finalizedFactoryType(ancestor)
              _ <- addParent(parentFactoryType)
            } yield ()
          }
        } else Command.skip[ooParadigm.ClassContext]

        // Add factory methods
        _ <- forEach(domain.flatten.typeCases.distinct.filter(dataTypeCase => latestModelDefiningDataTypeCaseInterface(domain, dataTypeCase).contains(domain))) { tpeCase =>
          addMethod(names.mangle(names.instanceNameOf(tpeCase)), makeFinalizedFactoryMethod(domain, finalizedType, tpeCase))
        }

        // Add convert method
        _ <- addMethod(ComponentNames.convertMethod, makeFinalizedConvertMethod(domain, finalizedType))

        _ <- setInterface()
      } yield ()
    }

    import ooParadigm.projectCapabilities._
    addClassToProject(makeNewFinalizedFactoryInterface(), names.mangle(names.instanceNameOf(domain)), ComponentNames.finalizedPackage, ComponentNames.factory)
  }

  def makeFinalizedGetSelfMethod(finalizedType: paradigm.syntax.Type): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import ooParadigm.methodBodyCapabilities._
    for {
      _ <- setGetSelfMethodSignature(finalizedType)
      result <- selfReference()
    } yield Some(result)
  }

  def addFinalizedTypeInterfaceIfNecessary(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
    def makeNewFinalizedTypeInterface(): Generator[ooParadigm.ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      for {
        finalizedType <- finalizedBaseInterfaceType(domain)

        // Set parent factory type
        parentFactoryType <- finalizedFactoryType(domain)
        _ <- addParent(parentFactoryType)

        // Set parent type interface
        parentTypeInterface <- mostSpecificBaseInterfaceType(domain, finalizedType)
        _ <- addParent(parentTypeInterface)

        // Add getSelf method
        _ <- addMethod(ComponentNames.getSelfMethod, makeFinalizedGetSelfMethod(finalizedType))

        _ <- setInterface()
      } yield ()
    }

    if (domain != latestModelDefiningNewTypeInterface(domain)) {
      Command.skip
    } else {
      import ooParadigm.projectCapabilities._
      addClassToProject(makeNewFinalizedTypeInterface(), names.mangle(names.instanceNameOf(domain)), ComponentNames.finalizedPackage, names.mangle(names.conceptNameOf(domain.baseDataType)))
    }
  }

  def makeAttributeGetter(domain: GenericModel, finalizedType: paradigm.syntax.Type, attribute: abstractions.Attribute): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import ooParadigm.methodBodyCapabilities._
    for {
      _ <- setAttributeGetterSignature(domain, finalizedType, attribute)
      self <- selfReference()
      result <- getMember(self, names.mangle(names.instanceNameOf(attribute)))
    } yield Some(result)
  }

  def makeFinalizedDataTypeCaseConstructor(domain: GenericModel, baseTypeInterface: paradigm.syntax.Type, dataTypeCase: DataTypeCase): Generator[ooParadigm.ConstructorContext, Unit] = {
    import ooParadigm.constructorCapabilities._
    for {
      parameters <- forEach(dataTypeCase.attributes) { attribute =>
        if (attribute.tpe.isModelBase(domain)) {
          Command.lift[ooParadigm.ConstructorContext, (paradigm.syntax.Name, paradigm.syntax.Type)]((names.mangle(names.instanceNameOf(attribute)), baseTypeInterface))
        } else {
          for {
            parameterType <- toTargetLanguageType(attribute.tpe)
            _ <- resolveAndAddImport(parameterType)
          } yield (names.mangle(names.instanceNameOf(attribute)), parameterType)
        }
      }
      _ <- setParameters(parameters)
      arguments <- getArguments()
      _ <- forEach(dataTypeCase.attributes.zip(arguments)) { case (attribute, argument) =>
        initializeField(names.mangle(names.instanceNameOf(attribute)), argument._3)
      }
    } yield ()
  }

  def addFinalizedTypeCaseClasses(evolutionImplementationProvider: EvolutionImplementationProvider[this.type], domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
    val _newDataTypeCasesWithNewOperations = newDataTypeCasesWithNewOperations(evolutionImplementationProvider, domain)

    def makeNewFinalizedTypeCaseClass(newDataTypeCase: DataTypeCase) : Generator[ooParadigm.ClassContext, Unit] = {
      import ooParadigm.classCapabilities._

      for {
        finalizedType <- finalizedBaseInterfaceType(domain)

        // Set parent data type interface
        _ <- addImplemented(finalizedType)

        // Also inherit from latest factory if necessary because no new base interface was declared
        _ <- if (latestModelDefiningNewTypeInterface(domain) != domain) {
          for {
            parentFinalizedFactoryInterfaceType <- finalizedFactoryType(domain)
            _ <- addImplemented(parentFinalizedFactoryInterfaceType)
          } yield ()
        } else Command.skip[ooParadigm.ClassContext]

        // Inherit non finalized data type case implementation
        nonFinalizedDataTypeCaseInterface <- mostSpecificTypeCaseInterface(domain, finalizedType, newDataTypeCase)
        _ <- addImplemented(nonFinalizedDataTypeCaseInterface.get)

        // Add fields and their getters
        baseTypeInterface <- leastSpecialBaseInterfaceType(domain, finalizedType)
        _ <- forEach(newDataTypeCase.attributes) { attribute =>
          for {
            attributeType <- if (!attribute.tpe.isModelBase(domain)) {
                toTargetLanguageType(attribute.tpe)
              } else Command.lift[ooParadigm.ClassContext, paradigm.syntax.Type](baseTypeInterface)
            _ <- addField(names.mangle(names.instanceNameOf(attribute)), attributeType)
            _ <- addMethod(ComponentNames.getter(attribute), makeAttributeGetter(domain, finalizedType, attribute))
          } yield ()
        }

        // Add constructor
        _ <- addConstructor(makeFinalizedDataTypeCaseConstructor(domain, baseTypeInterface, newDataTypeCase))
      } yield ()
    }

    import ooParadigm.projectCapabilities._
    for {
      _ <- forEach(_newDataTypeCasesWithNewOperations.toList) { case (newDataTypeCase, _) =>
        addClassToProject(makeNewFinalizedTypeCaseClass(newDataTypeCase), names.mangle(names.instanceNameOf(domain)), ComponentNames.finalizedPackage, names.mangle(names.conceptNameOf(newDataTypeCase)))
      }
    } yield ()
  }
  */


  def addNewAlgebraInterfaceIfNecessary(model: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
    throw new NotImplementedError("not yet done")
  }

  def addNewAlgebraProducerInterfaceIfNecessary(model: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
    throw new NotImplementedError("not yet done")
  }

  def addOperationAlgebras(domainSpecific: EvolutionImplementationProvider[this.type], model: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
    throw new NotImplementedError("not yet done")
  }

  def appliedSignatureType(domain: GenericModel, carrierType: paradigm.syntax.Type): Generator[ooParadigm.ClassContext, paradigm.syntax.Type] = {
    import genericsParadigm.classCapabilities._
    import ooParadigm.classCapabilities._
    for {
      signatureInterface <- findClass(names.mangle(names.instanceNameOf(domain)), ComponentNames.signature)

      _ <- resolveAndAddImport(signatureInterface)
      resultType <- applyType(signatureInterface, Seq(carrierType))
    } yield resultType
  }

  def setSignatureMethodSignature(domain: GenericModel, carrierType: paradigm.syntax.Type, tpeCase: DataTypeCase): Generator[paradigm.MethodBodyContext, Unit] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._
    for {
      parameters <- forEach(tpeCase.attributes) { attribute =>
        if (attribute.tpe.isModelBase(domain)) {
          Command.lift[paradigm.MethodBodyContext, (paradigm.syntax.Name, paradigm.syntax.Type)]((names.mangle(names.instanceNameOf(attribute)), carrierType))
        } else {
          for {
            parameterType <- toTargetLanguageType(attribute.tpe)
            _ <- resolveAndAddImport(parameterType)
          } yield (names.mangle(names.instanceNameOf(attribute)), parameterType)
        }
      }
      _ <- setParameters(parameters)
      _ <- setReturnType(carrierType)
    } yield ()
  }

  def setOperationMethodSignature(domain: GenericModel, op:Operation): Generator[paradigm.MethodBodyContext, Unit] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._
    for {
      parameters <- forEach(op.parameters) { parameter =>
        for {
          parameterType <- toTargetLanguageType(parameter.tpe)
          _ <- resolveAndAddImport(parameterType)
        } yield (names.mangle(parameter.name), parameterType)
      }
      _ <- setParameters(parameters)
      returnType <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(returnType)
      _ <- setReturnType(returnType)
    } yield ()
  }

  def getSelfSignature(domain: GenericModel): Generator[paradigm.MethodBodyContext, Unit] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._
    for {
      returnType <- toTargetLanguageType(TypeRep.DataType(domain.baseDataType))
      _ <- setReturnType(returnType)
    } yield ()
  }

  def addCombinedCarrierInterfaceIfNecessary(domain:GenericModel) : Generator[paradigm.ProjectContext, Unit] = {
//    package m0.carrier;
//
//    public interface M0 extends Eval<M0> {
//
//    }
    def makeCarrierInterface(): Generator[ooParadigm.ClassContext, Unit] = {
      import genericsParadigm.classCapabilities._
      import ooParadigm.classCapabilities._
      for {
        _ <- setInterface()
        us <- findClass(names.mangle(names.instanceNameOf(domain)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(domain)))
        _ <- resolveAndAddImport(us)

        // Add type parameter for the finalized type
        _ <- forEach(domain.flatten.ops) { op =>
          val home = domain.findOperation(op).get

          for {
            parent <- findClass(names.mangle(names.instanceNameOf(home)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(op)))
            _ <- resolveAndAddImport(parent)
            appliedParent <- applyType(parent, Seq(us))
            _ <- addParent(appliedParent)
          } yield()
        }
      } yield ()
    }

    // If you find that your flattened ops doesn't match at least one of your formers, then you need this
    if (domain.former.exists(p => p.flatten.ops != domain.flatten.ops)) {
      import ooParadigm.projectCapabilities._
      addClassToProject(makeCarrierInterface(), names.mangle(names.instanceNameOf(domain)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(domain)))
    } else {
      Command.skip[paradigm.ProjectContext]
    }
  }

  def addOperationCarrierInterface(domain:GenericModel, op:Operation): Generator[paradigm.ProjectContext, Unit] = {
    //    public interface PowBy<C> {
    //      C getSelf();
    //      C powBy(C other);
    //    }
    def makeOperationCarrierInterface(): Generator[ooParadigm.ClassContext, Unit] = {
      import genericsParadigm.classCapabilities._
      import ooParadigm.classCapabilities._
      val dtpeRep = TypeRep.DataType(domain.baseDataType)
      for {
        _ <- setInterface()

        // Add type parameter for the finalized type
        _ <- addTypeParameter(ComponentNames.carrier, Command.skip)
        _ <- registerTypeMapping(domain)   // can only have AFTER declared type parameter
        _ <- addAbstractMethod(ComponentNames.getSelf, getSelfSignature(domain))

        _ <- addAbstractMethod(names.mangle(names.instanceNameOf(op)), setOperationMethodSignature(domain, op))
      } yield ()
    }
    import ooParadigm.projectCapabilities._
    addClassToProject(makeOperationCarrierInterface(), names.mangle(names.instanceNameOf(domain)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(op)))
  }


  def addCombinedCarrierInstance(domain:GenericModel): Generator[paradigm.ProjectContext, Unit] = {
    // do I add an operation?; or do I add a data type AFTER a producer method? or what about merge WHEN at least one of
    // your branches HAS a producer method?

//    package m0.carrier.instance;
//
//    import m0.carrier.Eval;
//
//    public final class M0 implements m0.carrier.M0 {
//      m0.carrier.Eval<m0.carrier.M0> eval;
//
//      public M0(Eval<m0.carrier.M0> eval) {
//        this.eval = eval;
//      }
//
//      @Override public m0.carrier.M0 getSelf() {
//        return this;
//      }
//
//      @Override public double eval() {
//        return eval.eval();
//      }
//    }


    def makeOperationClass(): Generator[ooParadigm.ClassContext, Unit] = {

      def makeOperationImpl(op:Operation): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ooParadigm.methodBodyCapabilities._
        import paradigm.methodBodyCapabilities._
        for {
          returnType <- toTargetLanguageType(op.returnType)
          _ <- resolveAndAddImport(returnType)
          _ <- setReturnType(returnType)

          parameters <- forEach(op.parameters) { parameter =>
            for {
              parameterType <- toTargetLanguageType(parameter.tpe)
              _ <- resolveAndAddImport(parameterType)
            } yield (names.mangle(parameter.name), parameterType)
          }
          _ <- setParameters(parameters)

          self <- selfReference()
          carrierInstance <- getMember(self, names.mangle(names.instanceNameOf(op)))
          method <- getMember(carrierInstance, names.mangle(names.instanceNameOf(op)))
          args <- getArguments()
          result <- apply(method, args.map(_._3))
        } yield Some(result)
      }

      def getSelfMethod(interfaceType:paradigm.syntax.Type) : Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ooParadigm.methodBodyCapabilities._
        import paradigm.methodBodyCapabilities._
        for {
          _ <- setReturnType(interfaceType)
          self <- selfReference()
        }  yield Some(self)
      }

      def makeOperationConstructor(interfaceType:paradigm.syntax.Type): Generator[ooParadigm.ConstructorContext, Unit] = {
        import ooParadigm.constructorCapabilities._
        import genericsParadigm.constructorCapabilities._
        for {
          parameters <- forEach(domain.flatten.ops) { op =>
            for {
              // must be where operation is defined. NOTE: It must be there because otherwise we wouldn't be called.
              tpe <- findClass(names.mangle(names.instanceNameOf(domain.findOperation(op).get)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(op)))
              _ <- resolveAndAddImport(tpe)
              finalTpe <- applyType(tpe, Seq(interfaceType))
            } yield (names.mangle(names.instanceNameOf(op)), finalTpe)
          }

          _ <- setParameters(parameters)
          arguments <- getArguments()
          _ <- forEach (arguments) { arg =>
            for {
              _ <- initializeField(arg._1, arg._3)
            } yield ()
          }
        } yield ()
      }

      import ooParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._
      for {
        _ <- registerInstanceTypeMapping(domain)    // CRITICAL to do before generating any subsequent artifacts

        interfaceType <- findClass(names.mangle(names.instanceNameOf(domain)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(domain)))
        _ <- resolveAndAddImport(interfaceType)
        _ <- addImplemented(interfaceType)

        _ <- forEach(domain.flatten.ops) { op =>
          for {
            // Note: Must find the class where op was defined. MUST be able to .get() result
            tpe <- findClass(names.mangle(names.instanceNameOf(domain.findOperation(op).get)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(op)))
            _ <- resolveAndAddImport(tpe)
            finalTpe <- applyType(tpe, Seq(interfaceType))

            _ <- addField(names.mangle(names.instanceNameOf(op)), finalTpe)
            _ <- addMethod(names.mangle(names.instanceNameOf(op)), makeOperationImpl(op))
          } yield ()
        }
        _ <- addConstructor(makeOperationConstructor(interfaceType))

        // need get Self if ANY operation is binary or producer
        _ <- addMethod(ComponentNames.getSelf, getSelfMethod(interfaceType))

      } yield ()
    }

    // need to register new types because now we "redefine"
    import ooParadigm.projectCapabilities._
    addClassToProject(makeOperationClass(), names.mangle(names.instanceNameOf(domain)), ComponentNames.pkgCarrier, ComponentNames.pkgInstance, names.mangle(names.conceptNameOf(domain)))
  }

  def addOperationCarrierInstance(domain:GenericModel, op:Operation): Generator[paradigm.ProjectContext, Unit] = {
    //    public final class Eval implements m0.carrier.Eval {
    //      private final double value;
    //
    //      public Eval(final double value) {
    //        this.value = value;
    //      }
    //
    //      @Override public double eval() {
    //        return value;
    //      }
    //    }

    def makeOperationClass(): Generator[ooParadigm.ClassContext, Unit] = {

      def makeOperationImpl(): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ooParadigm.methodBodyCapabilities._
        import paradigm.methodBodyCapabilities._
        for {
          returnType <- toTargetLanguageType(op.returnType)
          _ <- resolveAndAddImport(returnType)
          _ <- setReturnType(returnType)
          self <- selfReference()
          result <- getMember(self, ComponentNames.value)
        } yield Some(result)
      }

      def makeOperationConstructor(): Generator[ooParadigm.ConstructorContext, Unit] = {
        import ooParadigm.constructorCapabilities._
        for {
          parameter <- toTargetLanguageType(op.returnType)
          _ <- setParameters(Seq((ComponentNames.value, parameter)))
          arguments <- getArguments()
          _ <- initializeField(ComponentNames.value, arguments.head._3)
        } yield ()
      }

      import ooParadigm.classCapabilities._
      for {
        interfaceType <- findClass(names.mangle(names.instanceNameOf(domain)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(op)))
        _ <- resolveAndAddImport(interfaceType)
        _ <- addImplemented(interfaceType)

        tpe <- toTargetLanguageType(op.returnType)
        _ <- addField(ComponentNames.value, tpe)

        _ <- addConstructor(makeOperationConstructor())
        _ <- addMethod(names.mangle(names.instanceNameOf(op)), makeOperationImpl())
      } yield ()
    }

    import ooParadigm.projectCapabilities._
    addClassToProject(makeOperationClass(), names.mangle(names.instanceNameOf(domain)), ComponentNames.pkgCarrier, ComponentNames.pkgInstance, names.mangle(names.conceptNameOf(op)))
  }

  def addAlgebraOperation(domain:GenericModel, domainSpecific: EvolutionImplementationProvider[this.type], op:Operation): Generator[paradigm.ProjectContext, Unit] = {
    //    public final class Eval implements Signature<m0.carrier.Eval> {
    //      @Override public m0.carrier.Eval lit(double value) {
    //        return new m0.carrier.instance.Eval(value);
    //      }
    //
    //      @Override public m0.carrier.Eval add(m0.carrier.Eval left, m0.carrier.Eval right) {
    //        return new m0.carrier.instance.Eval(left.eval() + right.eval());
    //      }
    //    }
    def setMethodSignature(tpe:DataTypeCase): Generator[paradigm.MethodBodyContext, Unit] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import polymorphics.methodBodyCapabilities._
      for {
        // Translate result and parameter types
//        carrierType <- findClass(names.mangle(names.instanceNameOf(domain.findOperation(op).get)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(op)))
//        _ <- resolveAndAddImport(carrierType)

        baseType <- toTargetLanguageType(TypeRep.DataType(domain.baseDataType))
        signature <- forEach(tpe.attributes) { att => toTargetLanguageType(att.tpe) }
//          if (att.tpe.isModelBase(domain)) {
//            Command.lift[paradigm.MethodBodyContext, paradigm.syntax.Type](carrierType)
//          } else {
//            for {
//              signatureType <- toTargetLanguageType(att.tpe)
//              _ <- resolveAndAddImport(signatureType)
//            } yield signatureType
//          }
//        }
        _ <- setParameters(tpe.attributes.map(att => names.mangle(att.name)).zip(signature))
        _ <- setReturnType(baseType)
      } yield ()
    }

    def newOpMethod(tpe:DataTypeCase): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      for {
        _ <- setMethodSignature(tpe)
        latest = latestModelDefiningNewTypeInterface(domain)
        inst <- findClass(names.mangle(names.instanceNameOf(latest)), ComponentNames.pkgCarrier, ComponentNames.pkgInstance, names.mangle(names.conceptNameOf(op)))
        _ <- resolveAndAddImport(inst)
        cons <- getConstructor(inst)
        //result <- apply(cons, Seq.empty)
        attributeValues <- getArguments()
        result <- instantiateObject(inst, attributeValues.map(_._3))

      } yield Some(result)
    }

    def makeAlgebraOperation(): Generator[ooParadigm.ClassContext, Unit] = {
      import genericsParadigm.classCapabilities._
      import ooParadigm.classCapabilities._
      for {

//        // Add type parameter for the finalized type
//        _ <- if (op.isBinary(domain) || op.isProducer(domain)) {
//          val dtpeRep = TypeRep.DataType(domain.baseDataType)
//          for {
//            _ <- addTypeParameter(ComponentNames.carrier, Command.skip)
//            _ <- registerTypeMapping(domain)   // can only have AFTER declared type parameter
//            _ <- addAbstractMethod(ComponentNames.getSelf, getSelfSignature(domain))
//          } yield ()
//        } else {
//          Command.skip[ooParadigm.ClassContext]
//        }

        tpeParams <- getTypeArguments()
        _ <- registerInstanceTypeMappingOperation(domain, tpeParams, op)
        _ <- forEach (domain.flatten.typeCases) { tpe =>
          for {
            _ <- addMethod(names.mangle(names.instanceNameOf(tpe)), newOpMethod(tpe), true)
          } yield ()
        }

      } yield ()
    }
    import ooParadigm.projectCapabilities._
    addClassToProject(makeAlgebraOperation(), names.mangle(names.instanceNameOf(domain)), ComponentNames.pkgAlgebra, names.mangle(names.conceptNameOf(op)))
  }

  //  public interface Signature<Carrier> {
  //    Carrier lit(double value);
  //    Carrier add(Carrier left, Carrier right);
  //  }
  def addSignatureInterface(domain:GenericModel): Generator[paradigm.ProjectContext, Unit] = {

    def makeNewSignatureInterface(): Generator[ooParadigm.ClassContext, Unit] = {
      import genericsParadigm.classCapabilities._
      import ooParadigm.classCapabilities._
      for {

        // Add type parameter for the finalized type
        _ <- addTypeParameter(ComponentNames.carrier, Command.skip)
        carrierType <- getTypeArguments().map(tpeArgs => tpeArgs.head)

        _ <- registerTypeMapping(domain)

        // Set parent factory types
        //        public interface Signature<Carrier> extends m0.Signature<Carrier> {
        //          Carrier sub(Carrier left, Carrier right);
        //        }
        // public interface Signature<Carrier> extends m4.Signature<Carrier>, i2.Signature<Carrier>, m3i1.Signature<Carrier> {

        _ <- forEach(domain.former) { ancestor =>
          if (ancestor.isDomainBase) {
            Command.skip[ooParadigm.ClassContext]
          } else {
            for {
              parentSignatureType <- appliedSignatureType(ancestor, carrierType)
              _ <- addParent(parentSignatureType)
            } yield ()
          }
        }

        //        public interface Signature<Carrier> {
        //          Carrier lit(double value);
        //          Carrier add(Carrier left, Carrier right);
        //        }
        // Add factory methods
        _ <- forEach(domain.typeCases) { tpeCase =>
          addAbstractMethod(names.mangle(names.instanceNameOf(tpeCase)), setSignatureMethodSignature(domain, carrierType, tpeCase))
        }

        _ <- setInterface()
      } yield ()
    }
    import ooParadigm.projectCapabilities._

    addClassToProject(makeNewSignatureInterface(), names.mangle(names.instanceNameOf(domain)), ComponentNames.signature)
  }

  def appliedSignatureType(domain: GenericModel): Generator[ooParadigm.ClassContext, paradigm.syntax.Type] = {
    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._
    for {
      signatureInteface <- findClass(names.mangle(names.instanceNameOf(domain)), ComponentNames.signature)
      carrierInterface <- findClass(names.mangle(names.instanceNameOf(domain)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(domain)))

      _ <- resolveAndAddImport(signatureInteface)
      resultType <- applyType(signatureInteface, Seq(carrierInterface))
      _ <- addImplemented(resultType)
    } yield resultType
  }

  def addAlgebraModel(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {


    def makeNewAlgebraModel(): Generator[ooParadigm.ClassContext, Unit] = {
      import genericsParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._
      import ooParadigm.classCapabilities._
      val latest = latestModelDefiningNewTypeInterface(domain)
      for {
        // Add type parameter for the finalized type
        _ <- appliedSignatureType(latest)

        tpeParam <- findClass(names.mangle(names.instanceNameOf(latest)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(latest)))
        _ <- resolveAndAddImport(tpeParam)

        // Need to add final attributes for all existing operations
        _ <- forEach (domain.flatten.ops) { op =>
          //val location = domain.findOperation(op).get //must always exist

          for {
            opType <- findClass(names.mangle(names.instanceNameOf(latest)), ComponentNames.pkgAlgebra, names.mangle(names.conceptNameOf(op)))
            _ <- resolveAndAddImport(opType)
            _ <- addField(names.mangle(names.instanceNameOf(op)), opType)

          } yield ()
        }

        //_ <- addImplemented(tpeParam)
        //_ <- addTypeParameter(tpeParam, Command.skip)
        //finalizedType <- getTypeArguments().map(tpeArgs => tpeArgs.head)
        //
        //        // Set parent factory type
        //        parentFactoryType <- appliedFactoryInterfaceType(domain, finalizedType)
        //        _ <- addParent(parentFactoryType)
        //
        //        // Set parent type interfaces
        //        _ <- forEach(ancestorsDefiningNewTypeInterfaces(domain).toList) { ancestor =>
        //          for {
        //            parentTypeInterface <- mostSpecificBaseInterfaceType(ancestor, finalizedType)
        //            _ <- addParent(parentTypeInterface)
        //          } yield ()
        //        }
        //
        //        // Add methods for new operations
        //        _ <- forEach(domain.ops) { operation =>
        //          ooParadigm.classCapabilities.addAbstractMethod(
        //            names.mangle(names.instanceNameOf(operation)),
        //            setOperationMethodSignature(domain, finalizedType, operation)
        //          )
        //        }
        //
        //        _ <- setInterface()
      } yield ()
    }
    // DO in all cases because we don't want to get involved in inheritance
    import ooParadigm.projectCapabilities._
    addClassToProject(makeNewAlgebraModel(), names.mangle(names.instanceNameOf(domain)), ComponentNames.pkgAlgebra, names.mangle(names.conceptNameOf(domain)))
  }

  def addCarrierModel(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
    def makeNewCarrierModel(): Generator[ooParadigm.ClassContext, Unit] = {
      import genericsParadigm.classCapabilities._
      import ooParadigm.classCapabilities._
      for {
        _ <- setInterface()

        tpeParam <- findClass(names.mangle(names.instanceNameOf(domain)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(domain)))
        _ <- resolveAndAddImport(tpeParam)

        _ <- forEach (domain.flatten.ops) { op =>
          val location = domain.findOperation(op).get   //must always exist

          for {
            carrierType <- findClass(names.mangle(names.instanceNameOf(location)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(op)))
            _ <- resolveAndAddImport(carrierType)
            fullCarrierType <- if (op.isProducer(domain) || op.isBinary(domain)) {
              for {
                result <- applyType(carrierType, Seq(tpeParam))
              } yield (result)
            } else {
              Command.lift[ooParadigm.ClassContext, paradigm.syntax.Type](carrierType)
            }
            _ <- addParent(fullCarrierType)
          } yield ()
        }
      } yield ()
    }

    // DO in all cases because we don't want to get involved in inheritance
    if (domain.ops.nonEmpty || domain.former.length > 1) {
      import ooParadigm.projectCapabilities._
      addClassToProject(makeNewCarrierModel(), names.mangle(names.instanceNameOf(domain)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(domain)))
    } else {
      Command.skip[paradigm.ProjectContext]
    }
  }

  ////////////////////////////////////////////////////////////////

  /** Map data type cases to operations that require a new implementation in the given domain model.
   * Will only contain data type cases which have been newly introduced in at least one of the ancestor branches
   * or require an update because of missing/overwritten operations or merging of multiple branches.
   */
  def newDataTypeCasesWithNewOperations(evolutionImplementationProvider: EvolutionImplementationProvider[this.type], domain: GenericModel): Map[DataTypeCase, Set[Operation]] = {
    val flatDomain = domain.flatten
    val allDataTypeCases = flatDomain.typeCases.toSet
    val allOperations = flatDomain.ops.toSet

    allDataTypeCases.foldLeft(Map.empty[DataTypeCase, Set[Operation]]) { (resultMap, tpeCase) =>
      // Remembers all operations that are already supported
      val presentOperations = domain.former.flatMap(ancestor => {
        if (ancestor.supports(tpeCase)) {
          ancestor.flatten.ops.toSet
        } else {
          Set.empty[Operation]
        }
      })

      val overwrittenOperations = allOperations.filter { operation =>
        // Are we applicable based on EIP? Tells us in which domain EIP is applicable
        val lastOverwritingDomain =
          evolutionImplementationProvider.applicableIn(
            forApproach = this,
            potentialRequest = PotentialRequest(domain.baseDataType, tpeCase, operation),
            currentModel = domain
          )
        lastOverwritingDomain.contains(domain)
      }
      val updatedOperations = (allOperations -- presentOperations) ++ overwrittenOperations
      // If we have any updated operations, if we have a former one that doesn't support the current type case, or if we are in a merge.
      if (updatedOperations.nonEmpty || domain.former.exists(ancestor => !ancestor.supports(tpeCase)) || domain.former.size > 1) {
        resultMap.updated(tpeCase, updatedOperations)
      } else {
        resultMap
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////

  def addOperationCarrierInstanceForDataTypeCase(domain:GenericModel, dt:DataTypeCase, op:Operation): Generator[paradigm.ProjectContext, Unit] = {
//    public final class Add<C extends MultBy<C>> implements MultBy<C> {
//      private final i1.Signature<C> algebra;
//      private final C left;
//      private final C right;
//
//      public Add(Signature<C> algebra, C left, C right) {
//        this.algebra = algebra;
//        this.left = left;
//        this.right = right;
//      }
//
//      @Override public C getSelf() {
//        return algebra.add(left, right);
//      }
//
//      @Override public C multBy(C other) {
//        return algebra.add(left.multBy(other), right.multBy(other));
//      }
//    }

    def makeOperationClass(): Generator[ooParadigm.ClassContext, Unit] = {

      def makeOperationImpl(): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ooParadigm.methodBodyCapabilities._
        import paradigm.methodBodyCapabilities._
        for {
          returnType <- toTargetLanguageType(op.returnType)
          _ <- resolveAndAddImport(returnType)
          _ <- setReturnType(returnType)
          self <- selfReference()
          result <- getMember(self, ComponentNames.value)
        } yield Some(result)
      }

      def getSelfMethod(interfaceType:paradigm.syntax.Type) : Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ooParadigm.methodBodyCapabilities._
        import paradigm.methodBodyCapabilities._
        for {
          _ <- setReturnType(interfaceType)
          self <- selfReference()
        }  yield Some(self)
      }

      def makeOperationConstructor(): Generator[ooParadigm.ConstructorContext, Unit] = {
        import ooParadigm.constructorCapabilities._
        for {
          signatureTpe <- findClass(names.mangle(names.instanceNameOf(domain)), ComponentNames.signature)
          _ <- resolveAndAddImport(signatureTpe)
          parameter <- toTargetLanguageType(op.returnType)
          _ <- setParameters(Seq((ComponentNames.algebraAtt, signatureTpe), (ComponentNames.value, parameter)))
          arguments <- getArguments()
          _ <- forEach(arguments) { arg =>
            for {
              _ <- initializeField(arg._1, arg._3)
            } yield ()
          }
        } yield ()
      }

//      def helperType(): Generator[ooParadigm.ClassContext, paradigm.syntax.Type] = {
//        import ooParadigm.classCapabilities.findClass
//        import ooParadigm.classCapabilities.freshName
//        import polymorphics.methodBodyCapabilities._
//        // avoid conflicts by targeting just these two
//        import genericsParadigm.typeParameterCapabilities._
//        for {
//          interfaceType <- findClass(names.mangle(names.instanceNameOf(domain)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(op)))
//          carrierTypeParam <- freshName(ComponentNames.returnTypeParameter)
//          genericInterfaceType <- addUpperBoundInTypeParameter(interfaceType, carrierTypeParam)
//        } yield (genericInterfaceType)
//      }

//      def makeTypeParameter(name:paradigm.syntax.Name, tpe:paradigm.syntax.Type) :  Generator[polymorphics.TypeParameterContext, paradigm.syntax.Type] = {
//        import genericsParadigm.typeParameterCapabilities._
//
//        for {
//          clazz <- findClass(name)
//          itype <- applyType(tpe, )
//          head <- addLowerBound()
//        } yield()
//      }

      import ooParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._
      for {
        interfaceType <- findClass(names.mangle(names.instanceNameOf(domain)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(op)))
        carrierTypeParam <- freshName(ComponentNames.returnTypeParameter)
        //genericInterfaceType <- helperType()

        //_ <- addTypeParameter(carrierTypeParam, makeTypeParameter(carrierTypeParam, interfaceType))
        tpeParams <- getTypeArguments()

        _ <- addMethod(ComponentNames.getSelf, getSelfMethod(tpeParams.head))

        _ <- resolveAndAddImport(interfaceType)
        _ <- addImplemented(interfaceType)

        signatureTpe <- findClass(names.mangle(names.instanceNameOf(domain)), ComponentNames.signature)
        signatureTpeGeneric <- applyType(signatureTpe, Seq(interfaceType))
        _ <- addField(ComponentNames.algebraAtt, signatureTpeGeneric)

        _ <- forEach(dt.attributes) { att =>
          for {
            tpe <- toTargetLanguageType(att.tpe)
            _ <- addField(names.mangle(names.instanceNameOf(att)), tpe)
          } yield()
        }
        _ <- addConstructor(makeOperationConstructor())
        _ <- addMethod(names.mangle(names.instanceNameOf(op)), makeOperationImpl())
      } yield ()
    }

    import ooParadigm.projectCapabilities._
    addClassToProject(makeOperationClass(), names.mangle(names.instanceNameOf(domain)), ComponentNames.pkgCarrier, ComponentNames.pkgInstance, names.mangle(names.instanceNameOf(op)), names.mangle(names.conceptNameOf(dt)))
  }

  def implement(domain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    def implementRecursive(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
      if (domain.isDomainBase) {
        Command.skip[paradigm.ProjectContext]
      } else {

        for {
          _ <- addSignatureInterface(domain)

          _ <- forEach(domain.ops) { op =>
            for {

              _ <- addOperationCarrierInterface(domain, op)
              _ <- addAlgebraOperation(domain, domainSpecific, op)

              //_ <- addOperationCarrierInstance (domain, op)

            } yield ()
          }

          _ <- forEach(newDataTypeCasesWithNewOperations(domainSpecific, domain).toSeq) { case (dataTypeCase, ops) =>
            for {
              _ <- forEach (ops.toSeq){ op =>
                for {
                  _ <- addOperationCarrierInstanceForDataTypeCase(domain, dataTypeCase, op)
                } yield()
              }
            } yield()
          }

          _ <- if (domain == latestModelDefiningNewTypeInterface(domain)) {
            for {
              _ <- addCarrierModel(domain)
              _ <- addCombinedCarrierInstance(domain)
            } yield ()
          } else {
            Command.skip[paradigm.ProjectContext]
          }

          // _ <- addAlgebraModel (domain)

          _ <- addCombinedCarrierInterfaceIfNecessary(domain)
          //          _ <- addNewAlgebraInterfaceIfNecessary(domain)
          //          _ <- addNewAlgebraProducerInterfaceIfNecessary(domain)
          //          _ <- addOperationAlgebras(domainSpecific, domain)
          _ <- forEach(domain.former) { ancestor => implementRecursive(ancestor) }
        } yield ()
      }
    }

    for {
      _ <- domainSpecific.initialize(this)
      _ <- implementRecursive(domain)
    } yield ()
  }

  def registerInstanceTypeMappingOperation(model: GenericModel, tpeParams:Seq[paradigm.syntax.Type], op:Operation): Generator[ooParadigm.ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._
    import genericsParadigm.constructorCapabilities._     // NEEDED, despite IntelliJ editor
    import paradigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._
    import ooParadigm.constructorCapabilities._
    import ooParadigm.methodBodyCapabilities._
    val dtpeRep = TypeRep.DataType(model.baseDataType)

    def properCarrierType[Context](implicit
                                   canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
                                   canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
                                   canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]],
                                   canApplyType: Understands[Context, Apply[paradigm.syntax.Type,paradigm.syntax.Type,paradigm.syntax.Type]]
                                  ): Generator[Context, paradigm.syntax.Type] = {
      for {
        baseInterfaceType <- FindClass[paradigm.syntax.Name, paradigm.syntax.Type](Seq(names.mangle(names.instanceNameOf(model)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(op)))).interpret(canFindClass)
        _ <- resolveAndAddImport(baseInterfaceType)(canResolveImport,canAddImport)
        appliedInterfaceType <- Apply[paradigm.syntax.Type,paradigm.syntax.Type,paradigm.syntax.Type](baseInterfaceType, tpeParams).interpret(canApplyType)


      } yield appliedInterfaceType
    }

    for {
      _ <- addTypeLookupForMethods(dtpeRep, properCarrierType[paradigm.MethodBodyContext])
      _ <- addTypeLookupForClasses(dtpeRep, properCarrierType[ooParadigm.ClassContext])
      _ <- addTypeLookupForConstructors(dtpeRep, properCarrierType[ooParadigm.ConstructorContext])
    } yield ()
  }

  def registerInstanceTypeMapping(model: GenericModel): Generator[ooParadigm.ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._
    import paradigm.methodBodyCapabilities._
    import ooParadigm.constructorCapabilities._
    import ooParadigm.methodBodyCapabilities._
    val dtpeRep = TypeRep.DataType(model.baseDataType)

    def properCarrierType[Context](implicit
                                   canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
                                   canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
                                   canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]]
                                  ): Generator[Context, paradigm.syntax.Type] = {
      for {
        baseInterfaceType <- FindClass[paradigm.syntax.Name, paradigm.syntax.Type](Seq(names.mangle(names.instanceNameOf(model)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(model)))).interpret(canFindClass)
        _ <- resolveAndAddImport(baseInterfaceType)(canResolveImport,canAddImport)
      } yield baseInterfaceType
    }

    for {
      _ <- addTypeLookupForMethods(dtpeRep, properCarrierType[paradigm.MethodBodyContext])
      _ <- addTypeLookupForClasses(dtpeRep, properCarrierType[ooParadigm.ClassContext])
      _ <- addTypeLookupForConstructors(dtpeRep, properCarrierType[ooParadigm.ConstructorContext])
    } yield ()
  }

  def registerTypeMapping(model: GenericModel): Generator[ooParadigm.ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._

    val dtpeRep = TypeRep.DataType(model.baseDataType)

    for {
      carrierType <- getTypeArguments().map(tpeArgs => tpeArgs.head)
      _ <- addTypeLookupForMethods(dtpeRep, Command.lift(carrierType))
      _ <- addTypeLookupForClasses(dtpeRep, Command.lift(carrierType))
      _ <- addTypeLookupForConstructors(dtpeRep, Command.lift(carrierType))
    } yield ()
  }

  override def implement(tests: Map[GenericModel, Seq[TestCase]], testImplementationProvider: TestImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    import paradigm.compilationUnitCapabilities._
    import paradigm.projectCapabilities._
    import paradigm.testCapabilities._
    Command.skip[paradigm.ProjectContext]

    /*for {
      _ <- forEach(tests.toList) { case (model, tests) => {
        val testCode: Generator[paradigm.MethodBodyContext, Seq[paradigm.syntax.Expression]] =
          for {
            code <- forEach(tests) {
              test => testImplementationProvider.test(this)(test)
            }
          } yield code.flatten

        import ooParadigm.testCapabilities._
        val compUnit = for {

          // add test case first
          _ <- addTestCase(testCode, testName)
          factory <- findClass(names.mangle(model.name), ComponentNames.finalizedPackage, ComponentNames.factory)
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
    } yield () */
  }

  def dispatch(message: communication.SendRequest[paradigm.syntax.Expression]): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    /*for {
      self <- selfReference()
      convert <- getMember(self, ComponentNames.convertMethod)  // convert the receiver of the dispatch as late as possible
      converted <- apply(convert, Seq(message.to))
      method <- getMember(converted, names.mangle(names.instanceNameOf(message.request.op)))

      result <- apply(method, message.request.op.parameters.map(message.request.arguments))
    } yield result*/
    throw new NotImplementedError("not yet done")
  }

  def instantiate(baseTpe: abstractions.DataType, tpeCase: abstractions.DataTypeCase, args: paradigm.syntax.Expression*): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    /*for {
      thisRef <- selfReference()
      factory <- getMember(thisRef, names.mangle(names.instanceNameOf(tpeCase)))
      res <- apply(factory, args)
    } yield res */
    throw new NotImplementedError("not yet done")

  }
}



object ObjectAlgebras {
  type WithParadigm[P <: AnyParadigm] = ObjectAlgebras {val paradigm: P}
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (
    nameProvider: NameProvider[base.syntax.Name],
    oo: ObjectOriented.WithBase[base.type],
    params: ParametricPolymorphism.WithBase[base.type]
  )
  (generics: Generics.WithBase[base.type, oo.type, params.type]): ObjectAlgebras.WithParadigm[base.type] =
    new ObjectAlgebras {
      val paradigm: base.type = base
      val ooParadigm: oo.type = oo
      val polymorphics: params.type = params
      val genericsParadigm: generics.type = generics
      val names: NameProvider[paradigm.syntax.Name] = nameProvider
    }
}