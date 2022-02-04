package org.combinators.ep.approach.oo  /*DI:LI:AD*/

import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, Operation, TestCase, TypeRep}
import org.combinators.ep.domain.{GenericModel, abstractions}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.{AbstractSyntax, ApproachImplementationProvider, Command, EvolutionImplementationProvider, NameProvider, TestImplementationProvider, Understands, communication}
import org.combinators.ep.generator.paradigm.{AddImport, AnyParadigm, Apply, FindClass, Generics, ObjectOriented, ParametricPolymorphism, ResolveImport}

import scala.tools.nsc.interpreter.NamedParamClass


trait CoCoClean extends ApproachImplementationProvider {
  val paradigm: AnyParadigm
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]
  val names: NameProvider[paradigm.syntax.Name]

  object ComponentNames {
    val factory = names.mangle("Factory")
    val convertMethod = names.mangle("convert")
    val convertMethodParameter = names.mangle("toConvert")
    val finalizedTypeParameter = names.mangle("FT")
    val finalizedPackage = names.mangle("finalized")
    val getSelfMethod = names.mangle("getSelf")

    def getter(attribute: abstractions.Attribute): paradigm.syntax.Name = {
      names.addPrefix("get", names.mangle(names.conceptNameOf(attribute)))
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
      val ancestorsWithTypeInterfaces = ancestorsDefiningNewTypeInterfaces(domain)
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

  def setConvertMethodSignature(domain: GenericModel, finalizedType: paradigm.syntax.Type): Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._
    for {
      typeToConvert <- leastSpecialBaseInterfaceType(domain, finalizedType)
      _ <- setParameters(Seq((ComponentNames.convertMethodParameter, typeToConvert)))
      conversionResultType <- mostSpecificBaseInterfaceType(domain, finalizedType)
      _ <- setReturnType(conversionResultType)
    } yield ()
  }

  def addBasicFactoryInterface(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
    def makeBasicFactoryInterface(): Generator[ooParadigm.ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._
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
    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._
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
      import ooParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._
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
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
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
      import ooParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._
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
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
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
      import ooParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._
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
      val updatedOperations = (allOperations -- presentOperations) ++ overwrittenOperations
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
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
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
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
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
      import ooParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._

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
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      _ <- setFactoryMethodSignature(domain, finalizedType, dataTypeCase)
      attributeValues <- getArguments()
      typeToInstantiate <- mostSpecificTypeCaseClass(domain, finalizedType, dataTypeCase)
      result <- instantiateObject(typeToInstantiate.get, attributeValues.map(_._3))
    } yield Some(result)
  }

  def makeFinalizedConvertMethod(domain: GenericModel, finalizedType: paradigm.syntax.Type): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
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
      import genericsParadigm.classCapabilities._
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
      import genericsParadigm.classCapabilities._
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
      import genericsParadigm.classCapabilities._

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

  def implement(domain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    def implementRecursive(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
      if (domain.isDomainBase) {
        for {
          _ <- addBasicFactoryInterface(domain)
          _ <- addBasicTypeInterface(domain)
        } yield ()
      } else {
        for {
          _ <- addNewFactoryInterface(domain)
          _ <- addNewTypeInterfaceIfNecessary(domain)
          _ <- addDataTypeCaseInterfaces(domainSpecific, domain)
          _ <- addFinalizedFactoryInterface(domain)
          _ <- addFinalizedTypeInterfaceIfNecessary(domain)
          _ <- addFinalizedTypeCaseClasses(domainSpecific, domain)
          _ <- forEach(domain.former) { ancestor => implementRecursive(ancestor) }
        } yield ()
      }
    }
    for {
      _ <- domainSpecific.initialize(this)
      _ <- implementRecursive(domain)
    } yield ()
  }

  def registerTypeMapping(model: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    import ooParadigm.classCapabilities.canFindClassInClass
    import ooParadigm.constructorCapabilities.canFindClassInConstructor
    import ooParadigm.methodBodyCapabilities.canFindClassInMethod
    import paradigm.projectCapabilities._
    import org.combinators.ep.generator.Understands
    import org.combinators.ep.generator.paradigm.FindClass

    def domainTypeLookup[Ctxt](covariantType: paradigm.syntax.Name*)(implicit canFindClass: Understands[Ctxt, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]]): Generator[Ctxt, paradigm.syntax.Type] = {
      FindClass(covariantType).interpret(canFindClass)
    }

    val baseInterface = Seq(names.mangle(names.conceptNameOf(model.baseDataType)))
    val dtpeRep = TypeRep.DataType(model.baseDataType)

    for {
      _ <- addTypeLookupForMethods(dtpeRep, domainTypeLookup(baseInterface: _*))
      _ <- addTypeLookupForClasses(dtpeRep, domainTypeLookup(baseInterface: _*))
      _ <- addTypeLookupForConstructors(dtpeRep, domainTypeLookup(baseInterface: _*))
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
    } yield ()
  }

  def dispatch(message: communication.SendRequest[paradigm.syntax.Expression]): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      self <- selfReference()
      convert <- getMember(self, ComponentNames.convertMethod)  // convert the receiver of the dispatch as late as possible
      converted <- apply(convert, Seq(message.to))
      method <- getMember(converted, names.mangle(names.instanceNameOf(message.request.op)))

      result <- apply(method, message.request.op.parameters.map(message.request.arguments))
    } yield result
  }

  def instantiate(baseTpe: abstractions.DataType, tpeCase: abstractions.DataTypeCase, args: paradigm.syntax.Expression*): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      thisRef <- selfReference()
      factory <- getMember(thisRef, names.mangle(names.instanceNameOf(tpeCase)))
      res <- apply(factory, args)
    } yield res
  }
}


object CoCoClean {
  type WithParadigm[P <: AnyParadigm] = CoCoClean {val paradigm: P}
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
    (base: P)
      (
        nameProvider: NameProvider[base.syntax.Name],
        oo: ObjectOriented.WithBase[base.type],
        params: ParametricPolymorphism.WithBase[base.type]
      )
      (generics: Generics.WithBase[base.type, oo.type, params.type]): CoCoClean.WithParadigm[base.type] =
    new CoCoClean {
      val paradigm: base.type = base
      val ooParadigm: oo.type = oo
      val polymorphics: params.type = params
      val genericsParadigm: generics.type = generics
      val names: NameProvider[paradigm.syntax.Name] = nameProvider
    }
}
