package org.combinators.ep.approach.oo  /*DI:LI:AD*/

import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, Operation, TestCase, TypeRep}
import org.combinators.ep.domain.{GenericModel, abstractions}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.{AbstractSyntax, ApproachImplementationProvider, Command, EvolutionImplementationProvider, NameProvider, TestImplementationProvider, Understands, communication}
import org.combinators.ep.generator.paradigm.{AddImport, AnyParadigm, Apply, FindClass, Generics, ObjectOriented, ParametricPolymorphism, ResolveImport, AddTypeLookup}

import scala.tools.nsc.interpreter.NamedParamClass

// j7 mistakenly generates lit/multBy and lit/powBy
// might be in newDataTypeCasesWithNewOperations...
trait CoCoClean extends ApproachImplementationProvider {
  val paradigm: AnyParadigm
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]
  val names: NameProvider[paradigm.syntax.Name]

  object ComponentNames {
    val factory: paradigm.syntax.Name = names.mangle("Factory")
    val convertMethod: paradigm.syntax.Name = names.mangle("convert")
    val convertMethodParameter: paradigm.syntax.Name = names.mangle("toConvert")
    val finalizedTypeParameter: paradigm.syntax.Name = names.mangle("FT")
    val finalizedPackage: paradigm.syntax.Name = names.mangle("finalized")
    val getSelfMethod: paradigm.syntax.Name = names.mangle("getSelf")

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

  /**
   * Critical aspect of CoCo is that the Extended Intermediate Interface (i.e., ep.m3.Exp) is only created when
   * needed, specifically: (a) a new operation is being defined, and this interface will host the default
   * implementation; or (b) a branch is being merged from branches in which new Exp had been defined
   * useful when determining merging.
   *
   * Also found in Interpreter
   *
   * @param domain     typically last domain for which the ancestor is sought
   */
  def ancestorsDefiningNewTypeInterfaces(domain: GenericModel): Set[GenericModel] = {
    val ancestorsWithNewTypeInterfaces = domain.former.map(ancestor => latestModelDefiningNewTypeInterface(ancestor))
    ancestorsWithNewTypeInterfaces.distinct.filterNot { ancestor =>
      // get rid of everything that has an antecedent
      ancestorsWithNewTypeInterfaces.exists(otherAncestor => ancestor.before(otherAncestor))
    }.toSet
  }

  /**
   * Base domain always needs. With new data types, we need a new factory. If current domain defines a new type interface, then
   * we need to define factory.
   *
   * Otherwise if merge case and have more than one ancestor that defines type interface then definitely need a factory
   */
  def latestModelDefiningNewFactoryType(domain: GenericModel): GenericModel = {
    if (domain.isDomainBase || domain.typeCases.nonEmpty || domain == latestModelDefiningNewTypeInterface(domain)) {
      domain
    } else {
      // is there a single type that can represent the "least upper bound" of all prior branches.
      val ancestorsWithFactoryTypes = domain.former.map(ancestor => latestModelDefiningNewFactoryType(ancestor)).distinct
      // To validate this works, need multiple branches where NEITHER defines operators
      if (ancestorsWithFactoryTypes.size == 1 && !ancestorsWithFactoryTypes.head.isDomainBase) { // take care to avoid falling below "floor"
        ancestorsWithFactoryTypes.head
      } else {
        domain
      }
    }
  }

  /**
   * When there is a new operation, definitely need a new Exp interface.
   *
   * Merge case handed here.
   *
   * Also duplicated in Interpreter with modifications
   */
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

  /**
   * Could have a merge case where both branches only add data types, and so the subsequent merge has no EIP.
   *
   * Could have a merge case where both branches only add operations, and so the subsequent merge has no EIP.
   *
   * Could have a merge case where both branches only have EIP optimizations, and the subsequent merge has no EIP.
   *   (a) could be the case that both optimizations involve the same op/datatype case
   *   (b) could be independent
   *
   * Check if all ops and data types present at the current point are present in all formers. If this is not the
   * case then structurally there is a need for a Factory
   */
  def addNewFactoryInterfaceIfNecessary(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
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

    if (domain != latestModelDefiningNewFactoryType(domain)) {
      Command.skip
    } else {
      import ooParadigm.projectCapabilities._
      addClassToProject(makeNewFactoryInterface(), names.mangle(names.instanceNameOf(domain)), ComponentNames.factory)
    }
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

  /**
   * When a new Exp is necessary, there must be a factory.
   *
   * When a new DT is defined or an Operation then you
   */
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
  def newDataTypeCasesWithNewOperations(domain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Map[DataTypeCase, Set[Operation]] = {
    val flatDomain = domain.flatten
    val allDataTypeCases = flatDomain.typeCases.toSet
    val allOperations = flatDomain.ops.toSet
    
    allDataTypeCases.foldLeft(Map.empty[DataTypeCase, Set[Operation]]) { (resultMap, tpeCase) =>
      // Remembers all operations that are already supported
      val presentOperations = domain.operationsPresentEarlier(tpeCase)

      val overwrittenOperations = allOperations.filter { operation =>
        // Does our current domain contain an override implementation?
        domainSpecific.evolutionSpecificDependencies(
          PotentialRequest(domain.baseDataType, tpeCase, operation)
        ).contains(domain)
      }
      val updatedOperations = (allOperations -- presentOperations) ++ overwrittenOperations
      // If we have any updated operations, if we have a former one that doesn't support the current type case, or if we are in a merge
      if (updatedOperations.nonEmpty || domain.former.exists(ancestor => !ancestor.supports(tpeCase)) || domain.former.size > 1) {
        resultMap.updated(tpeCase, updatedOperations)
      } else {
        resultMap
      }
    }
  }

  def latestModelDefiningDataTypeCaseInterface(domain: GenericModel, dataTypeCase: DataTypeCase, domainSpecific: EvolutionImplementationProvider[this.type]): Option[GenericModel] = {
    // Either this is the current domain (new Exp interface, then we know dataTypeCase is being redeclared anyway because of new operation); or
    // it is a prior domain (using a former Exp interface, we are currently freshly declaring the data type case interface either because our
    // branch has never seen this data type case OR if it has seen it, since ancestor had added data type and we need to find latest point where
    // it was seen)

    // cannot skip over intervening models that have EIP overridden for this dataTypeCase on any operation
    val m = newDataTypeCasesWithNewOperations(domain, domainSpecific)

    if (m.contains(dataTypeCase)) {
      // then we are in charge
      Some(domain)
    } else {
      val latestModelsForBranches = domain.former.flatMap(gm => latestModelDefiningDataTypeCaseInterface(gm, dataTypeCase, domainSpecific).toSeq).distinct
      if (latestModelsForBranches.size == 1 && !latestModelsForBranches.head.isDomainBase) {
        Some(latestModelsForBranches.head)
      } else {
        // If more than one ancestor doing this,
        None
      }
    }
  }

  def mostSpecificTypeCaseInterface[Context](domain: GenericModel, finalizedType: paradigm.syntax.Type, dataTypeCase:DataTypeCase, domainSpecific: EvolutionImplementationProvider[this.type])(implicit
          canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
          canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
          canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]],
          canApplyType: Understands[Context, Apply[paradigm.syntax.Type, paradigm.syntax.Type, paradigm.syntax.Type]],
  ): Generator[Context, Option[paradigm.syntax.Type]] = {
    val _latestModelDefiningDataTypeCaseInterface = latestModelDefiningDataTypeCaseInterface(domain, dataTypeCase, domainSpecific)

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
      domainSpecific: EvolutionImplementationProvider[this.type],
      domain: GenericModel,
      finalizedType: paradigm.syntax.Type,
      dataTypeCase: DataTypeCase,
      operation: Operation
    ): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      _ <- setOperationMethodSignature(domain, finalizedType, operation)
      _ <- if (domain.operationsPresentEarlier(dataTypeCase).contains(operation)) {
          setOverride()
        } else {
          Command.skip[paradigm.MethodBodyContext]
        }
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
          ),
          model = Some(domain)
        )
      result <- domainSpecific.logic(this)(receivedRequest)
    } yield result
  }

  def addDataTypeCaseInterfaces(domain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    val _newDataTypeCasesWithNewOperations = newDataTypeCasesWithNewOperations(domain, domainSpecific)

    def makeNewTypeCaseInterface(newDataTypeCase:DataTypeCase, newOperations:Set[Operation]) : Generator[ooParadigm.ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._

      // all interfaces to add. Likely there is a better way to implement this...
      val added:scala.collection.mutable.Set[paradigm.syntax.Type] = scala.collection.mutable.Set[paradigm.syntax.Type]()

      for {
        // Add type parameter for the finalized type
        _ <- addTypeParameter(ComponentNames.finalizedTypeParameter, Command.skip)
        finalizedType <- getTypeArguments().map(tpeArgs => tpeArgs.head)

        // Set parent data type interface
        parentBaseInterfaceType <- mostSpecificBaseInterfaceType(domain, finalizedType)
        _ <- forEach(Seq(parentBaseInterfaceType)) {
            tpe => added.add(tpe)
            Command.skip[ooParadigm.ClassContext]
        }

        // Also inherit from latest factory if necessary because no new base interface was declared.
        // If these are identical, then we get stuff from the Exp (since it is always latest factory); if different we need to pull in.
        _ <- if (latestModelDefiningNewTypeInterface(domain) != latestModelDefiningNewFactoryType(domain)) {
          for {
            pfi <- appliedFactoryInterfaceType(latestModelDefiningNewFactoryType(domain), finalizedType)
            _ <- forEach(Seq(pfi)) {
              tpe => added.add(tpe)
                Command.skip[ooParadigm.ClassContext]
            }
          } yield pfi
        } else Command.skip[ooParadigm.ClassContext]

        // Inherit previous data type case implementations. Make sure not added twice...
        _ <- forEach(domain.former) { ancestor =>
          for {
             parentDataTypeCaseInterface <- mostSpecificTypeCaseInterface(ancestor, finalizedType, newDataTypeCase, domainSpecific)
            _ <- if (parentDataTypeCaseInterface.nonEmpty && !added.contains(parentDataTypeCaseInterface.get)) {
              added.add(parentDataTypeCaseInterface.get)
              Command.skip[ooParadigm.ClassContext]
            } else {
              Command.skip[ooParadigm.ClassContext]
            }
          } yield ()
        }

        // now that all parent interfaces are prepared, add them, so distinct and non-duplicated
        _ <- forEach(added.toSeq) { tpe =>
          for {
            _ <- addParent(tpe)
          } yield ()
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
              domainSpecific = domainSpecific,
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

  def finalizedBaseInterfaceType[Context](domain: GenericModel)(implicit
    canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
    canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
    canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]]
  ): Generator[Context, paradigm.syntax.Type] = {
    val _latestModelDefiningNewTypeInterface = latestModelDefiningNewTypeInterface(domain)
    for {
      finalizedBaseInterfaceType <-
        FindClass[paradigm.syntax.Name, paradigm.syntax.Type](Seq(
          names.mangle(names.instanceNameOf(_latestModelDefiningNewTypeInterface)),
          ComponentNames.finalizedPackage,
          names.mangle(names.conceptNameOf(domain.baseDataType)))
        ).interpret(canFindClass)
      _ <- resolveAndAddImport(finalizedBaseInterfaceType)(canResolveImport, canAddImport)
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

  def mostSpecificTypeCaseClass[Context](domain: GenericModel, finalizedType: paradigm.syntax.Type, dataTypeCase: DataTypeCase, domainSpecific: EvolutionImplementationProvider[this.type])(implicit
    canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
    canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
    canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]]
  ): Generator[Context, Option[paradigm.syntax.Type]] = {
    val _latestModelDefiningDataTypeCaseInterface = latestModelDefiningDataTypeCaseInterface(domain, dataTypeCase, domainSpecific)

    if (_latestModelDefiningDataTypeCaseInterface.isEmpty) {
      Command.lift[Context, Option[paradigm.syntax.Type]](Option.empty)
    } else {
      for {
        resultType <- FindClass[paradigm.syntax.Name, paradigm.syntax.Type](Seq(names.mangle(names.instanceNameOf(_latestModelDefiningDataTypeCaseInterface.get)), ComponentNames.finalizedPackage, names.mangle(names.conceptNameOf(dataTypeCase)))).interpret(canFindClass)
        _ <- resolveAndAddImport(resultType)
      } yield Some(resultType)
    }
  }

  def makeFinalizedFactoryMethod(domain: GenericModel, finalizedType: paradigm.syntax.Type, dataTypeCase: DataTypeCase, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      _ <- setFactoryMethodSignature(domain, finalizedType, dataTypeCase)
      attributeValues <- getArguments()
      _ <- if (!domain.typeCases.contains(dataTypeCase)) {
        setOverride()     // override when not defined herein
      } else {
        Command.skip[paradigm.MethodBodyContext]
      }
      typeToInstantiate <- mostSpecificTypeCaseClass(domain, finalizedType, dataTypeCase, domainSpecific)
      result <- instantiateObject(typeToInstantiate.get, attributeValues.map(_._3))
    } yield Some(result)
  }

  def makeFinalizedConvertMethod(domain: GenericModel, finalizedType: paradigm.syntax.Type): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      _ <- setConvertMethodSignature(domain, finalizedType)
      _ <- setOverride()
      arguments <- getArguments()
      toConvert = arguments.head._3
      getSelfMethod <- getMember(toConvert, ComponentNames.getSelfMethod)
      result <- apply(getSelfMethod, Seq.empty)
    } yield Some(result)
  }

  def addFinalizedFactoryInterface(domain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    def makeNewFinalizedFactoryInterface(): Generator[ooParadigm.ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._
      val _latestModelDefiningNewTypeInterface = latestModelDefiningNewTypeInterface(domain)
      val latestFactory = latestModelDefiningNewFactoryType(domain)
      val currentDomainDefinesNewTypeInterface = _latestModelDefiningNewTypeInterface == domain
      for {
        finalizedType <- finalizedBaseInterfaceType(domain)

        // Set parent factory types
        nonFinalizedParentFactoryType <- appliedFactoryInterfaceType(latestFactory, finalizedType)
        _ <- addParent(nonFinalizedParentFactoryType)

        _ <- if (!currentDomainDefinesNewTypeInterface) {
          forEach(domain.former) { ancestor =>
            for {
              parentFactoryType <- finalizedFactoryType(ancestor)
              _ <- addParent(parentFactoryType)
            } yield ()
          }
        } else Command.skip[ooParadigm.ClassContext]

        // Add factory methods BUT ONLY for type cases that need it. Ordinarily
        _ <- forEach(domain.flatten.typeCases.distinct.filter(dataTypeCase => latestModelDefiningDataTypeCaseInterface(domain, dataTypeCase, domainSpecific).contains(domain))) { tpeCase =>
          addMethod(names.mangle(names.instanceNameOf(tpeCase)), makeFinalizedFactoryMethod(domain, finalizedType, tpeCase, domainSpecific))
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

  def addFinalizedTypeCaseClasses(domain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    val _newDataTypeCasesWithNewOperations = newDataTypeCasesWithNewOperations(domain, domainSpecific)

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
        nonFinalizedDataTypeCaseInterface <- mostSpecificTypeCaseInterface(domain, finalizedType, newDataTypeCase, domainSpecific)
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
          _ <- addNewFactoryInterfaceIfNecessary(domain)
          _ <- addNewTypeInterfaceIfNecessary(domain)
          _ <- addDataTypeCaseInterfaces(domain, domainSpecific)
          _ <- addFinalizedFactoryInterface(domain, domainSpecific)
          _ <- addFinalizedTypeInterfaceIfNecessary(domain)
          _ <- addFinalizedTypeCaseClasses(domain, domainSpecific)
          _ <- forEach(domain.former) { ancestor => implementRecursive(ancestor) }
        } yield ()
      }
    }
    for {
      _ <- domainSpecific.initialize(this)
      _ <- implementRecursive(domain)
    } yield ()
  }

  def registerFinalizedTestTypes[Context](model: GenericModel)(implicit
    canAddTypeLookupForMethods: Understands[Context, AddTypeLookup[paradigm.MethodBodyContext, paradigm.syntax.Type]],
    canAddTypeLookupForClasses: Understands[Context, AddTypeLookup[ooParadigm.ClassContext, paradigm.syntax.Type]],
    canAddTypeLookupForConstructors: Understands[Context, AddTypeLookup[ooParadigm.ConstructorContext, paradigm.syntax.Type]],
  ): Generator[Context, Unit] = {   
    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._
    import genericsParadigm.constructorCapabilities._
    import polymorphics.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    import ooParadigm.constructorCapabilities._
    import ooParadigm.methodBodyCapabilities._
    val dtpeRep = TypeRep.DataType(model.baseDataType)

    def baseInterfaceType[Context](implicit
      canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
      canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
      canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]],
      canApplyType: Understands[Context, Apply[paradigm.syntax.Type, paradigm.syntax.Type, paradigm.syntax.Type]]
    ): Generator[Context, paradigm.syntax.Type] = {
      for {
        finalizedBaseInterfaceType <- finalizedBaseInterfaceType(model)(canFindClass, canResolveImport, canAddImport)
        appliedBaseInterface <- leastSpecialBaseInterfaceType(model, finalizedBaseInterfaceType)(canFindClass, canResolveImport, canAddImport, canApplyType)
        _ <- resolveAndAddImport(appliedBaseInterface)(canResolveImport,canAddImport)
      } yield appliedBaseInterface
    }

    for {
      _ <- AddTypeLookup[paradigm.MethodBodyContext, paradigm.syntax.Type](dtpeRep, baseInterfaceType[paradigm.MethodBodyContext]).interpret(canAddTypeLookupForMethods)
      _ <- AddTypeLookup[ooParadigm.ClassContext, paradigm.syntax.Type](dtpeRep, baseInterfaceType[ooParadigm.ClassContext]).interpret(canAddTypeLookupForClasses)
      _ <- AddTypeLookup[ooParadigm.ConstructorContext, paradigm.syntax.Type](dtpeRep, baseInterfaceType[ooParadigm.ConstructorContext]).interpret(canAddTypeLookupForConstructors)
    } yield ()
  }

  override def implement(tests: Map[GenericModel, Seq[TestCase]], testImplementationProvider: TestImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    import paradigm.projectCapabilities._
    import paradigm.compilationUnitCapabilities._
    import paradigm.testCapabilities._
    import ooParadigm.projectCapabilities._

    for {
      _ <- forEach(tests.toList) { case (model, tests) =>
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
          _ <- registerFinalizedTestTypes(model)
          _ <- addCompilationUnit(
            testSuite,
            testCaseName(model)
          )
        } yield None
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
