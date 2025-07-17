package org.combinators.ep.approach.oo

/*DI:LI:AD*/

import org.combinators.cogen.{TestCase, TypeRep, Command, Understands, NameProvider, AbstractSyntax}
import Command.Generator
import org.combinators.cogen.paradigm.{AddImport, AddTypeLookup, AnyParadigm, Apply, FindClass, Generics, ObjectOriented, ParametricPolymorphism, ResolveImport}
import org.combinators.ep.domain.abstractions.{DataTypeCase, Operation, Parameter, DomainTpeRep}
import org.combinators.ep.domain.{GenericModel, abstractions}
import org.combinators.ep.domain.extensions._
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request}
import org.combinators.cogen.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator._

/**
  * Sufficiently different EP approach that this trait does not extend SharedOO
  */
trait ObjectAlgebras extends ApproachImplementationProvider {
  val paradigm: AnyParadigm
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]
  val names: NameProvider[paradigm.syntax.Name]

  object ComponentNames {
    val algebra: paradigm.syntax.Name = names.mangle("Algebra")
    val algebraAtt: paradigm.syntax.Name = names.mangle("algebra")

    val signature: paradigm.syntax.Name = names.mangle("Signature")
    val carrier: paradigm.syntax.Name = names.mangle("Carrier")
    val returnTypeParameter: paradigm.syntax.Name = names.mangle("C")

    val pkgAlgebra: paradigm.syntax.Name = names.mangle("algebra")
    val pkgCarrier: paradigm.syntax.Name = names.mangle("carrier")
    val pkgInstance: paradigm.syntax.Name = names.mangle("instance")

    val getSelf: paradigm.syntax.Name = names.mangle("getSelf")

    val value: paradigm.syntax.Name = names.mangle("value")
    val inner: paradigm.syntax.Name = names.mangle("inner")

    def constructor(tpeCase: abstractions.DataTypeCase): paradigm.syntax.Name = {
      names.mangle(names.instanceNameOf(tpeCase))
    }

    def carrierInstance(op: Operation): paradigm.syntax.Name = {
      names.mangle(names.instanceNameOf(op) + "Carrier")
    }
  }

  def dispatch(message: communication.SendRequest[paradigm.syntax.Expression]): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      method <- getMember(message.to, names.mangle(names.instanceNameOf(message.request.op)))
      result <- apply(method, message.request.op.parameters.map(message.request.arguments))
    } yield result
  }

  def instantiate(baseTpe: abstractions.DataType, tpeCase: abstractions.DataTypeCase, args: paradigm.syntax.Expression*): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      thisRef <- selfReference()
      algebra <- getMember(thisRef, ComponentNames.algebraAtt)
      tpeMethod <- getMember(algebra, names.mangle(names.instanceNameOf(tpeCase)))
      res <- apply(tpeMethod, args)
    } yield res
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

  // find carrier to use. Note that M3 goes back to M2 (likely because that is when last operation was defined)
  // Case 1: If a new operation always add a new carrier interface; or
  // Case 2: If you merge together and there is a new operation from the merging
  def carrierToUse(domain: GenericModel): GenericModel = {
    if (domain.ops.nonEmpty) {
      domain
    } else {
      val ancestorsWithTypeInterfaces = domain.former.map(ancestor => carrierToUse(ancestor)).distinct
      // To validate this works, need multiple branches where NEITHER defines operators
      if (ancestorsWithTypeInterfaces.size == 1 && !ancestorsWithTypeInterfaces.head.isDomainBase) { // take care to avoid falling below "floor"
        ancestorsWithTypeInterfaces.head
      } else {
        domain
      }
    }
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
    import paradigm.methodBodyCapabilities._
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

  def setOperationMethodSignature(domain: GenericModel, op: Operation): Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
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
    import paradigm.methodBodyCapabilities._
    for {
      returnType <- toTargetLanguageType(DomainTpeRep.DataType(domain.baseDataType))
      _ <- setReturnType(returnType)
    } yield ()
  }

  def addOperationCarrierInterface(domain: GenericModel, op: Operation): Generator[paradigm.ProjectContext, Unit] = {
    //    public interface PowBy<C> {
    //      C getSelf();
    //      C powBy(C other);
    //    }
    def makeOperationCarrierInterface(): Generator[ooParadigm.ClassContext, Unit] = {
      import genericsParadigm.classCapabilities._
      import ooParadigm.classCapabilities._
      for {
        _ <- setInterface()

        // Add type parameter for the finalized type
        _ <- addTypeParameter(ComponentNames.carrier, Command.skip)
        _ <- registerTypeMapping(domain) // can only have AFTER declared type parameter
        _ <- addAbstractMethod(ComponentNames.getSelf, getSelfSignature(domain))

        _ <- addAbstractMethod(names.mangle(names.instanceNameOf(op)), setOperationMethodSignature(domain, op))
      } yield ()
    }
    import ooParadigm.projectCapabilities._
    addClassToProject(makeOperationCarrierInterface(), names.mangle(names.instanceNameOf(domain)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(op)))
  }


  def addCombinedCarrierInstance(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
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

      def makeOperationImpl(op: Operation): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
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
          carrierInstance <- getMember(self, ComponentNames.carrierInstance(op))
          method <- getMember(carrierInstance, names.mangle(names.instanceNameOf(op)))
          args <- getArguments()
          result <- apply(method, args.map(_._3))
        } yield Some(result)
      }

      def getSelfMethod(interfaceType: paradigm.syntax.Type): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ooParadigm.methodBodyCapabilities._
        import paradigm.methodBodyCapabilities._
        for {
          _ <- setReturnType(interfaceType)
          self <- selfReference()
        } yield Some(self)
      }

      def makeOperationConstructor(interfaceType: paradigm.syntax.Type): Generator[ooParadigm.ConstructorContext, Unit] = {
        import ooParadigm.constructorCapabilities._
        import genericsParadigm.constructorCapabilities._
        for {
          parameters <- forEach(domain.flatten.ops) { op =>
            for {
              // must be where operation is defined. NOTE: It must be there because otherwise we wouldn't be called.
              tpe <- findClass(names.mangle(names.instanceNameOf(domain.findOperation(op).get)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(op)))
              _ <- resolveAndAddImport(tpe)
              finalTpe <- applyType(tpe, Seq(interfaceType))
              name <- freshName(names.mangle(names.instanceNameOf(op)))
            } yield (name, finalTpe)
          }

          _ <- setParameters(parameters)
          arguments <- getArguments()
          _ <- forEach(arguments.zip(domain.flatten.ops.map(op => ComponentNames.carrierInstance(op)))) { case (arg, att) =>
            for {
              _ <- initializeField(att, arg._3)
            } yield ()
          }
        } yield ()
      }

      import ooParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._
      for {
        _ <- registerInstanceTypeMapping(domain) // CRITICAL to do before generating any subsequent artifacts

        interfaceType <- findClass(names.mangle(names.instanceNameOf(domain)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(domain)))
        _ <- resolveAndAddImport(interfaceType)
        _ <- addImplemented(interfaceType)

        _ <- forEach(domain.flatten.ops) { op =>
          for {
            // Note: Must find the class where op was defined. MUST be able to .get() result
            tpe <- findClass(names.mangle(names.instanceNameOf(domain.findOperation(op).get)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(op)))
            _ <- resolveAndAddImport(tpe)
            finalTpe <- applyType(tpe, Seq(interfaceType))

            _ <- addField(ComponentNames.carrierInstance(op), finalTpe)
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

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def addOperationCarrierInstance(domain: GenericModel, op: Operation): Generator[paradigm.ProjectContext, Unit] = {
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

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def addAlgebraOperation(domain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type], op: Operation): Generator[paradigm.ProjectContext, Unit] = {
    //    public final class Eval<C extends m0.carrier.Eval<C>> implements Signature<m0.carrier.Eval<C>> {
    //      private final m0.Signature<C> inner;
    //
    //      public Eval(Signature<C> inner) {
    //          this.inner = inner;
    //      }
    //
    //    @Override public m0.carrier.Eval<C> lit(double value) {
    //      return new m0.carrier.instance.eval.Lit<C> (inner, value);
    //    }
    //
    //    @Override public m0.carrier.Eval<C> add(m0.carrier.Eval<C> left, m0.carrier.Eval<C> right) {
    //      return new m0.carrier.instance.eval.Add<C> (inner, left.getSelf(), right.getSelf());
    //    }
    //    }
    def setMethodSignature(tpe: DataTypeCase): Generator[paradigm.MethodBodyContext, Unit] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import polymorphics.methodBodyCapabilities._
      for {
        baseType <- toTargetLanguageType(DomainTpeRep.DataType(domain.baseDataType))
        signature <- forEach(tpe.attributes) { att => toTargetLanguageType(att.tpe) }
        _ <- setParameters(tpe.attributes.map(att => names.mangle(att.name)).zip(signature))
        _ <- setReturnType(baseType)
      } yield ()
    }

    def newOpMethod(tpe: DataTypeCase, carrierType: paradigm.syntax.Type): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import polymorphics.methodBodyCapabilities._
      val newOperations = dataTypeCasesWithNewOperations(domainSpecific, domain).getOrElse(tpe, Set.empty)
      val possible = if (newOperations.contains(op)) {
        domain
      } else {
        domainSpecific.evolutionSpecificDependencies(PotentialRequest(domain.baseDataType, tpe, op)).keySet.max((l: GenericModel, r: GenericModel) => {
          if (l.before(r)) -1 else if (r.before(l)) 1 else 0
        })
      }

      // CANNOT go earlier than when operation is defined.
      val latest = possible.later(domain.findOperation(op).get)
      for {
        _ <- setMethodSignature(tpe)

        attributeValues <- getArguments()
        selfRef <- selfReference()
        inner <- getMember(selfRef, ComponentNames.inner)

        result <- if (latest == domain) { // newly defined....or new operation?
          for {
            inst <- findClass(names.mangle(names.instanceNameOf(latest)), ComponentNames.pkgCarrier, ComponentNames.pkgInstance,
              names.mangle(names.instanceNameOf(op)), names.mangle(names.conceptNameOf(tpe)))
            _ <- resolveAndAddImport(inst)
            instGeneric <- applyType(inst, Seq(carrierType))
            // when accessing ANEW, since you have to construct
            processedAtts <- forEach(tpe.attributes.zip(attributeValues)) { case (att, attv) =>
              for {
                pt <- if (att.tpe == DomainTpeRep.DataType(domain.baseDataType)) {
                  for {
                    getSelfMethod <- getMember(attv._3, ComponentNames.getSelf)
                    result <- apply(getSelfMethod, Seq.empty)
                  } yield result
                } else {
                  Command.lift[paradigm.MethodBodyContext, paradigm.syntax.Expression](attv._3)
                }
              } yield pt

            }
            result <- instantiateObject(instGeneric, inner +: processedAtts)
          } yield result
        } else {
          // must exist since we didn't settle on domain
          val defined = domain.former.collectFirst { case dom if dom.supports(op) && dom.supports(tpe) =>
            latestDomainWithAlgebraOperation(dom, domainSpecific, op)
          }.get
          for {

            selfRef <- selfReference()
            factoryAtt <- getMember(selfRef, names.mangle(names.instanceNameOf(defined)))

            // when accessing ANEW, since you have to construct
            processedAtts <- forEach(tpe.attributes.zip(attributeValues)) { case (att, attv) =>
              for {
                pt <- if (att.tpe == DomainTpeRep.DataType(domain.baseDataType)) {
                  for {
                    getSelfMethod <- getMember(attv._3, ComponentNames.getSelf)
                    result <- apply(getSelfMethod, Seq.empty)
                  } yield result
                } else {
                  Command.lift[paradigm.MethodBodyContext, paradigm.syntax.Expression](attv._3)
                }
              } yield pt
            }
            factoryMethod <- getMember(factoryAtt, names.mangle(names.instanceNameOf(tpe)))
            result <- apply(factoryMethod, processedAtts)
          } yield result

        }
      } yield Some(result)
    }

    def makeAlgebraConstructor(signatureTpe: paradigm.syntax.Type, carrierTpe: paradigm.syntax.Type): Generator[ooParadigm.ConstructorContext, Unit] = {
      import ooParadigm.constructorCapabilities._
      import genericsParadigm.constructorCapabilities._

      for {
        paramName <- freshName(ComponentNames.inner)
        _ <- setParameters(Seq((paramName, signatureTpe)))
        arguments <- getArguments()
        _ <- forEach(arguments.zip(Seq(ComponentNames.inner))) { case (arg, att) =>
          for {
            _ <- initializeField(att, arg._3)
          } yield ()
        }

        _ <- forEach(domain.former.collect { case dom if dom.supports(op) => latestDomainWithAlgebraOperation(dom, domainSpecific, op) }.distinct) {
          dom => {
            for {
              _ <- if (!dom.equals(domain)) { // only if not the same
                for {
                  // m3.algebra.PrettyPrint
                  clazz <- findClass(names.mangle(names.instanceNameOf(dom)), ComponentNames.pkgAlgebra, names.mangle(names.conceptNameOf(op)))
                  _ <- resolveAndAddImport(clazz)

                  appliedClazz <- applyType(clazz, Seq(carrierTpe))
                  thisRef <- selfReference()
                  innerRef <- getMember(thisRef, ComponentNames.inner)
                  expr <- instantiateObject(appliedClazz, Seq(innerRef))
                  _ <- initializeField(names.mangle(names.instanceNameOf(dom)), expr)
                } yield ()
              } else {
                Command.skip[ooParadigm.ConstructorContext]
              }
            } yield ()
          }
        }
      } yield ()
    }

    def makeAlgebraOperation(): Generator[ooParadigm.ClassContext, Unit] = {
      import genericsParadigm.classCapabilities._
      import ooParadigm.classCapabilities._

      for {
        // make sure to grab appropriate carrier interface from where op is defined
        interfaceType <- findClass(names.mangle(names.instanceNameOf(domain.findOperation(op).get)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(op)))
        carrierTypeParam <- freshName(ComponentNames.returnTypeParameter)

        // Needs to add all prior dependencies, because the algebra operation pulls together all previous carriers with
        // all constraints on dependent operations.
        diTypes <- forEach(domain.flatten.typeCases.distinct.flatMap(tpeCase =>
          domainSpecific.dependencies(PotentialRequest(domain.baseDataType, tpeCase, op)).getOrElse(Set.empty)
        ).distinct) { dop => {
          for {
            ditype <- findClass(names.mangle(names.instanceNameOf(domain.findOperation(dop).get)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(dop)))
            _ <- resolveAndAddImport(ditype)
          } yield ditype
        }
        }

        _ <- addTypeParameter(carrierTypeParam, makeTypeParameter(interfaceType, diTypes))
        tpeParams <- getTypeArguments()

        signatureTpe <- findClass(names.mangle(names.instanceNameOf(domain)), ComponentNames.signature)
        interfaceTpeGeneric <- applyType(interfaceType, Seq(tpeParams.head))
        signatureTpeGeneric <- applyType(signatureTpe, Seq(interfaceTpeGeneric))
        _ <- resolveAndAddImport(signatureTpe)
        _ <- addImplemented(signatureTpeGeneric)

        _ <- registerInstanceTypeMappingOperation(domain, tpeParams, op)

        signatureTpeCarrierGeneric <- applyType(signatureTpe, Seq(tpeParams.head))
        _ <- addField(ComponentNames.inner, signatureTpeCarrierGeneric)

        // if in MERGE case, need to add oldNNN for each operation that was defined on a previous branch, we need
        // oldNNN to record that branch
        _ <- forEach(domain.former.collect { case dom if dom.supports(op) => latestDomainWithAlgebraOperation(dom, domainSpecific, op) }.distinct) {
          dom => {
            for {
              // m3.algebra.PrettyPrint
              clazz <- findClass(names.mangle(names.instanceNameOf(dom)), ComponentNames.pkgAlgebra, names.mangle(names.conceptNameOf(op)))
              _ <- resolveAndAddImport(clazz)
              appliedClazz <- applyType(clazz, Seq(tpeParams.head))
              _ <- addField(names.mangle(names.instanceNameOf(dom)), appliedClazz)
            } yield ()
          }
        }

        _ <- addConstructor(makeAlgebraConstructor(signatureTpeCarrierGeneric, tpeParams.head))

        _ <- forEach(domain.flatten.typeCases) { tpe =>
          for {
            _ <- addMethod(names.mangle(names.instanceNameOf(tpe)), newOpMethod(tpe, tpeParams.head), isPublic = true)
          } yield ()
        }

      } yield ()
    }

    if (domain.findOperation(op).isDefined) {
      import ooParadigm.projectCapabilities._
      addClassToProject(makeAlgebraOperation(), names.mangle(names.instanceNameOf(domain)), ComponentNames.pkgAlgebra, names.mangle(names.conceptNameOf(op)))
    } else {
      Command.skip[paradigm.ProjectContext]
    }
  }

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def addCombinedAlgebra(domain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    //    public class M0 implements Signature<m0.carrier.M0> {
    //      private final Eval<m0.carrier.M0> eval = new Eval<m0.carrier.M0>(this);
    //
    //      @Override public m0.carrier.M0 lit(double value) {
    //        return new m0.carrier.instance.M0(eval.lit(value));
    //      }
    //
    //      @Override public m0.carrier.M0 add(m0.carrier.M0 left, m0.carrier.M0 right) {
    //        return new m0.carrier.instance.M0(eval.add(left, right));
    //      }
    //    }

    def setMethodSignature(tpe: DataTypeCase): Generator[paradigm.MethodBodyContext, Unit] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import polymorphics.methodBodyCapabilities._
      val latest = carrierToUse(domain) // latestModelDefiningNewTypeInterface

      for {
        carrierInstanceType <- findClass(names.mangle(names.instanceNameOf(latest)),
          ComponentNames.pkgAlgebra, ComponentNames.pkgInstance, names.mangle(names.conceptNameOf(latest)))
        _ <- resolveAndAddImport(carrierInstanceType)
        carrierType <- findClass(names.mangle(names.instanceNameOf(latest)),
          ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(latest)))
        _ <- resolveAndAddImport(carrierType)

        signature <- forEach(tpe.attributes) { att =>
          if (att.tpe.isModelBase(latest)) { // operhaps domain still?
            Command.lift[paradigm.MethodBodyContext, paradigm.syntax.Type](carrierType)
          } else {
            for {
              signatureType <- toTargetLanguageType(att.tpe)
              _ <- resolveAndAddImport(signatureType)
            } yield signatureType
          }
        }
        _ <- setParameters(tpe.attributes.map(att => names.mangle(att.name)).zip(signature))
        _ <- setReturnType(carrierType)
      } yield ()
    }

    def newOpMethod(tpe: DataTypeCase): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import polymorphics.methodBodyCapabilities._

      for {
        _ <- setMethodSignature(tpe)
        latest = latestModelDefiningNewTypeInterface(domain)
        inst <- findClass(names.mangle(names.instanceNameOf(latest)), ComponentNames.pkgCarrier, ComponentNames.pkgInstance,
          names.mangle(names.conceptNameOf(latest)))
        _ <- resolveAndAddImport(inst)

        attributeValues <- getArguments()
        selfRef <- selfReference()
        processedAtts <- forEach(tpe.attributes.zip(attributeValues)) { case (att, attv) =>
          Command.lift[paradigm.MethodBodyContext, paradigm.syntax.Expression](attv._3)
        }

        args <- forEach(domain.flatten.ops) { op =>
          for {
            opMember <- getMember(selfRef, names.mangle(names.instanceNameOf(op)))
            tpeMemberMethod <- getMember(opMember, names.mangle(names.instanceNameOf(tpe)))
            partial <- apply(tpeMemberMethod, processedAtts)
          } yield partial
        }

        result <- instantiateObject(inst, args)

      } yield Some(result)
    }

    // instead of auto-initializing values for fields, do so here.
    def makeAlgebraConstructor(): Generator[ooParadigm.ConstructorContext, Unit] = {
      import ooParadigm.constructorCapabilities._
      import genericsParadigm.constructorCapabilities._
      for {
        _ <- setParameters(Seq())
        _ <- forEach(domain.flatten.ops) { op =>

          // Use DomainSpecific to determine those operations that are newly created.
          val defined = carrierToUse(domain)

          for {
            opType <- findClass(names.mangle(names.instanceNameOf(latestDomainWithAlgebraOperation(domain, domainSpecific, op))), ComponentNames.pkgAlgebra, names.mangle(names.conceptNameOf(op)))
            _ <- resolveAndAddImport(opType)
            carrierType <- findClass(names.mangle(names.instanceNameOf(defined)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(defined)))
            _ <- resolveAndAddImport(carrierType)
            appliedType <- applyType(opType, Seq(carrierType))

            thisRef <- selfReference()
            result <- instantiateObject(appliedType, Seq(thisRef)) // should be appliedType (not opType)
            _ <- initializeField(names.mangle(names.instanceNameOf(op)), result)
          } yield ()
        }
      } yield ()
    }

    def makeCombinedAlgebra(): Generator[ooParadigm.ClassContext, Unit] = {
      import genericsParadigm.classCapabilities._
      import ooParadigm.classCapabilities._
      val pastOps = domain.former.foldLeft(Seq[Operation]()) {
        case (accumulatedOps, gm) => (accumulatedOps ++ gm.ops).distinct
      }
      val latestCarrierDomain = carrierToUse(domain)
      for {
        // make sure to grab appropriate carrier interface from where op is defined
        interfaceType <- findClass(names.mangle(names.instanceNameOf(latestCarrierDomain)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(latestCarrierDomain)))

        signatureTpe <- findClass(names.mangle(names.instanceNameOf(domain)), ComponentNames.signature)
        signatureTpeGeneric <- applyType(signatureTpe, Seq(interfaceType))
        _ <- resolveAndAddImport(interfaceType)
        _ <- resolveAndAddImport(signatureTpe)
        _ <- addImplemented(signatureTpeGeneric)

        _ <- forEach(domain.flatten.ops) { op =>
          val carrier = carrierToUse(domain)
          for {
            carrierType <- findClass(names.mangle(names.instanceNameOf(carrier)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(carrier)))

            opType <- findClass(names.mangle(names.instanceNameOf(latestDomainWithAlgebraOperation(domain, domainSpecific, op))), ComponentNames.pkgAlgebra, names.mangle(names.conceptNameOf(op)))
            _ <- resolveAndAddImport(opType)
            appliedType <- applyType(opType, Seq(carrierType))
            _ <- addField(names.mangle(names.instanceNameOf(op)), appliedType)
          } yield ()
        }

        _ <- addConstructor(makeAlgebraConstructor())

        _ <- forEach(domain.flatten.typeCases) { tpe =>
          for {
            _ <- addMethod(names.mangle(names.instanceNameOf(tpe)), newOpMethod(tpe), isPublic = true)
          } yield ()
        }

      } yield ()
    }
    import ooParadigm.projectCapabilities._
    addClassToProject(makeCombinedAlgebra(), names.mangle(names.instanceNameOf(domain)), ComponentNames.pkgAlgebra, names.mangle(names.conceptNameOf(domain)))
  }


  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def addSignatureInterface(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
    //  public interface Signature<Carrier> {
    //    Carrier lit(double value);
    //    Carrier add(Carrier left, Carrier right);
    //  }
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

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////
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

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def addCarrierModel(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
    def makeNewCarrierModel(): Generator[ooParadigm.ClassContext, Unit] = {
      import genericsParadigm.classCapabilities._
      import ooParadigm.classCapabilities._
      for {
        _ <- setInterface()

        tpeParam <- findClass(names.mangle(names.instanceNameOf(domain)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(domain)))
        _ <- resolveAndAddImport(tpeParam)

        _ <- forEach(domain.flatten.ops) { op =>
          val location = domain.findOperation(op).get //must always exist

          for {
            carrierType <- findClass(names.mangle(names.instanceNameOf(location)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(op)))
            _ <- resolveAndAddImport(carrierType)
            fullCarrierType <- applyType(carrierType, Seq(tpeParam))
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
  def dataTypeCasesWithNewOperations(evolutionImplementationProvider: EvolutionImplementationProvider[this.type], domain: GenericModel): Map[DataTypeCase, Set[Operation]] = {
    val flatDomain = domain.flatten
    val allDataTypeCases = flatDomain.typeCases.toSet
    val allOperations = flatDomain.ops.toSet

    allDataTypeCases.foldLeft(Map.empty[DataTypeCase, Set[Operation]]) { (resultMap, tpeCase) =>
      // Remembers all operations that are already supported
      val presentOperations = domain.operationsPresentEarlier(tpeCase)

      val overwrittenOperations = allOperations.filter { operation =>
        // Does our current domain contain an override implementation?
        evolutionImplementationProvider.evolutionSpecificDependencies(
          PotentialRequest(domain.baseDataType, tpeCase, operation)
        ).contains(domain)
        //        // Are we applicable based on EIP? Tells us in which domain EIP is applicable
        //        val lastOverwritingDomain =
        //          evolutionImplementationProvider.applicableIn(
        //            forApproach = this,
        //            potentialRequest = PotentialRequest(domain.baseDataType, tpeCase, operation),
        //            currentModel = domain
        //          )
        //        lastOverwritingDomain.contains(domain)
      }
      val updatedOperations = (allOperations -- presentOperations) ++ overwrittenOperations
      // If we have any updated operations, if we have a former one that doesn't support the current type case, or if we are in a merge.
      val output = if (updatedOperations.nonEmpty || domain.former.exists(ancestor => !ancestor.supports(tpeCase)) || domain.former.size > 1) {
        resultMap.updated(tpeCase, updatedOperations)
      } else {
        resultMap
      }
      output
    }
  }

  def dataTypeCasesWithNewOperationsFailsJ3AndJ8(domain: GenericModel): Map[DataTypeCase, Set[Operation]] = {
    val flatDomain = domain.flatten

    val allDataTypeCases = flatDomain.typeCases.toSet
    val allOperations = flatDomain.ops.toSet
    val lastExp = latestModelDefiningNewTypeInterface(domain)
    val overridden = domain.toSeq.filter(dm => lastExp.before(dm)).flatMap(m => m.optimizations).groupBy(_._1).map(entry => (entry._1, entry._2.map(pair => pair._2).toSet))

    // Merging makes this more complicated BECAUSE there could be multiple Exp that are brought together,
    // and if so, then will need to BLEND together
    val pastWithExp = if (domain.former.length > 1) domain.former.filter(dm => dm == latestModelDefiningNewTypeInterface(dm)) else Seq.empty

    val merged = pastWithExp.flatMap(m => dataTypeCasesWithNewOperationsFailsJ3AndJ8(m)).groupBy(_._1)
      .map(triple => triple._1 -> triple._2.flatMap(pm => pm._2))
      .filter(entry => entry._2.nonEmpty)

    // whenever a new Exp is defined, MUST duplicate logic for all producer methods; incorporate into logic below
    val addedExp = domain == lastExp

    val updated = allDataTypeCases.map(tpe => {
      val mt = domain.findTypeCase(tpe).get

      val affected = allOperations.filter(op => {
        val mo = domain.findOperation(op).get
        val descendant = domain.inChronologicalOrder.find(m => !m.notComparableTo(mt) && !m.notComparableTo(mo) && mt.beforeOrEqual(m) && mo.beforeOrEqual(m)).get
        !descendant.before(domain) || (addedExp && op.isProducer(domain))
      })

      (tpe, affected)
    }).filter(pair => pair._2.nonEmpty).toMap

    val output = Seq(overridden, merged, updated)
      .flatten
      .groupBy { case (k, _) => k }
      .map(entry => (entry._1, entry._2.flatMap(pair => pair._2).toSet))

    output
  }

  def latestDependenciesForOp(domain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type], op: Operation)(tpeCases: Seq[DataTypeCase] = domain.flatten.typeCases.distinct): Seq[Operation] = {
    tpeCases.distinct.flatMap(tpeCase => {
      val dependencies = domainSpecific.evolutionSpecificDependencies(PotentialRequest(domain.baseDataType, tpeCase, op))
      val dependenciesDeclaredBeforeCurrentDomain = dependencies.view.filterKeys(d => d.beforeOrEqual(domain))
      val latestDependencies = dependenciesDeclaredBeforeCurrentDomain
        .max((l: (GenericModel, Set[Operation]), r: (GenericModel, Set[Operation])) => {
          if (l._1.before(r._1)) -1 else if (r._1.before(l._1)) 1 else 0
        })
      latestDependencies._2
    }).distinct
  }

  def latestDomainWithAlgebraOperation(domain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type], op: Operation): GenericModel = {
    // Find all domains with an EIP that implements op for any type case
    val domainsImplementingOp = domain.flatten.typeCases.flatMap(tpeCase =>
      domainSpecific.evolutionSpecificDependencies(PotentialRequest(domain.baseDataType, tpeCase, op)).keySet
    )

    def cmp(l: GenericModel, r: GenericModel) = {
      if (l.before(r)) -1 else if (r.before(l)) 1 else 0
    }

    def futureMergePoint(l: GenericModel, r: GenericModel)(m: GenericModel): Boolean = {
      l.beforeOrEqual(m) && r.beforeOrEqual(m)
    }

    val orderedImplementers = domainsImplementingOp.distinct
      .filter(d => d.beforeOrEqual(domain)) // filter to make sure we are before the current domain (we are not interested in later EIPs)
      .sorted((x, y) => cmp(x, y))
      .reverse

    // Are there two non-comparable ancestors l, r that haven't been merged by a third m which is past both? Then we are
    // responsible for the merge!
    if (orderedImplementers.size > 1 && orderedImplementers.exists(l => orderedImplementers.exists(r =>
      cmp(l, r) == 0 && !orderedImplementers.exists(futureMergePoint(l, r))))
    ) {
      return domain
    }
    orderedImplementers.head

    // LEAVE THIS HERE TO SHOW WHAT WAS REPLACED
    //    // find type case where domainSpecific says you are implemented here.
    //    val result = domain.flatten.typeCases.map(tpeCase =>
    //      // are we implementing or overriding.
    //        domainSpecific.applicableIn(
    //          forApproach = this,
    //          potentialRequest = PotentialRequest(domain.baseDataType, tpeCase, op),
    //          currentModel = domain
    //        )
    //    )
    //
    //    val implementedHere = result.contains(Some(domain))
    //    if (implementedHere) {
    //      domain
    //    } else {
    //       val pastOnes = domain.former.map(m => latestDomainWithAlgebraOperation(m, domainSpecific, op)).distinct
    //       if (domain.isDomainBase || pastOnes.size > 1) {
    //         domain
    //       } else {
    //         pastOnes.head
    //       }
    //    }
  }

  def makeTypeParameter(tpe: paradigm.syntax.Type, dependentOpTypes: Seq[paradigm.syntax.Type]): Generator[polymorphics.TypeParameterContext, Unit] = {
    import genericsParadigm.typeParameterCapabilities._

    // needs to add more lower bounds for dependent operations
    for {
      carrierType <- getCurrentTypeParameter()
      itype <- applyType(tpe, Seq(carrierType))
      _ <- addLowerBound(itype)

      _ <- forEach(dependentOpTypes) { ditype => {
        for {
          dtype <- applyType(ditype, Seq(carrierType))
          _ <- addLowerBound(dtype)
        } yield ()
      }
      }
    } yield ()
  }

  //////////////////////////////////////////////////////////////////////////////////////

  def addOperationCarrierInstanceForDataTypeCase(domainSpecific: EvolutionImplementationProvider[this.type], domain: GenericModel, dt: DataTypeCase, op: Operation): Generator[paradigm.ProjectContext, Unit] = {
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

        // this is different in CoCo, Trivially and others, though it means the same thing
        val properModel = latestDomainWithAlgebraOperation(domain, domainSpecific, op)

        for {
          returnType <- toTargetLanguageType(op.returnType)
          _ <- resolveAndAddImport(returnType)
          _ <- setReturnType(returnType)
          selfRef <- selfReference()
          selfMember <- getMember(selfRef, ComponentNames.getSelf)
          selfCall <- apply(selfMember, Seq.empty)

          dtElements <- forEach(dt.attributes) { param =>
            for {
              paramField <- getMember(selfRef, names.mangle(param.name))
            } yield (param, paramField)
          }

          // An operation -- like equals(other) -- has an argument; the isXXX() operations have two arguments
          // or one argument -- based on whether the corresponding data type has 1 or 2 attributes. Somehow have
          // to collect together arguments to use within the ReceivedRequest(). Sometimes from the op... sometimes
          // from the data type. TRICKY!
          myargs <- forEach(op.parameters) { op =>
            val name = names.mangle(op.name)
            for {
              tpe <- toTargetLanguageType(op.tpe)
            } yield (name, tpe)
          }
          _ <- setParameters(myargs)
          args <- getArguments()
          //_ <- debug(domain.name + " replaced with " + properModel.name + " for operation " + op.name + " & dt=" + dt.name)

          result <- domainSpecific.logic(this)(
            ReceivedRequest(
              domain.baseDataType,
              dt,
              selfCall,
              dt.attributes.zip(dtElements.map(_._2)).toMap,
              Request(op, op.parameters.zip(args.map(_._3)).toMap),
              Some(properModel)
            )
          )
        } yield result
      }

      def getSelfMethod(interfaceType: paradigm.syntax.Type): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        import ooParadigm.methodBodyCapabilities._
        import paradigm.methodBodyCapabilities._
        for {
          _ <- setReturnType(interfaceType)
          self <- selfReference()
          algebra <- getMember(self, ComponentNames.algebraAtt)
          dtMethod <- getMember(algebra, names.mangle(names.instanceNameOf(dt)))

          // params for datatype
          params <- forEach(dt.attributes) { att => {
            for {
              p <- getMember(self, names.mangle(att.name))
            } yield p
          }
          }

          // algebra.lit(value);
          invocation <- apply(dtMethod, params)

        } yield Some(invocation)
      }

      def makeOperationConstructor(tpeParam: paradigm.syntax.Type): Generator[ooParadigm.ConstructorContext, Unit] = {
        import ooParadigm.constructorCapabilities._
        import genericsParadigm.constructorCapabilities._
        for {
          signatureTpe <- findClass(names.mangle(names.instanceNameOf(domain)), ComponentNames.signature)
          _ <- resolveAndAddImport(signatureTpe)
          appliedSignatureTpe <- applyType(signatureTpe, Seq(tpeParam))
          params <- forEach(dt.attributes) { att => {
            for {
              tpe <- toTargetLanguageType(att.tpe)
              name <- freshName(names.mangle(att.name))
            } yield (name, tpe)
          }
          }
          attParamName <- freshName(ComponentNames.algebraAtt)
          _ <- setParameters((attParamName, appliedSignatureTpe) +: params)
          arguments <- getArguments()
          _ <- forEach(arguments.zip(ComponentNames.algebraAtt +: dt.attributes.map(att => names.mangle(att.name)))) { case (arg, att) =>
            for {
              _ <- initializeField(att, arg._3)
            } yield ()
          }
        } yield ()
      }

      import ooParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._
      for {
        // make sure to grab appropriate carrier interface from where op is defined
        interfaceType <- findClass(names.mangle(names.instanceNameOf(domain.findOperation(op).get)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(op)))
        carrierTypeParam <- freshName(ComponentNames.returnTypeParameter)

        // might not exist? must filter as well?
        // domainSpecific.dependencies(op, dt)
        diTypes <- forEach(latestDependenciesForOp(domain, domainSpecific, op)(Seq(dt))) { dop => {
          for {
            ditype <- findClass(names.mangle(names.instanceNameOf(domain.findOperation(dop).get)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(dop)))
            _ <- resolveAndAddImport(ditype)
          } yield ditype
        }
        }

        _ <- addTypeParameter(carrierTypeParam, makeTypeParameter(interfaceType, diTypes))
        tpeParams <- getTypeArguments()

        _ <- registerCarrierInstanceTypeMapping(domain, tpeParams) // CRITICAL to do before generating any subsequent artifacts

        _ <- addMethod(ComponentNames.getSelf, getSelfMethod(tpeParams.head))

        _ <- resolveAndAddImport(interfaceType)
        appliedInterfaceType <- applyType(interfaceType, Seq(tpeParams.head))
        _ <- addImplemented(appliedInterfaceType)

        signatureTpe <- findClass(names.mangle(names.instanceNameOf(domain)), ComponentNames.signature)
        _ <- resolveAndAddImport(signatureTpe)
        signatureTpeGeneric <- applyType(signatureTpe, Seq(tpeParams.head))
        _ <- addField(ComponentNames.algebraAtt, signatureTpeGeneric)

        _ <- forEach(dt.attributes) { att =>
          for {
            tpe <- toTargetLanguageType(att.tpe)
            _ <- addField(names.mangle(names.instanceNameOf(att)), tpe)
          } yield ()
        }
        _ <- addConstructor(makeOperationConstructor(tpeParams.head))
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
          _ <- addCombinedAlgebra(domain, domainSpecific) // need domainSpecific to find appropriate algebra.Op artifact

          // only necessary if operation is new
          _ <- forEach(domain.ops) { op =>
            for {
              _ <- addOperationCarrierInterface(domain, op)
            } yield ()
          }

          // if there has been any update to the signature then we need this
          // (1) new data type; (2) merge position to dispatch to different branches
          // per-operation, so find last point where operation was implemented - if HERE then generate algebra; if two
          // formers that define different ones; if just one former that does it, we don't need to (though later it is
          // necessary to take this into account when searching for the past algebra.operation class
          _ <- forEach(domain.flatten.ops.filter(op => latestDomainWithAlgebraOperation(domain, domainSpecific, op) == domain)) { op =>
            addAlgebraOperation(domain, domainSpecific, op)
          }

          _ <- forEach(dataTypeCasesWithNewOperations(domainSpecific, domain).toSeq) { case (dataTypeCase, ops) =>
            for {
              _ <- forEach(ops.toSeq) { op =>
                for {
                  _ <- addOperationCarrierInstanceForDataTypeCase(domainSpecific, domain, dataTypeCase, op)
                } yield ()
              }
            } yield ()
          }

          // case 1: if just doing new data types, no need for new carrier
          // case 2: if a new operation
          // check if we are carrier to use....
          _ <- if (domain == carrierToUse(domain)) {
            for {
              _ <- addCarrierModel(domain)
              _ <- addCombinedCarrierInstance(domain)
            } yield ()
          } else {
            Command.skip[paradigm.ProjectContext]
          }
          _ <- forEach(domain.former) { ancestor => implementRecursive(ancestor) }
        } yield ()
      }
    }

    for {
      _ <- domainSpecific.initialize(this)
      _ <- implementRecursive(domain)
    } yield ()
  }

  // must be the case that op is valid in model
  def registerInstanceTypeMappingOperation(model: GenericModel, tpeParams: Seq[paradigm.syntax.Type], op: Operation): Generator[ooParadigm.ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._
    import genericsParadigm.constructorCapabilities._ // NEEDED, despite IntelliJ editor
    import paradigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._
    import ooParadigm.constructorCapabilities._
    import ooParadigm.methodBodyCapabilities._
    val dtpeRep = DomainTpeRep.DataType(model.baseDataType)

    // Operation must be discoverable already for this model
    assert(model.findOperation(op).nonEmpty)

    def properCarrierType[Context](
      implicit
      canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
      canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
      canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]],
      canApplyType: Understands[Context, Apply[paradigm.syntax.Type, paradigm.syntax.Type, paradigm.syntax.Type]]
    ): Generator[Context, paradigm.syntax.Type] = {
      for {
        baseInterfaceType <- FindClass[paradigm.syntax.Name, paradigm.syntax.Type](Seq(names.mangle(names.instanceNameOf(model.findOperation(op).get)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(op)))).interpret(canFindClass)
        _ <- resolveAndAddImport(baseInterfaceType)(canResolveImport, canAddImport)
        appliedInterfaceType <- Apply[paradigm.syntax.Type, paradigm.syntax.Type, paradigm.syntax.Type](baseInterfaceType, tpeParams).interpret(canApplyType)
      } yield appliedInterfaceType
    }

    for {
      _ <- addTypeLookupForMethods(dtpeRep, properCarrierType[paradigm.MethodBodyContext])
      _ <- addTypeLookupForClasses(dtpeRep, properCarrierType[ooParadigm.ClassContext])
      _ <- addTypeLookupForConstructors(dtpeRep, properCarrierType[ooParadigm.ConstructorContext])
    } yield ()
  }

  def registerCarrierInstanceTypeMapping(model: GenericModel, tpeParams: Seq[paradigm.syntax.Type]): Generator[ooParadigm.ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._
    import genericsParadigm.constructorCapabilities._ // NEEDED, despite IntelliJ editor
    import paradigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._
    import ooParadigm.constructorCapabilities._
    import ooParadigm.methodBodyCapabilities._
    val dtpeRep = DomainTpeRep.DataType(model.baseDataType)

    for {
      _ <- addTypeLookupForMethods(dtpeRep, Command.lift[paradigm.MethodBodyContext, paradigm.syntax.Type](tpeParams.head))
      _ <- addTypeLookupForClasses(dtpeRep, Command.lift[ooParadigm.ClassContext, paradigm.syntax.Type](tpeParams.head))
      _ <- addTypeLookupForConstructors(dtpeRep, Command.lift[ooParadigm.ConstructorContext, paradigm.syntax.Type](tpeParams.head))
    } yield ()
  }

  // Generic registration of ep.<model>.carrier.<Model> in Class and Project context
  def registerInstanceTypeMapping[Context](model: GenericModel)(
    implicit
    canAddTypeLookupForMethods: Understands[Context, AddTypeLookup[paradigm.MethodBodyContext, paradigm.syntax.Type]],
    canAddTypeLookupForClasses: Understands[Context, AddTypeLookup[ooParadigm.ClassContext, paradigm.syntax.Type]],
    canAddTypeLookupForConstructors: Understands[Context, AddTypeLookup[ooParadigm.ConstructorContext, paradigm.syntax.Type]],
  ): Generator[Context, Unit] = {
    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._
    import paradigm.methodBodyCapabilities._
    import ooParadigm.constructorCapabilities._
    import ooParadigm.methodBodyCapabilities._
    val dtpeRep = DomainTpeRep.DataType(model.baseDataType)

    def properCarrierType[Context](
      implicit
      canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
      canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
      canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]]
    ): Generator[Context, paradigm.syntax.Type] = {
      for {
        baseInterfaceType <- FindClass[paradigm.syntax.Name, paradigm.syntax.Type](Seq(names.mangle(names.instanceNameOf(model)), ComponentNames.pkgCarrier, names.mangle(names.conceptNameOf(model)))).interpret(canFindClass)
        _ <- resolveAndAddImport(baseInterfaceType)(canResolveImport, canAddImport)
      } yield baseInterfaceType
    }

    for {
      _ <- AddTypeLookup[paradigm.MethodBodyContext, paradigm.syntax.Type](dtpeRep, properCarrierType[paradigm.MethodBodyContext]).interpret(canAddTypeLookupForMethods)
      _ <- AddTypeLookup[ooParadigm.ClassContext, paradigm.syntax.Type](dtpeRep, properCarrierType[ooParadigm.ClassContext]).interpret(canAddTypeLookupForClasses)
      _ <- AddTypeLookup[ooParadigm.ConstructorContext, paradigm.syntax.Type](dtpeRep, properCarrierType[ooParadigm.ConstructorContext]).interpret(canAddTypeLookupForConstructors)
    } yield ()
  }

  def registerTypeMapping(model: GenericModel): Generator[ooParadigm.ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._

    val dtpeRep = DomainTpeRep.DataType(model.baseDataType)

    for {
      carrierType <- getTypeArguments().map(tpeArgs => tpeArgs.head)
      _ <- addTypeLookupForMethods(dtpeRep, Command.lift(carrierType))
      _ <- addTypeLookupForClasses(dtpeRep, Command.lift(carrierType))
      _ <- addTypeLookupForConstructors(dtpeRep, Command.lift(carrierType))
    } yield ()
  }

  override def implement(tests: Map[GenericModel, Seq[TestCase]], testImplementationProvider: TestImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    import paradigm.projectCapabilities._
    import paradigm.compilationUnitCapabilities._
    import paradigm.testCapabilities._
    import ooParadigm.projectCapabilities._

    def addAlgebraFieldAutoinitialized(model: GenericModel): Generator[paradigm.TestContext, Unit] = {
      import ooParadigm.testCapabilities._
      for {
        tpe <- findClass(names.mangle(names.instanceNameOf(model)), ComponentNames.pkgAlgebra, names.mangle(names.conceptNameOf(model)))
        _ <- resolveAndAddImport(tpe)
        expr <- instantiateObject(tpe, Seq.empty)
        _ <- addField(ComponentNames.algebraAtt, tpe, initializer = Some(expr))
      } yield ()
    }

    for {
      _ <- forEach(tests.toList) { case (model, tests) =>

        val testCode: Generator[paradigm.MethodBodyContext, Seq[paradigm.syntax.Expression]] = {
          import paradigm.methodBodyCapabilities._
          for {
            code <- forEach(tests) {
              test =>
                testImplementationProvider.test(this)(test)
            }
          } yield code.flatten
        }


        val compUnit = for {
          _ <- addAlgebraFieldAutoinitialized(model) // made this autoinitialized using default constructor new CLASS()

          // add test case first
          _ <- addTestCase(testCode, testName)
        } yield ()

        val testSuite = for {
          _ <- addTestSuite(testCaseName(model), compUnit)
        } yield ()


        for {
          _ <- registerInstanceTypeMapping(model)
          _ <- addCompilationUnit(testSuite, testCaseName(model))
        } yield ()
      }
    } yield ()
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
      (generics: Generics.WithBase[base.type, oo.type, params.type]): ObjectAlgebras.WithParadigm[base.type] = {
    case class OA(
      override val paradigm: base.type
    )(
      override val names: NameProvider[paradigm.syntax.Name],
      override val ooParadigm: oo.type,
      override val polymorphics: params.type
    )(
      override val genericsParadigm: generics.type
    ) extends ObjectAlgebras
    OA(base)(nameProvider, oo, params)(generics)
  }
}