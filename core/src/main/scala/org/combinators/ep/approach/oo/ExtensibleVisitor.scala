package org.combinators.ep.approach.oo    /*DI:LI:AD*/

import org.combinators.ep.domain.GenericModel
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm._
import Command._
import AnyParadigm.syntax._

/**
 * Synthesizing OO and Functional Design to promote Reuse
 * Shriram Krishnamurthi, Matthias Felleisen, Daniel Friedman
 * https://dl.acm.org/citation.cfm?id=679709
 *
 * https://stackoverflow.com/questions/55501899/exception-in-intellijs-sbt-console-not-found-value-ideaport-ideaport-in-globa
 * problem with sbt...
 *
 * Original paper had no advice on merge case, so following is missing from k2j6
 *
     public ep.j4.IsPower makeIsPower(Exp left, Exp right) {
        return new IsPower(left, right);
    }
 */
trait ExtensibleVisitor extends SharedOO with OperationAsClass {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]

  import paradigm._
  import ooParadigm._
  import syntax._

  object factory {
    val makePrefix = "make"

    /**
     * Standard factory name for an operation.
     *
     * @param op    operation for which a factory is desired.
     * @return
     */
    def name(op:Operation) : Name = {
      names.addPrefix(makePrefix, names.mangle(names.conceptNameOf(op)))
    }

    /**
     * {{{
     *   public FACTORYNAME () {
     *     return new TYPENAME();
     *   }
     * }}}
     * @param model      last model.
     * @param op         operation that needs to be constructed
     * @param typeName   fully qualified class to be constructed
     * @return
     */
    def create(model:GenericModel, op:Operation, shouldOverride: Boolean, typeName:Seq[Name]): Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      for {
        // These must be PUBLIC to allow overriding to occur. Another alternative is to make them protected, but this
        // concept might translate differently among programming languages.
        _ <- addMethod(name(op), makeFactoryOperationImpl(model, op, shouldOverride, typeName))
      } yield ()
    }
  }

 object visitor {
   val accept: Name = names.mangle("accept")
   val visit: Name = names.mangle("visit")
   val visitorClass: Name = names.mangle("Visitor")
   val visitorParameter: String = "v"
   val expParameter: String = "exp"
   val visitTypeParameter: String = "R"

     /**
      * A visitor that returns values can have a type parameter that specifies the type of the value returned.
      *
      * In Java, for example, the following is an accept method that takes a type parameter R.
      *
      * {{{
      *  public abstract <R> R accept(Visitor<R> v);
      * }}}
      * @return
      */
     def makeAcceptSignatureWithType(): Generator[MethodBodyContext, Unit] = {
       import paradigm.methodBodyCapabilities.{toTargetLanguageType => _, _}
       import polymorphics.methodBodyCapabilities._
       import ooParadigm.methodBodyCapabilities._

       for {
         // R by itself, since not extending any other type parameter (hence Skip)
         visitTyParam <- freshName(names.mangle(visitTypeParameter))
         _ <- addTypeParameter(visitTyParam, Command.skip)

         // this returns mangled visitTypeParameter name and gets list of all type parameters, for which there is only one, so we get head
         args <- getTypeArguments()
         _ <- setReturnType(args.head)

         // identify Visitor<R>
         visitorClassType <- findClass(visitorClass)
         _ <- resolveAndAddImport(visitorClassType)
         visitorType  <- applyType (visitorClassType, args)

         visitParam <- freshName(names.mangle(visitorParameter))
         _ <- setParameters(Seq((visitParam, visitorType)))      // a pair (name,type) of only one sequence
       } yield ()
     }

     /**
      * Define the base class for Exp which must contain the accept method as an abstract method.
      *
      * {{{
      *  public abstract class Exp {
      *    public abstract <R> R accept(Visitor<R> v);
      *  }
      * }}}
      *
      * @param tpe    data type case whose class needs to be generated
      * @return
      */
     def makeBase(tpe: DataType): Generator[ProjectContext, Unit] = {
       import ooParadigm.projectCapabilities._

       val makeClass: Generator[ClassContext, Unit] = {
         import ooParadigm.classCapabilities._
         for {
           _ <- setAbstract()
           _ <- addAbstractMethod(accept, makeAcceptSignatureWithType())
         } yield ()
       }

       // adds the 'Exp' class, with a single accept method
       addClassToProject( makeClass, names.mangle(names.conceptNameOf(tpe)))
     }

     /**
      * {{{
      *   public class Add extends Exp {
      *      public Add(Exp left, Exp right) {
      *         this.left = left;
      *         this.right = right;
      *     }
      *
      *     private Exp left;
      *     private Exp right;
      *     public Exp getLeft() { return this.left; }
      *     public Exp getRight() { return this.right; }
      *     public <R> R accept(Visitor<R> v) { return v.visit(this); }
      *   }
      * }}}
      *
      *  Make sure to register whenever you make the class
      */
     def makeDerived(parentType: DataType, tpeCase: DataTypeCase, model: GenericModel): Generator[ProjectContext, Unit] = {
       import ooParadigm.projectCapabilities._
       val makeClass: Generator[ClassContext, Unit] = {
         import ooParadigm.classCapabilities._
         for {
           parent <- toTargetLanguageType(TypeRep.DataType(parentType))
           _ <- resolveAndAddImport(parent)
           _ <- addParent(parent)
           _ <- forEach (tpeCase.attributes) { att => makeField(att) }
           _ <- addConstructor(makeConstructor(tpeCase))
           _ <- forEach (tpeCase.attributes) { att => makeGetter(att) }

           // this is potentially different with extensible visitor
           _ <- makeAcceptImplementation(model)
         } yield ()
       }

       addClassToProject(makeClass, dataTypeClassName(model, tpeCase) : _ *)
     }

     /**
      * Creates the signature for the 'abstract R visit(DataType exp)' method which still has no body, and can
      * thus become an abstract interface declaration or form the basis for an implementation.
      *
      * {{{
      *   public R visit(Sub exp);
      * }}}
      * @return
      */
     def makeVisitSignature(tpe:DataTypeCase, visitResultType: Type): Generator[MethodBodyContext, Unit] = {
       import paradigm.methodBodyCapabilities.{toTargetLanguageType => _, _}
       import polymorphics.methodBodyCapabilities._
       import ooParadigm.methodBodyCapabilities._

       for {
         _ <- setReturnType(visitResultType)
         visitedClassType <- findClass(names.mangle(names.conceptNameOf(tpe)))
         _ <- resolveAndAddImport(visitedClassType)
         visitParamName <- freshName(names.mangle(expParameter))
         _ <- setParameters(Seq((visitParamName, visitedClassType)))      // a pair (name,type) of only one sequence
       } yield ()
     }

     /** Visitor requires an abstract base class as follows, integrating all types:
      * {{{
      *     public abstract class Visitor<R> {
      *       public abstract R visit(Sub exp);
      *       public abstract R visit(Lit exp);
      *       ...
      *     }
      * }}}
      *
      * @param allTypes   the flattened types in the model, for which Visitor has a single method for each type.
      * @return
      */
     def makeVisitorInterface(allTypes:Seq[DataTypeCase]): Generator[ClassContext, Unit] = {
       import ooParadigm.classCapabilities._
       import genericsParadigm.classCapabilities._

       for {
         _ <- setInterface()
         visitTyParam <- freshName(names.mangle(visitTypeParameter))
         _ <- addTypeParameter(visitTyParam, Command.skip) // R by itself, since not extending any other type parameter (hence Skip)
         visitResultType <- getTypeArguments().map(_.head)
         _ <- forEach (allTypes) { tpe => addAbstractMethod(visit, makeVisitSignature(tpe, visitResultType)) }
       } yield ()
     }

   /**
    * Constructor for an operation which MAY have parameters
    * @param op   operation for which a constructor is needed
    * @return
    */
   def makeOperationConstructor(op: Operation, parent:Option[Type]): Generator[ConstructorContext, Unit] = {
     import constructorCapabilities._
     for {
       params <- forEach (op.parameters) { param =>
         for {
           paramTy <- toTargetLanguageType(param.tpe)
           _ <- resolveAndAddImport(paramTy)
           pName <- freshName(names.mangle(param.name))
         } yield (pName, paramTy)
       }
       _ <- setParameters(params)
       args <- getArguments()
       _ <- if (parent.isEmpty) {
         forEach(op.parameters.zip(args)) { case (param, (_, _, arg)) =>
           initializeField(names.mangle(param.name), arg)
         }
       } else {
         initializeParent(parent.get, args.map(p => p._3))
       }
     } yield ()

   }
   }

  /** Before instantiate is called, use this to register class where type is placed. */
  case class OnlyDataTypeCase(tpeCase: DataTypeCase) extends TypeRep

  /** Produces, for example, Some(EvalDivdMultNeg).
   *
   * Either (1) the operation is defined in the current model and so you don't need to append class name, but can
   * simply reply on past datatypes; or (2) concatenate and find recent one
   *
   * Compute the name for the visitor implementation of the given model and operation, if an implementation
   * is required.
   */
  def visitorClassName(model: GenericModel, operation: Operation) : Option[Seq[Name]] = {
    val operationName = names.mangle(names.conceptNameOf(operation))
    Some(Seq(names.mangle(names.instanceNameOf(model)), operationName))
  }

  def dataTypeClassName(model: GenericModel, tpeCase: DataTypeCase): Seq[Name] = {
    Seq(names.mangle(names.instanceNameOf(model)), names.mangle(names.conceptNameOf(tpeCase)))
  }

  /**
   * Instantiates an instance of the domain object.
   *
   * Same implementation for OO as for visitor.
   *
   * new Add(new Lit(new Double(1.0)), new Lit(new Double(2.0)))
   */
  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      // access the constructor for the class associated with type case and invoke constructors with arguments.
      rt <- toTargetLanguageType(OnlyDataTypeCase(tpeCase))
      _ <- resolveAndAddImport(rt)
      res <- instantiateObject(rt, args)
    } yield res
  }

  /**
   * Instantiate an implementation of the visitor.
   *
   * Required capability of having makeXXX() within the extensible visitors.
   *
   * {{{
   *   Eval makeEval() {
   *     return new Eval();
   *   }
   * }}}
   *
   * If/When the operation has parameters, then they would have to be defined and passed through from makeXXX() into the new XXXX()
   */
  def instantiateVisitor(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      self <- selfReference()
      method <- getMember(self, factory.name(message.request.op))
      instance <- apply(method, message.request.arguments.toSeq.map(_._2))  // no arguments
    } yield instance
  }

  /**
   * Dispatch in visitor we need to find context on which to accept a visitor.
   *
   * That is, e.getLeft().accept(new Eval()) + e.getRight().accept(new Eval());
   *
   * In particular, when dispatching an operation (with parameters) on an expression,
   * we want to return something like this:
   *
   * $expr.accept(make${op}($args))
   *
   * {{{
   * public Double mult(Mult exp) {
   *   return exp.getLeft().accept(new Eval()) * exp.getRight().accept(new Eval());
   * }
   *
   * public Double visit(Mult e) {
   *   return e.getLeft().accept(makeEval()) * e.getRight().accept(makeEval());
   * }
   * }}}
   * If the operation (i.e., Eval) has parameters, then the makeXXX() should have those and the outer accept(v) remains
   * untouched.
   *
   * @param message    the SendRequest to be dispatched.
   * @return
   */
  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._
    for {

      // In the "message.to" expression, invoke the 'accept' method with a visitor argument
      genericMethod <- getMember(message.to, visitor.accept)   // things which are code-generated use the '<-' handles unpacking results
      rt <- toTargetLanguageType(message.request.op.returnType)
      _ <- resolveAndAddImport(rt)
      method <- instantiateTypeParameter(genericMethod, Seq(rt))

      // construct the visitor object for the given type (and parameters). Not just a constructor but a
      // call to makeXXXX()
      visitor <- instantiateVisitor(message)

      // apply to method with the visitor, resulting in the expression
      result <- apply(method, Seq(visitor))           // has to be Seq[] just for syntax of calling the method
    } yield result
  }

  /** Find the last evolution that requires its own TypeCase definition. */
  def latestModelDefiningVisitor(domain: GenericModel): GenericModel = {
    if (domain.isDomainBase || domain.typeCases.nonEmpty || domain.former.length > 1) {   // handle merge case as well
      domain
    } else {
      // find where tpe was defined and also where last operation was defined and choose later of the two
      // will only have one, since merge case handled above.
      // could be one of our ancestors is a merge point
      latestModelDefiningVisitor(domain.former.head)
    }
  }

  /** Handles situation where operation is redefined in multiple branches because of new data types in each. */
  def latestModelDefiningOperation(domain:GenericModel, op:Operation): Seq[GenericModel] = {
    if ((domain == latestModelDefiningVisitor(domain) && domain.flatten.ops.contains(op)) || domain.ops.contains(op)) {
      Seq(domain)
    } else {
      // throw out everything for which there is a newer one in the former.
      val allPast = domain.former.flatMap(m => latestModelDefiningOperation(m, op))
      allPast.filterNot(m => allPast.exists(mnewer => m.before(mnewer)))
    }
  }

  /**
   * Compute the name for the visitor interface of the given model, if a new interface is required.
   *
   * Base remains as {visitorClass}
   * sub-classes are {visitorClass}DataTypes...
   */
  def visitorInterfaceName(model: GenericModel): Seq[Name] = {
    if (model.isDomainBase) {
      Seq(visitor.visitorClass)
    } else {
      Seq(names.mangle(names.instanceNameOf(model)), visitor.visitorClass)
    }
  }

  /** Create an accept implementation from the accept method signature.
   * {{{
   *  public <R> R accept(Visitor<R> v) {
   *    if (v instanceof VisitorDivdMultNeg) {
   *         return ((VisitorDivdMultNeg<R>) v).visit(this);
   *    }
   *     throw new RuntimeException("Older visitor used with newer datatype variant.");
   * }
   * }}}
   *
   * {{{
   *    return ((VisitorDivdMultNeg<R>) v).visit(this);
   * }}}
   * //TODO: Could change this to perform instanceof check + throw exceptions
   */
  def makeAcceptImplementation(model: GenericModel): Generator[ClassContext, Unit] = {
    val makeBody: Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import polymorphics.methodBodyCapabilities._

      for {
        _ <- visitor.makeAcceptSignatureWithType()   // start from the accept signature and add a method body.
        args <- getArguments()                       // get name, type, expression
        v = args.head._3
        vType = visitorInterfaceName(model)          // convert Name to a class

        visitorClassType <- findClass(vType : _ *)
        tpeParam <- getTypeArguments()
        instVisitClassType <- applyType(visitorClassType, tpeParam)
        castV <- castObject(instVisitClassType, v)
        // invoke visit method on 'v' with 'this' as argument
        visitFunction <- getMember(castV, visitor.visit)

        self <- selfReference()
        result <- apply(visitFunction, Seq(self))  // make the method invocation
      } yield Some(result)
    }

    import ooParadigm.classCapabilities._
    addMethod(visitor.accept, makeBody)
  }

  /**
   * Just return the expression required for a factory, which must be the name of the class since
   * we are the visitor
   *
   * {{{
   *     return new EvalSub();
   * }}}
   *
   * @return
   *
   * For Extensible visitor, choose the EARLIEST location of operation and use covariant overiding
   */
  def makeFactoryOperationImpl(model:GenericModel, op: Operation, shouldOverride:Boolean, typeName:Seq[Name]): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    // The earliest this operation occurs is the first candidate. BUT must then find whether there is some type such that (op, tpe) has
    // an overridden implementation. INCLUDING merging
    val earliest = model.findOperation(op).get
    val selected = model.flatten.typeCases.foldLeft(earliest)((earliest,tpe) =>
      model.haveImplementation(PotentialRequest(model.baseDataType, tpe, op)).foldLeft(earliest)((earliest, m) => m.later(earliest)))

    // doesn't handle MERGE well....
    val possible = model.inChronologicalOrder.filter(m => m.former.length > 1 && selected.earlier(m) == selected)
    val chosen = if (possible.nonEmpty) {
      // could be multiple? So take the last one
      possible.last    // head
    } else {
      selected
    }

   for {
      // Type signature uses the earliest one to define, but instantiates the latest with covariant overriding.
      earliestOpClass <- findClass(visitorClassName(earliest, op).get : _ *)
      _ <- resolveAndAddImport(earliestOpClass)
      _ <- setReturnType(earliestOpClass)
      latestOpClass <- findClass(visitorClassName(chosen, op).get : _ *) // findClass(typeName : _ *)
      _ <- resolveAndAddImport(latestOpClass)

      // need parameters for operations with parameters
      params <- forEach (op.parameters) { param =>
        for {
          paramTy <- toTargetLanguageType(param.tpe)
          _ <- resolveAndAddImport(paramTy)
          pName <- freshName(names.mangle(param.name))
        } yield (pName, paramTy)
      }
      _ <- setParameters(params)  // params: Seq[(Name, Type)]

      // subsequent ones need to override.
      _ <- if (shouldOverride) {
        setOverride()
      } else {
        Command.skip[MethodBodyContext]
      }

      args <- getArguments()
      res <- instantiateObject(latestOpClass,args.map(_._3))
    } yield Some(res)
  }

  def makeEachVisitorInterface(domain:GenericModel): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._

    for {
      _ <- setInterface()
      visitTyParam <- freshName(names.mangle(visitor.visitTypeParameter))
      _ <- addTypeParameter(visitTyParam, Command.skip) // R by itself, since not extending any other type parameter (hence Skip)
      visitResultType <- getTypeArguments().map(_.head)
      _ <- forEach (domain.typeCases) { tpe => addAbstractMethod(visitor.visit, makeEachVisitSignature(domain, tpe, visitResultType)) }
    } yield ()
  }

  /** Take existing visitor generated by Shared and add (where needed) a "extends VisitorSub<R>"
   *
   * @param domain      create visitor interface for the given model in the extension graph.
   * @return
   */
  def makeExtensibleVisitorInterface(domain:GenericModel): Generator[ClassContext, Unit] = {
    // ignore degenerate case where the first model only has an operation without any types
    def addParentInterfaces(): Generator[ClassContext, Unit] =  {
      import ooParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._

      // may have duplicates because of merge
      val parentModels = domain.former.map(m => latestModelDefiningVisitor(m)).distinct

      for {
        _ <- forEach(parentModels) { parent => {
          for {
            // find former Op interface
            visitorInterfaceType <- findClass(visitorInterfaceName(parent): _ *)
            _ <- resolveAndAddImport(visitorInterfaceType)

            visitTyParam <- getTypeArguments() // can do this because we need this interfaces paramType
            modifiedType <- applyType(visitorInterfaceType, visitTyParam) // R by itself, since not extending any other type parameter (hence Skip)
            // applyType generates a fresh Type and that is the one that is returned
            _ <- addParent(modifiedType)
          } yield ()
        }
        }
      } yield ()
    }

    for {
      _ <- makeEachVisitorInterface(domain)      // Inherit past type cases from parent visitors
      _ <- addParentInterfaces()
    } yield ()
  }

  def makeEachVisitSignature(domain:GenericModel, tpe:DataTypeCase, visitResultType: Type): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities.{toTargetLanguageType => _, _}
    import polymorphics.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    val whereDefined = domain.findTypeCase(tpe).get
    for {
      _ <- setReturnType(visitResultType)
      visitedClassType <- findClass(dataTypeClassName(whereDefined,tpe) : _ *)
      _ <- resolveAndAddImport(visitedClassType)
      visitParamName <- freshName(names.mangle(visitor.expParameter))
      _ <- setParameters(Seq((visitParamName, visitedClassType)))      // a pair (name,type) of only one sequence
    } yield ()
  }

  def makeEachImplementation(domain:GenericModel, tpe: DataType,
                tpeCase: DataTypeCase,
                op: Operation,
                domainSpecific: EvolutionImplementationProvider[this.type]
               ): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    val properModel = latestModelDefiningOperatorClass(domain, tpeCase, op, domainSpecific).get
    if (properModel != domain) {
      println ("ExtensibleVisitor::makeEachImplementation chooses " + properModel.name + " over " + domain.name + " for (" + op.name + "," + tpeCase.name + ")")
    }
    for {
      returnType <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(returnType)
      _ <- makeEachVisitSignature(domain, tpeCase, returnType)
      _ <- if (!domain.ops.contains(op)) {
        setOverride()    // Hmmm: Might be able to infer this from the model and operation
      } else {
        Command.skip[MethodBodyContext]
      }
      visitedRef <- getArguments().map(_.head._3)
      attAccessors: Seq[Expression] <- forEach (tpeCase.attributes) { att =>
        for {
          getter <- getMember(visitedRef, getterName(att))
          getterCall <- apply(getter, Seq.empty)
        } yield getterCall
      }

      args <- forEach (op.parameters) { param =>
        for {
          thisRef <- selfReference()
          paramField <- getMember(thisRef, names.mangle(param.name))
        } yield (param, paramField)
      }

      // body of this implementation is the result of the individual domain-specific logic.
      result <- domainSpecific.logic(this)(
                  ReceivedRequest(
                    tpe,
                    tpeCase,
                    visitedRef,
                    tpeCase.attributes.zip(attAccessors).toMap,
                    Request(op, args.toMap),
                    Some(properModel)    // scala implementation for j8 needed this
                  )
        )
    } yield result
  }

  def dependentOperationsOf(
    domain: GenericModel,
    op: Operation,
    domainSpecific: EvolutionImplementationProvider[this.type],
  ): Set[Operation] = {
    val allTpeCases = domain.flatten.typeCases.distinct
    val allDependencies = allTpeCases.map(tpeCase => domainSpecific.evolutionSpecificDependencies(PotentialRequest(domain.baseDataType, tpeCase, op)))
    val combinedDependencies = allDependencies.foldLeft(Map.empty[GenericModel, Set[Operation]]){ case (combined, singular) =>
      val allDomainsWithDependencies = singular.keySet ++ combined.keySet
      allDomainsWithDependencies.foldLeft(Map.empty[GenericModel, Set[Operation]]){ case (newCombinedMap, currentDomain) =>
        newCombinedMap + (currentDomain -> (singular.getOrElse(currentDomain, Set.empty) ++ combined.getOrElse(currentDomain, Set.empty)))
      }
    }
    val priorDomainsDeclaringDependencies = combinedDependencies.keySet.filter(dependencyDomain => dependencyDomain.beforeOrEqual(domain))
    if (priorDomainsDeclaringDependencies.isEmpty) {
      Set.empty
    } else {
      val lastPriorDomainsDeclaringDependencies = priorDomainsDeclaringDependencies.filter(d1 =>
        !priorDomainsDeclaringDependencies.exists(d2 => d1.before(d2))
      )
      lastPriorDomainsDeclaringDependencies.foldLeft[Set[Operation]](Set.empty){ case (s, d) => s ++ combinedDependencies(d) }
    }
  }

  /** Each operation is placed in its own class, with a 'visit' method for newly defined types.
   *
   * {{{
   *   public class EvalSub extends Eval implements VisitorSub<Double> {
   *
   *     public Double visit(Sub e) {
   *         return e.getLeft().accept(makeEval()) - e.getRight().accept(makeEval());
   *     }
   *
   *     EvalSub makeEval() {
   *         return new EvalSub();
   *     }
   * }
   * }}}
   *
   * @param domain     Model for which new types are to be incorporated
   * @param operation  operation that needs an implementation
   * @param domainSpecific   contains the logic
   * @return           Returns class context without actually adding to ProjectContext; this is job of caller of this function
   */
  def makeExtensibleOperationImplementation(domain:GenericModel,
        operation: Operation,
        domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {

    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._

    val allPast = domain.former.flatMap(m => latestModelDefiningOperation(m, operation))
    val previous = allPast.filterNot(m => allPast.exists(mnewer => m.before(mnewer)))

    // only capture new ones. Make sure to apply distinct to handle merging
    val typeCasesToDeclare = if (previous.isEmpty) {
      domain.flatten.typeCases.distinct
    } else {
      val primaryParent = previous.maxBy(m => m.flatten.typeCases.length)
      val primaryParentTypeCases = primaryParent.flatten.typeCases

      domain.flatten.typeCases.filter(tpe => {
        val knownDependencies = domainSpecific.evolutionSpecificDependencies(PotentialRequest(
          domain.baseDataType,
          tpe,
          operation
        ))
        val containsOverride = knownDependencies.exists{ case (overridingDomain, _) =>
          primaryParent.before(overridingDomain) && overridingDomain.beforeOrEqual(domain)
        }
        !primaryParentTypeCases.contains(tpe) || containsOverride
      }).distinct
    }

    // ignore degenerate case where the first model only has an operation without any types
    // if you get the last model with data types, you should stop if op is defined AFTER
    // that one (for example with PrettyP which is defined in M2 but Sub is defined in M1)
    def addParentClass(): Generator[ClassContext, Option[Type]] = if (domain.isDomainBase) {
        Command.lift[ClassContext, Option[Type]](Option.empty)
      } else {
        val latestVisitor = latestModelDefiningVisitor(domain)

        for {
          visitTyParam <- toTargetLanguageType(operation.returnType) // can do this because we need this interfaces paramType
          _ <- resolveAndAddImport(visitTyParam)
          visitorClassType <- findClass(visitorInterfaceName(latestVisitor) : _ *)
          _ <- resolveAndAddImport(visitorClassType)

          modifiedType <- applyType(visitorClassType, Seq(visitTyParam))
          _ <- addImplemented(modifiedType)

          possibleParent <- if (previous.nonEmpty) {
            val primaryParent = previous.maxBy(m => m.flatten.typeCases.length)
            for {
              parentType <- findClass(visitorClassName(primaryParent, operation).get : _ *)
              _ <- resolveAndAddImport(parentType)
              _ <- addParent(parentType)
            } yield Some(parentType)
          }  else {
            Command.lift[ClassContext,Option[Type]](Option.empty)
          }

        } yield possibleParent
      }

    // Later evolution stages can remove a past dependent operation or possibly add. First tuple is
    // m0 and the last one is the current domain.
    val wholeStructure = domain.inChronologicalOrder.map(m => dependentOperationsOf(m, operation, domainSpecific))

    val reduced:Set[Operation] = wholeStructure.foldLeft(Set.empty[Operation])((aggregate, ops) =>
      if (ops.isEmpty) {
        Set.empty[Operation]
      } else {
        aggregate ++ ops
      }
    )

    //val opsSeq:Seq[Operation] = (Seq(operation) ++ domain.inChronologicalOrder.flatMap(m => dependentOperationsOf(m, operation, domainSpecific))).distinct
    val opsSeq:Seq[Operation] = (Seq(operation) ++ reduced).distinct

    for {
      possibleParent <- addParentClass()

      // instead of directly accessing [operationClass]
      _ <- if (possibleParent.isEmpty) {
        addParamFields(operation)
      } else {
        Command.skip[ClassContext]
      }

      _ <- addConstructor(visitor.makeOperationConstructor(operation, possibleParent))

      // for all that are not in primary parent
      _ <- forEach (typeCasesToDeclare) { tpe =>
        for {
          _ <- addMethod(visitor.visit, makeEachImplementation(domain, domain.baseDataType, tpe, operation, domainSpecific))

          // if dependent operations exist, those factories need to be generated as well later
        } yield ()
      }

      // have to take LATEST version, which means the latest of either (a) where op was defined; or (b)
      // most recently defined model with data types. When trying to call another operation, you need.
      // might have to go backward in time to find the model that declared that operation or had
      // an intervening visitor declaration.
      _ <- forEach(opsSeq) { dop =>
          val modelsToUse = latestModelDefiningOperation(domain, dop)
          val modelToUse = modelsToUse.head

        /**
         * There is special case in k1 for MultBy, which needs a makeEval factory method but it isn't overridden
         * because it is a dependent operation and appears for the first time.
         *
         *  makeIsNeg in the Eql.java file in j3 DOES NOT override because it is first presence. Fix this by
         *  identifying that while the operation came from the PAST the dependent operations are, in fact, defined
         *  in modelToUse so they cannot be overridden. DONE
         *
         *  Harder one is k1.MultBy which has a dependent operation Eval, but ONLY for the first time in K1 and
         *  no one before. IN Java if this is public without @Override annotation it works OK. However in Scala
         *  there truly needs to be the "override" keyword.
         *
         *  Final one is a result of single inheritance. Once k2j6 decides to extend from j3 branch, methods that
         *  HAD been defined in the k2 branch no longer deserve to be overridden. SO we somehow have to
         *  ensure that the dependent operation comes from the non-inherited branch, it DOES NOT get overriden,
         *  otherwise it must.
         *
         */

        // HOWEVER, if you are a dependent operation in a merge
        val do_override = if (operation != dop) {
          if (modelToUse.ops.contains(operation) || modelToUse.ops.contains(dop)) {
            false
          } else {
            // this is a dependent operation and MUST CHECK to see if any place in past this same dependent operation had been generated.
            val primaryParent:Option[GenericModel] = if (previous.nonEmpty) {
              Some(previous.maxBy(m => m.flatten.typeCases.length))
            } else {
              None
            }

            // CHOOSE where to start
            var n = if (primaryParent.nonEmpty && modelToUse.former.length > 1) {
              modelToUse.former.filter(p => primaryParent.get.beforeOrEqual(p)).head
            } else {
              modelToUse.former.head
            }

            // likely possible to make this functional...
            var exists_in_past = false
            while (!exists_in_past && !n.isDomainBase) {
              // MERGE that occurred **in the past** MUST have done this already (as long as op was present), so we override.
              if (dependentOperationsOf(n, operation, domainSpecific).contains(dop)) {
                exists_in_past = true
              } else if (n.former.length > 1) {
                // if the HEAD (which is used for single inheritance) does not have this as dependent operation then
                // exists_in_past can be set to false since there is nothing to override. IF, however, the head DOES
                // have this as dependent operation, then Must override.
                exists_in_past = dependentOperationsOf(n.former.head, operation, domainSpecific).contains(dop)
              }

              // Possible issue if a past MERGE must be diverted so it isn't arbitrarily one of the formers. If this
              // ever happens, logic like above could be used, though .filter (p => p.beforeOrEqual(primaryParent.get)).head
              n = n.former.head
            }
            exists_in_past    // if exists in past, then must override
          }
        } else {
          // for O1 need to double check modelToUse != domain since means parent exists
          (!modelToUse.ops.contains(operation)) || (modelToUse != domain)   // if same COULD be defining, but only if previously defined
        }

        /**

          These three are fixed with checking modelToUse.ops.contains(operation)
            j3\IsDivd = makeEql
            j3\IsNeg => makeEql
            k1\IsPower => makeEql (because IsPower is new operation)

        * FINAL SET! THESE ALL are overriding BUT SHOULD NOT be. Fixed above

          x  j6\PowBy => makeEval (because eval is dependency)
          x  k1\MultBy => makeEval (because EIP has implementation)
          x  k2\Simplify => makeEval
          x  k2j6\Eql => makeIsPower (NOT overriding since primary inheritance through J branch
          x  k2j6\MultBy => makeEval (NOT overriding since primary inheritance through J branch
         */

        // dependent operations in their first use are not overridden.
        factory.create(modelToUse, dop, do_override, visitorClassName(modelToUse, dop).get)
      }
    } yield ()
  }

  /**
   * Define the base class for Exp
   * {{{
   *  package visitor;
   *  public abstract class Exp {
   *    public abstract <R> R accept(Visitor<R> v);
   *  }
   * }}}
   * @param model        generate base class for the given domain
   * @return
   */
  def makeOperationsBase(model:GenericModel): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val makeClass: Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      for {
        _ <- setAbstract()
        _ <- addAbstractMethod(visitor.accept, visitor.makeAcceptSignatureWithType())
      } yield ()
    }

    // adds the 'Exp' class, with a single accept method
    addClassToProject(makeClass, names.mangle(names.conceptNameOf(model.baseDataType)))
  }

  def locateTypeCaseClass[Context](domain: GenericModel, dataTypeCase:DataTypeCase)(implicit
       canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
       canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
       canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]],
  ): Generator[Context, paradigm.syntax.Type] = {
    for {
      dataTypeCaseClass <- FindClass[paradigm.syntax.Name, paradigm.syntax.Type](dataTypeClassName(domain,dataTypeCase)).interpret(canFindClass)
      _ <- resolveAndAddImport(dataTypeCaseClass)
    } yield dataTypeCaseClass
  }

  /**
   * Default registration for findClass, which works with each registerTypeMapping for the different approaches.
   *
   * Sometimes the mapping is fixed for an EP approach, but sometimes it matters when a particular class is requested
   * in the evolution of the system over time.
   *
   * @param domain          find base-type class
   * @tparam Ctxt           the Context (to allow for multiple usages)
   * @return
   */
  def domainTypeLookup[Ctxt](domain:GenericModel)(implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]]): Generator[Ctxt, Type] = {
    FindClass(Seq(names.mangle(names.conceptNameOf(domain.baseDataType)))).interpret(canFindClass)
  }

  def registerNewlyDeclaredDataTypeClasses(model:GenericModel): Generator[ProjectContext, Unit] = {
    import paradigm.projectCapabilities.addTypeLookupForMethods
    import ooParadigm.projectCapabilities.addTypeLookupForClasses
    import ooParadigm.projectCapabilities.addTypeLookupForConstructors
    for {
      _ <- forEach(model.typeCases) { tpeCase => // passes on capabilities so it knows which generators to use...
        for {
          _ <- addTypeLookupForMethods(OnlyDataTypeCase(tpeCase), locateTypeCaseClass(model, tpeCase)(canFindClass = ooParadigm.methodBodyCapabilities.canFindClassInMethod, canAddImport = paradigm.methodBodyCapabilities.canAddImportInMethodBody, canResolveImport = paradigm.methodBodyCapabilities.canResolveImportInMethod))
          _ <- addTypeLookupForClasses(OnlyDataTypeCase(tpeCase), locateTypeCaseClass(model, tpeCase)(canFindClass = ooParadigm.classCapabilities.canFindClassInClass, canAddImport = ooParadigm.classCapabilities.canAddImportInClass, canResolveImport = ooParadigm.classCapabilities.canResolveImportInClass))
          _ <- addTypeLookupForConstructors(OnlyDataTypeCase(tpeCase), locateTypeCaseClass(model, tpeCase)(canFindClass = ooParadigm.constructorCapabilities.canFindClassInConstructor, canAddImport = ooParadigm.constructorCapabilities.canAddImportInConstructor, canResolveImport = ooParadigm.constructorCapabilities.canResolveImportInConstructor))
        } yield ()
      }
    } yield ()
  }

  override def registerTypeMapping(domain: GenericModel): Generator[ProjectContext, Unit] = {

    import paradigm.projectCapabilities.addTypeLookupForMethods
    import ooParadigm.projectCapabilities.addTypeLookupForClasses
    import ooParadigm.projectCapabilities.addTypeLookupForConstructors

    import ooParadigm.methodBodyCapabilities.canFindClassInMethod             // These three all are needed
    import ooParadigm.classCapabilities.canFindClassInClass
    import ooParadigm.constructorCapabilities.canFindClassInConstructor

    val dtpeRep = TypeRep.DataType(domain.baseDataType)
    for {

      _ <- addTypeLookupForMethods(dtpeRep, domainTypeLookup(domain))
      _ <- addTypeLookupForClasses(dtpeRep, domainTypeLookup(domain))
      _ <- addTypeLookupForConstructors(dtpeRep, domainTypeLookup(domain))
    } yield ()
  }

  def addOperationsAsClasses(operations:Seq[Operation], model:GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]) : Generator[ProjectContext,Unit] = {
    import ooParadigm.projectCapabilities._
    import paradigm.projectCapabilities._

    for {
      _ <- forEach(operations) { op => {
        addClassToProject(makeExtensibleOperationImplementation(model, op, domainSpecific), visitorClassName(model, op).get : _ *)
      }}
    } yield ()
  }

  def makeDerivedClassesInChronologicalOrder(gdomain:GenericModel) : Generator[ProjectContext, Unit] = {
    for {
      _ <- forEach(gdomain.inChronologicalOrder) { dm =>

        for {
          _ <- registerNewlyDeclaredDataTypeClasses(dm)
          _ <- forEach (dm.typeCases) {tpeCase =>
            for {
            _ <- visitor.makeDerived (gdomain.baseDataType, tpeCase, dm)
            } yield ()
          }
        } yield ()
      }
    } yield ()
  }
  
  def makeOperationsClassesInChronologicalOrder(gdomain:GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]) : Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    for {
      // WHEN new data types are added and there are existing operations in the past
      // need to run generators in sequence and when that happens you need to group with a for {...} yield()
      _ <- forEach (gdomain.inChronologicalOrder) { m => {
        for {
          _ <- registerNewlyDeclaredDataTypeClasses(m)

          // If a new Extensible Visitor interface is required, add the interface AND THEN for all operations
          // add their corresponding classes.
          _ <- if (m == latestModelDefiningVisitor(m)) {
            for {
              _ <- addClassToProject(makeExtensibleVisitorInterface(m), visitorInterfaceName(m) : _ *)
              _ <- addOperationsAsClasses(m.flatten.ops, m, domainSpecific)
            } yield ()
          } else {
            // For overridden operations that were defined IN THE PAST have to add those to m.ops
            val flat = m.flatten
            val over = flat.typeCases.flatMap{ tpe =>
              flat.ops.filter{ op =>
                domainSpecific.evolutionSpecificDependencies(
                  PotentialRequest(m.baseDataType, tpe, op)
                ).contains(m)
              }
            }.toSet

            addOperationsAsClasses(m.ops ++ over, m, domainSpecific)   // these override from past
          }
        } yield ()
      }
      }
    } yield ()
  }

  /**
   * The Extensible Visitor approach is defined as follows. This handles the code generation for the implementation
   *
   * 1. Make the base class (for the domain)
   * 2. For each of the data types (in flattened set) create a derived class
   * 3. Create the Visitor interface
   *
   * @param gdomain           final node in extension graph
   * @param domainSpecific    final eip corresponding to this final node
   * @return
   */
  override def implement(gdomain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    import paradigm.projectCapabilities._

    for {
      _ <- debug ("Processing Extensible Visitor")
      _ <- domainSpecific.initialize(this)
      _ <- visitor.makeBase(gdomain.baseDataType)
      _ <- registerTypeMapping(gdomain)
      _ <- addClassToProject(visitor.makeVisitorInterface(Seq.empty), visitor.visitorClass)   // top-level Visitor

      _ <- makeOperationsBase(gdomain)
      _ <- makeOperationsClassesInChronologicalOrder(gdomain, domainSpecific)
      _ <- makeDerivedClassesInChronologicalOrder(gdomain)
    } yield ()
  }

  /** Adds tests to the project context */
  override def implement(tests: Map[GenericModel, Seq[TestCase]], testImplementationProvider: TestImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    import paradigm.projectCapabilities._
    import paradigm.compilationUnitCapabilities._
    import paradigm.testCapabilities._
    import ooParadigm.testCapabilities.addMethod
    for {
      _ <-
        forEach(tests.toList) { case (model, tests) =>
          val testCode: Generator[MethodBodyContext, Seq[Expression]] =
            for {
              code <- forEach(tests) {
                test => testImplementationProvider.test(this)(test)
              }
            } yield code.flatten

          val compUnit = for {
            // add test case first
            _ <- addTestCase(testCode, testName)

            // each operation gets a factory that is added using the 'addMethodInTest' capabilities.
            _ <- forEach (model.flatten.ops.distinct) { op => {
              for {
                _ <- addMethod(factory.name(op), makeFactoryOperationImpl(model, op, shouldOverride = false,
                  visitorClassName(latestModelDefiningOperation(model, op).head, op).get))
              } yield ()
            }}

          } yield()

          val testSuite = for {
            _ <- addTestSuite(
              testCaseName(model),
              compUnit
            )
          } yield ()

          addCompilationUnit(
            testSuite,
            testCaseName(model)
          )

        }
    } yield ()
  }
}

object ExtensibleVisitor {
  type WithParadigm[P <: AnyParadigm] = ExtensibleVisitor { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   oo: ObjectOriented.WithBase[base.type],
   params: ParametricPolymorphism.WithBase[base.type])
  (generics: Generics.WithBase[base.type,oo.type,params.type]): ExtensibleVisitor.WithParadigm[base.type] =
    new ExtensibleVisitor {
      val paradigm: base.type = base
      val names: NameProvider[paradigm.syntax.Name] = nameProvider
      val ooParadigm: oo.type = oo
      val polymorphics: params.type = params
      val genericsParadigm: generics.type = generics
    }
}
