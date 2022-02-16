package org.combinators.ep.approach.oo    /*DI:LI:AD*/

import org.combinators.ep.domain.{GenericModel, Model}
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
 * Original paper had no advice on merge case, so following is missing from j5j8
 *
     public ep.j4.IsPower makeIsPower(Exp left, Exp right) {
        return new IsPower(left, right);
    }
 */
trait ExtensibleVisitor extends OOApproachImplementationProvider with SharedOO with OperationAsClass {
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
     * @param model
     * @param op
     * @param typeName
     * @return
     */
    def create(model:GenericModel, op:Operation, typeName:Seq[Name]): Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      for {
        _ <- addMethod(name(op), makeFactoryOperationImpl(model, op, typeName))
      } yield ()
    }

    // TODO: would love to avoid duplicating contexts
    def createTest(model:GenericModel, op:Operation, typeName:Seq[Name]): Generator[TestContext, Unit] = {
      import ooParadigm.testCapabilities._
      for {
        _ <- addMethod(name(op), makeFactoryOperationImpl(model, op, typeName))
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
      * @param tpe
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
    * @param op
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
         initializeParent(args.map(p => p._3))
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
   * @param message
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
  def makeFactoryOperationImpl(model:GenericModel, op: Operation, typeName:Seq[Name]): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      earliestOpClass <- findClass(visitorClassName(model.findOperation(op).get, op).get : _ *)
      _ <- resolveAndAddImport(earliestOpClass)
      _ <- setReturnType(earliestOpClass)
      latestOpClass <- findClass(typeName : _ *)
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
   * @param domain
   * @return
   */
  def makeExtensibleVisitorInterface(domain:GenericModel): Generator[ClassContext, Unit] = {
    // ignore degenerate case where the first model only has an operation without any types
    def addParentInterfaces(): Generator[ClassContext, Unit] =  {
      import ooParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._

      val parentModels = domain.former.map(m => latestModelDefiningVisitor(m))

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
    for {
      returnType <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(returnType)
      _ <- makeEachVisitSignature(domain, tpeCase, returnType)
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
      result <-
        domainSpecific.logic(this)(
          ReceivedRequest(
            tpe,
            tpeCase,
            visitedRef,
            tpeCase.attributes.zip(attAccessors).toMap,
            Request(op, args.toMap)
          )
        )
    } yield result
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
   * @param op
   * @param domainSpecific
   * @return           Returns class context without actually adding to ProjectContext; this is job of caller of this function
   */
  def makeExtensibleOperationImplementation(domain:GenericModel,
        op: Operation,
        domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {

    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._

    val allPast = domain.former.flatMap(m => latestModelDefiningOperation(m, op))
    val previous = allPast.filterNot(m => allPast.exists(mnewer => m.before(mnewer)))

    // only capture new ones. Make sure to apply distinct to handle merging
    val typeCasesToDeclare = if (previous.isEmpty) {
      domain.flatten.typeCases.distinct
    } else {
      val primaryParentTypeCases = previous.maxBy(m => m.flatten.typeCases.length).flatten.typeCases
      domain.flatten.typeCases.filterNot(tpe => primaryParentTypeCases.contains(tpe)).distinct
    }

    // ignore degenerate case where the first model only has an operation without any types
    // if you get the last model with data types, you should stop if op is defined AFTER
    // that one (for example with PrettyP which is defined in M2 but Sub is defined in M1)
    def addParentClass(): Generator[ClassContext, Option[Type]] = if (domain.isDomainBase) {
        Command.lift[ClassContext, Option[Type]](Option.empty)
      } else {
        val latestVisitor = latestModelDefiningVisitor(domain)

        for {
          visitTyParam <- toTargetLanguageType(op.returnType) // can do this because we need this interfaces paramType
          _ <- resolveAndAddImport(visitTyParam)
          visitorClassType <- findClass(visitorInterfaceName(latestVisitor) : _ *)
          _ <- resolveAndAddImport(visitorClassType)

          modifiedType <- applyType(visitorClassType, Seq(visitTyParam))
          _ <- addImplemented(modifiedType)

          possibleParent <- if (previous.nonEmpty) {
            val primaryParent = previous.maxBy(m => m.flatten.typeCases.length)
            for {
              parentType <- findClass(visitorClassName(primaryParent, op).get : _ *)
              _ <- resolveAndAddImport(parentType)
              _ <- addParent(parentType)
            } yield Some(parentType)
          }  else {
            Command.lift[ClassContext,Option[Type]](Option.empty)
          }

        } yield possibleParent
      }

    // have to be careful with merging, where dependent operations have to be managed
    val allDependentOps = domain.flatten.typeCases.distinct.flatMap(tpeCase => {
      if (domain.findTypeCase(tpeCase).isDefined && !domain.typeCases.contains(tpeCase)) {    // NO NEED TO DUPLICATE CURRENT ONES...
        domainSpecific.dependencies(op, tpeCase).filter(op => domain.findOperation(op).isDefined)
      } else {
        Seq.empty
      }
    }).distinct

    // get all formers that are NOT latest visitors, and then take those operations and throw out those that are already supported...
   // val latestVisitor = latestModelDefiningVisitor(domain). This case is clearly not covered in the original expression problem paper.
    val otherBranchOps = if (previous.isEmpty) {
      Seq.empty
    } else {
      val primaryParent = previous.maxBy(m => m.flatten.typeCases.length)
     // might be too far in the past, but want the most direct
     // former that leads to that branch
     val latestInPrimaryParentBranch = domain.former.foldLeft(primaryParent)((latest,m) => latest.later(m))
     domain.former.filterNot(p => p == latestInPrimaryParentBranch).flatMap(m => m.flatten.ops.filterNot(op => latestInPrimaryParentBranch.supports(op)))
    }

    for {
      possibleParent <- addParentClass()

      // instead of directly accessing [operationClass]
      _ <- if (possibleParent.isEmpty) {
        addParamFields(op)
      } else {
        Command.skip[ClassContext]
      }

      _ <- addConstructor(visitor.makeOperationConstructor(op, possibleParent))

      // for all that are not in primary parent
      _ <- forEach (typeCasesToDeclare) { tpe =>
        for {
          _ <- addMethod(visitor.visit, makeEachImplementation(domain, domain.baseDataType, tpe, op, domainSpecific))

          // if dependent operations exist, those factories need to be generated as well...
        } yield ()
      }

      // have to take LATEST version, which means the latest of either (a) where op was defined; or (b)
      // most recently defined model with data types. When trying to call another operation, you need.
      // might have to go backward in time to find the model that declared that operation or had
      // an intervening visitor declaration.
      _ <- forEach((allDependentOps ++ otherBranchOps ++ domain.ops :+ op).distinct) { dependentOp =>
          val modelsToUse = latestModelDefiningOperation(domain, dependentOp)
          val modelToUse = modelsToUse.head

          factory.create(modelToUse, dependentOp, visitorClassName(modelToUse, dependentOp).get)
        }

      //_ <- factory.create(domain, op, visitorClassName(domain, op).get)
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
   * @param model
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
   * @param dtpe
   * @param canFindClass
   * @tparam Ctxt
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

  /**
   * The Extensible Visitor approach is defined as follows. This handles the code generation for the implementation
   *
   * 1. Make the base class (for the domain)
   * 2. For each of the data types (in flattened set) create a derived class
   * 3. Create the Visitor interface
   *
   * @param gdomain
   * @param domainSpecific
   * @return
   */
  override def implement(gdomain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    import paradigm.projectCapabilities._

    gdomain.inChronologicalOrder.foreach(_.output)

    for {
      _ <- debug ("Processing Extensible Visitor")
      _ <- domainSpecific.initialize(this)
      _ <- visitor.makeBase(gdomain.baseDataType)
      _ <- registerTypeMapping(gdomain)
      _ <- addClassToProject(visitor.makeVisitorInterface(Seq.empty), visitor.visitorClass)   // top-level Visitor

      _ <- makeOperationsBase(gdomain)

      // WHEN new data types are added and there are existing operations in the past
      // need to run generators in sequence and when that happens you need to group with a for {...} yield()
      _ <- forEach (gdomain.inChronologicalOrder) { m => {
          for {
            _ <- forEach (m.typeCases) { tpeCase =>
              for {
                _ <- visitor.makeDerived(gdomain.baseDataType, tpeCase, m)
              } yield ()
            }

            // now that type classes are declared, they can be registered (chicken-and-egg problem)
            _ <- registerNewlyDeclaredDataTypeClasses(m)

            // If a new Extensible Visitor interface is required, add the interface AND THEN for all operations
            // add their corresponding classes.
            _ <- if (m == latestModelDefiningVisitor(m)) {
              for {
                _ <- addClassToProject(makeExtensibleVisitorInterface(m), visitorInterfaceName(m) : _ *)
                _ <- forEach(m.flatten.ops) { op => {
                  addClassToProject(makeExtensibleOperationImplementation(m, op, domainSpecific), visitorClassName(m, op).get : _ *)
                }}
              } yield ()
            } else {
              for {
                _ <- forEach(m.ops) { op =>
                  addClassToProject(makeExtensibleOperationImplementation(m, op, domainSpecific), visitorClassName(m, op).get : _ *)
                }
              } yield ()
            }
          } yield ()
        }
      }

      _ <- forEach (gdomain.inChronologicalOrder) { dm =>
            forEach (dm.typeCases) { tpeCase =>
              for {
                _ <- visitor.makeDerived(gdomain.baseDataType, tpeCase, dm)
                _ <- registerNewlyDeclaredDataTypeClasses(dm)
              } yield ()
            }
        }
    } yield ()
  }

  /** Adds tests to the project context */
  override def implement(tests: Map[GenericModel, Seq[TestCase]], testImplementationProvider: TestImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    // TODO: import projectCapabilities._
    import paradigm.projectCapabilities._
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

          val compUnit = for {
            // add test case first
            _ <- addTestCase(testCode, testName)

            _ <- forEach (model.flatten.ops.distinct) { op => {
              for {
                _ <- factory.createTest (model, op, visitorClassName(latestModelDefiningOperation(model, op).head, op).get)
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
