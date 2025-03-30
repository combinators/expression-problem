package org.combinators.ep.approach.oo    /*DI:LI:AD*/

import org.combinators.ep.domain.GenericModel
import org.combinators.ep.domain.abstractions.{Attribute, DataType, DataTypeCase, Operation, Parameter, TypeRep}
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.generator.{AbstractSyntax, Command, EvolutionImplementationProvider, NameProvider}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.communication.{PotentialRequest, ReceivedRequest, Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.control.Imperative.WithBase
import org.combinators.ep.generator.paradigm.{AnyParadigm, Generics, ObjectOriented, ParametricPolymorphism}

/**
 * Straightforward implementation places all generated code in the current ep.* package.
 */
trait Visitor extends SharedOO with OperationAsClass { self =>
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val visitorSpecifics: VisitorSpecifics

  import paradigm._
  import ooParadigm._
  import syntax._

  // necessary constants used to ensure no typos
  lazy val accept: Name = names.mangle("accept")
  lazy val visit: Name = names.mangle("visit")
  lazy val visitorClass: Name = names.mangle("Visitor")
  val visitorParameter: String = "v"
  val expParameter: String = "exp"

  /** Visitor is the first parameter in the argument list. */
  def targetVisitor: Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._

    for {
      args <- getArguments()
      visitedRef <- getArguments().map(_.head._3)
    } yield visitedRef
  }

  /**
   * The Visitor-specific implementation knows how to dispatch.
   */
  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] =
    visitorSpecifics.dispatch(message)

  /**
   * Override SharedOO by delegating to visitor-specific implementation
   */
   override def makeTypeCaseImplementation(tpe: DataType, tpeCase: DataTypeCase, op: Operation, model:GenericModel,
                         domainSpecific: EvolutionImplementationProvider[this.type]): Generator[MethodBodyContext, Option[Expression]] =
    visitorSpecifics.makeTypeCaseImplementation(tpe, tpeCase, op, model, domainSpecific)

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
      rt <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(rt)
      res <- instantiateObject(rt, args)
    } yield res
  }

  trait VisitorSpecifics {
    def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression]

    // different based on side effect vs. return
    def makeTypeCaseImplementation(tpe: DataType, tpeCase: DataTypeCase, op: Operation, model: GenericModel,
                           domainSpecific: EvolutionImplementationProvider[self.type]): Generator[MethodBodyContext, Option[Expression]]

    // must be provided by the appropriate extension
    def makeAcceptImplementation(model: GenericModel): Generator[ClassContext, Unit]

    // will need a visit method
    def makeOperationImplementation(domain:GenericModel, op: Operation, domainSpecific: EvolutionImplementationProvider[self.type]): Generator[ClassContext, Unit]

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
    def makeAcceptSignatureWithType(): Generator[MethodBodyContext, Unit]

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
    def makeVisitorInterface(allTypes:Seq[DataTypeCase]): Generator[ClassContext, Unit]
  }

  /**
   * Access attributes using default getter methods via argument to method, exp
   *
   * @param attribute    Data Type Case attribute to be accessed
   * @return
   */
  def attributeVisitorAccess(attribute:Attribute, tpeCase: DataTypeCase, domain:GenericModel, baseType:Option[paradigm.syntax.Type]) : Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      args <- getArguments()
      expRef <- getArguments().map(_.head._3)
      getterMethod <- getMember(expRef, getterName(attribute))
      getterCall <- apply(getterMethod, Seq.empty)
    } yield getterCall
  }

  /** Default argument access. */
  def argumentVisitorAccess(arg:(Name, Type, Expression), param:Parameter, domain: GenericModel, baseType: Option[paradigm.syntax.Type] = None): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    // Properly cast all Base arguments to designated baseType (which was used in the method signature)
    for {
      //thisRef <- selfReference()
      args <- getArguments()
      expRef <- getArguments().map(_.head._3)
      paramField <- getMember(expRef, names.mangle(param.name))
    } yield paramField
  }

  def targetExp : Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      args <- getArguments()
      expRef <- getArguments().map(_.head._3)
    } yield expRef
  }

  trait GenericVisitor extends VisitorSpecifics {
    val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
    val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]
    val visitTypeParameter: String = "R"

    /** Make a method body for each operation, which is a visit method for a defined data type
     *
     * {{{
     *     public Double visit(Sub e) {
     *         return e.getLeft().accept(new Eval()) - e.getRight().accept(new Eval());
     *     }
     * }}}
     *
     * Access the results via a visitor method which returns the information using accept method.
     *
     * @param tpe               The Base Data Type.
     * @param tpeCase           And the Specific Data Type Case.
     * @param op                Where there is an operation.
     * @param domainSpecific    And the logic for the operation is contained in the EIP.
     * @return                  So return implementation of the method.
     */
    override def makeTypeCaseImplementation(tpe: DataType, tpeCase: DataTypeCase, op: Operation, model: GenericModel,
            domainSpecific: EvolutionImplementationProvider[self.type]): Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._

      val properModel = latestModelDefiningOperatorClass(model, tpeCase, op,  domainSpecific).get

      for {
        returnType <- toTargetLanguageType(op.returnType)
        _ <- resolveAndAddImport(returnType)
        _ <- makeVisitSignature(tpeCase, returnType)

        result <- completeImplementationFromParameters(tpe, tpeCase, op, properModel, domainSpecific, target=targetExp, attributeAccess=attributeVisitorAccess)

      } yield result
    }

    /**
     * Dispatch in visitor we need to find context on which to accept a visitor.
     *
     * That is, e.getLeft().accept(new Eval()) + e.getRight().accept(new Eval());
     *
     * In particular, when dispatching an operation (with parameters) on an expression,
     * we want to return something like this:
     *
     * $expr.accept(new ${op.concept}($args))
     *
     * @param message   The SendRequest to be dispatched.
     * @return
     */
    def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
      import ooParadigm.methodBodyCapabilities._
      import paradigm.methodBodyCapabilities._
      import polymorphics.methodBodyCapabilities._
      for {

        // In the 'message.to' expression, invoke the 'accept' method with a visitor argument
        genericMethod <- getMember(message.to, accept)   // things which are code-generated use the '<-' handles unpacking results
        rt <- toTargetLanguageType(message.request.op.returnType)
        _ <- resolveAndAddImport(rt)
        method <- instantiateTypeParameter(genericMethod, Seq(rt))

        // the operation is encoded in its own class, which we must find to determine the visitor type
        op = message.request.op
        visitorType <- findClass(names.mangle(names.conceptNameOf(op)))     // each visitor is named based on operation
        _ <- resolveAndAddImport(visitorType)            // gives resulting import statement (if needed)

        // construct the visitor object for the given type (and parameters)
        visitor <- instantiateObject(visitorType, op.parameters.map(param => message.request.arguments(param)))

        // apply to method with the visitor, resulting in the expression
        result <- apply(method, Seq(visitor))           // has to be Seq[] just for syntax of calling the method
      } yield result
    }

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
        _ <- setParameters(Seq((visitParam, visitorType))) // a pair (name,type) of only one sequence
      } yield ()
    }

    /** Create an accept implementation from the accept method signature.
     * {{{
     *  public <R> R accept(Visitor<R> v) {
     *     return v.visit(this);
     * }
     * }}}
     * @return
     */
    def makeAcceptImplementation(model:GenericModel): Generator[ClassContext, Unit] = {
      val makeBody: Generator[MethodBodyContext, Option[Expression]] = {
        import paradigm.methodBodyCapabilities._
        import ooParadigm.methodBodyCapabilities._

        import polymorphics.methodBodyCapabilities._
        for {
          // start from the accept signature and add a method body.
          _ <- makeAcceptSignatureWithType()
          args <- getArguments()   // get name, type, expression
          // invoke visit method on 'v' with 'this' as argument
          visitFunction <- getMember(args.head._3, visit)
          self <- selfReference()
          result <- apply(visitFunction, Seq(self))  // make the method invocation
        } yield Some(result)
      }

      import ooParadigm.classCapabilities._
      addMethod(accept, makeBody)
    }

    /**
     * Each operation is placed in its own class, with a 'visit' method for each known data type.
     *
     * Uses the generic 'operationClass' capability to create the structure of the class.
     *
     * {{{
     * import sun.reflect.generics.visitor.Visitor
     * class Eval extends Visitor[Double] { }
     *
     *   public Double visit(Sub e) {
     *         return e.getLeft().accept(new Eval()) - e.getRight().accept(new Eval());
     *     }
     *
     *     public Double visit(Lit e) {
     *         return e.getValue();
     *     }
     *
     *     public Double visit(Add e) {
     *         return e.getLeft().accept(new Eval()) + e.getRight().accept(new Eval());
     *     }
     *   }
     * }}}
     *
     * @param domain     Model in the Extension Graph (no longer flattened)
     * @param op         The operation that is at hand.
     * @param domainSpecific   The EIP that contains the logic.
     * @return        The one invoking this method must be sure to add this class to project.
     */
    def makeOperationImplementation(domain:GenericModel, op: Operation, domainSpecific: EvolutionImplementationProvider[self.type]): Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._
      for {
        _ <- operationClass(visit, op, domain, domain.flatten.typeCases.distinct, domain.baseDataType, domainSpecific)

        visitorInterface <- findClass(visitorClass)
        _ <- resolveAndAddImport(visitorInterface)
        returnTpe <- toTargetLanguageType(op.returnType)
        _ <- resolveAndAddImport(returnTpe)
        visitorInterfaceWithReturnType <- applyType(visitorInterface, Seq(returnTpe))
        _ <- addImplemented(visitorInterfaceWithReturnType)
      } yield ()
    }

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

 }

  trait VisitorSideEffect extends VisitorSpecifics {
    val impParadigm: Imperative.WithBase[MethodBodyContext,paradigm.type]

    object ComponentNames {
      val getValue: Name = names.mangle("getVisitorValue")
      val visitImpl: Name = names.mangle("visitImpl")
      val value: Name = names.mangle("visitorValue")     // HACK: Cannot conflict with 'value' from a datatype (i.e., Lit)
    }

    /** Make a method body for each operation, which is a visit method for a defined data type
     *
     * {{{
     *     public Double visit(Sub e) {
     *        this.visitorValue = this.visitImpl(exp);
     *     }
     * }}}
     *
     * @param tpe                The Base Data Type.
     * @param tpeCase            And the specific Data Type Case.
     * @param op                 Where there is an operation.
     * @param domainSpecific     And the EIP containing the needed logic.
     * @return                   Which returns the implementation of the visit() method.
     */
    override def makeTypeCaseImplementation(tpe: DataType, tpeCase: DataTypeCase, op: Operation, model:GenericModel,
                                     domainSpecific: EvolutionImplementationProvider[self.type]
                                    ): Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._

      for {
        unitType <- toTargetLanguageType(TypeRep.Unit)
        _ <- makeVisitSignature(tpeCase, unitType)
        thisRef <- selfReference()
        impl <- getMember(thisRef, ComponentNames.visitImpl)
        args <- getArguments().map(_.map(_._3))
        result <- apply(impl, args)

        // now need to store it. AND add those statements to the method body
        storedField <- getMember(thisRef, ComponentNames.value)
        stmt <- assignVar(storedField, result)
        _ <- addBlockDefinitions(Seq(stmt))

        // Return unit (translates to no return/void in Java, real unit return in Scala)
        result <- self.reify(InstanceRep(TypeRep.Unit)(()))
      } yield Some(result)
    }

    /**
     * Dispatch in visitor we need to find context on which to accept a visitor.
     *
     * That is, e.getLeft().accept(new Eval()) + e.getRight().accept(new Eval());
     *
     * In particular, when dispatching an operation (with parameters) on an expression,
     * we want to return something like this:
     *
     * evalLeft.getValue() + evalRight.getValue()
     *
     * but this only works if the following is also done prior to this dispatching.
     *
     * Visitor evalLeft = new Eval()
     * $expr.accept(evalLeft)
     *
     * @param message   The SendRequest to be dispatched.
     * @return
     */
    def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
      import ooParadigm.methodBodyCapabilities._
      import paradigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._

      for {
        // In the 'message.to' expression, invoke the 'accept' method with a visitor argument
        method <- getMember(message.to, accept)   // things which are code-generated use the '<-' handles unpacking results

        // the operation is encoded in its own class, which we must find to determine the visitor type
        op = message.request.op
        visitorType <- findClass(names.mangle(names.conceptNameOf(op)))     // each visitor is named based on operation
        _ <- resolveAndAddImport(visitorType)            // gives resulting import statement (if needed)

        // construct the visitor object for the given type (and parameters).
        visitor <- instantiateObject(visitorType, op.parameters.map(param => message.request.arguments(param)))
        fname <- freshName(names.mangle(names.instanceNameOf(op)))

        fvar <- declareVar(fname, visitorType, Some(visitor))

        // apply to method with the visitor, resulting in the expression
        callExpr <- apply(method, Seq(fvar))
        stmt <- liftExpression(callExpr)
        _ <- addBlockDefinitions(Seq(stmt))

        // obtain actual result expression by getting method and then invoking it (with empty seq)
        resultOfMethod <- getMember(fvar, ComponentNames.getValue)
        result <- apply(resultOfMethod, Seq.empty)

      } yield result
    }

    /** Create the accept method signature which eliminates the type parameter and ensures the accept method is of unit type.
     *
     * {{{
     *   public void accept(Visitor v)
     * }}}
     * @return
     */
    def makeAcceptSignatureWithType(): Generator[MethodBodyContext, Unit] = {
      import paradigm.methodBodyCapabilities.{toTargetLanguageType => _, _}
      import ooParadigm.methodBodyCapabilities._
      import paradigm.methodBodyCapabilities._

      for {
        unitTpe <- toTargetLanguageType(TypeRep.Unit)
        _ <- setReturnType(unitTpe)

        visitorType <- findClass(visitorClass)
        _ <- resolveAndAddImport(visitorType)

        visitParam <- freshName(names.mangle(visitorParameter))
        _ <- setParameters(Seq((visitParam, visitorType)))      // a pair (name,type) of only one sequence
      } yield ()
    }

    /** Create an accept implementation from the accept method signature.
     *
     * {{{
     *  public void accept(Visitor v) {
     *     return v.visit(this);
     *  }
     * }}}
     * @return
     */
    def makeAcceptImplementation(model:GenericModel): Generator[ClassContext, Unit] = {
      val makeBody: Generator[MethodBodyContext, Option[Expression]] = {
        import ooParadigm.methodBodyCapabilities._
        import paradigm.methodBodyCapabilities._
        import impParadigm.imperativeCapabilities._

        for {
          // start from the accept signature and add a method body.
          _ <- makeAcceptSignatureWithType()
          args <- getArguments()   // get name, type, expression

          // invoke visit method on 'v' with 'this' as argument
          visitFunction <- getMember(args.head._3, visit)
          myself <- selfReference()
          result <- apply(visitFunction, Seq(myself)) // make the method invocation
          visitationStmt <- liftExpression(result)
          _ <- addBlockDefinitions(Seq(visitationStmt))
          unit <- self.reify(InstanceRep(TypeRep.Unit)(()))
        } yield Some(unit)
      }

      import ooParadigm.classCapabilities._
      addMethod(accept, makeBody)
    }

    /**
     * Access attributes using default getter methods.
     *
     * @param attribute    Data Type Case attribute to be accessed
     * @return
     */
    def expGetterAccess(attribute:Attribute, tpeCase: DataTypeCase, domain:GenericModel, baseType:Option[paradigm.syntax.Type]) : Generator[MethodBodyContext, Expression] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._

      for {
        expRef <- getArguments().map(_.head._3)  // from the parameter to this method
        getterMethod <- getMember(expRef, getterName(attribute))
        getterCall <- apply(getterMethod, Seq.empty)
      } yield getterCall
    }

    /** Each operation is placed in its own class, with a 'visit' method for each known data type.
     *
     * {{{
     * class Eval extends Visitor<Double> {
     *
     *    Double value;
     *    public Double getValue() { return value; }
     *
     *   public Double visit(Sub e) {
     *      return e.getLeft().accept(new Eval()) - e.getRight().accept(new Eval());
     *   }
     *
     *   public Double visit(Lit e) {
     *      return e.getValue();
     *   }
     *
     *   public Double visit(Add e) {
     *      return e.getLeft().accept(new Eval()) + e.getRight().accept(new Eval());
     *   }
     * }
     * }}}
     *
     * @param domain     Model in the extension graph (no longer flattened)
     * @param op         The Operation at hand.
     * @param domainSpecific    The EIP containing the logic.
     * @return        The one invoking this method must be sure to add this class to project.
     */
    def makeOperationImplementation(domain:GenericModel, op: Operation, domainSpecific: EvolutionImplementationProvider[self.type]): Generator[ClassContext, Unit] = {

      def provideImplHere(tpeCase: DataTypeCase): Generator[MethodBodyContext, Option[Expression]] = {
        import paradigm.methodBodyCapabilities._
        for {
          returnType <- toTargetLanguageType(op.returnType)
          _ <- resolveAndAddImport(returnType)
          _ <- makeVisitSignature(tpeCase, returnType)   // changed this from generic 'makeSignature'                  // something like argumentExpAccess?
          res <- completeImplementationFromParameters(domain.baseDataType, tpeCase, op, domain, domainSpecific, target=targetExp, attributeAccess=expGetterAccess)
        } yield res
      }

      import ooParadigm.classCapabilities._
      for {
        _ <- operationClass(visit, op, domain, domain.flatten.typeCases.distinct, domain.baseDataType, domainSpecific)

        visitorInterface <- findClass(visitorClass)
        _ <- resolveAndAddImport(visitorInterface)

        returnTpe <- toTargetLanguageType(op.returnType)
        _ <- resolveAndAddImport(returnTpe)

        _ <- addField(ComponentNames.value, returnTpe)
        field <- getField(ComponentNames.value)
        _ <- addMethod(ComponentNames.getValue, returnValue(op.returnType, field))
        _ <- addImplemented(visitorInterface)

        _ <- forEach (domain.flatten.typeCases.distinct) { tpe =>
          addMethod(ComponentNames.visitImpl, provideImplHere(tpe))
        }
      } yield ()
    }

    /** Create a method implementation that simply returns field as an expression, with appropriate return type for operation.
     *
     * {{{
     *   RETURNTYPE ???() { return FIELD; }
     * }}}
     *
     * @param tpe      The TypeRep of interest.
     * @param field    Describing the type of this field.
     * @return         The method signature as appropriate for returning that expression.
     */
    def returnValue(tpe: TypeRep, field:Expression): Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._

      for {
        rt <- toTargetLanguageType(tpe)
        _ <- resolveAndAddImport(rt)
        _ <- setReturnType(rt)
      } yield Some(field)
    }

    /** Create the visitor interface.
     * Override to remove type parameters.
     * {{{
     *   public interface Visitor {
     *     public void visit(Lit lit);
     *     public void visit(Add add);
     *     ...
     *   }
     * }}}
     * */
    def makeVisitorInterface(allTypes:Seq[DataTypeCase]): Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._

      for {
        _ <- setInterface()
        unitTpe <- toTargetLanguageType(TypeRep.Unit)
        _ <- forEach (allTypes) { tpe => addAbstractMethod(visit, makeVisitSignature(tpe, unitTpe)) }
      } yield ()
    }

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
   * @param tpe     The Base Data Type.
   * @return        Created within the ProjectContext.
   */
  def makeBase(tpe: DataType): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val makeClass: Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      for {
        _ <- setAbstract()
        _ <- addAbstractMethod(accept, visitorSpecifics.makeAcceptSignatureWithType())
      } yield ()
    }

    // adds the 'Exp' class, with a single accept method
    addClassToProject( makeClass, names.mangle(names.conceptNameOf(tpe)))
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
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      _ <- setReturnType(visitResultType)
      visitedClassType <- findClass(names.mangle(names.conceptNameOf(tpe)))
      _ <- resolveAndAddImport(visitedClassType)
      visitParamName <- freshName(names.mangle(expParameter))
      _ <- setParameters(Seq((visitParamName, visitedClassType)))      // a pair (name,type) of only one sequence
    } yield ()
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
        _ <- visitorSpecifics.makeAcceptImplementation(model)
      } yield ()
    }
    addClassToProject(makeClass, names.mangle(names.conceptNameOf(tpeCase)))
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

  /**
   * The Visitor approach is defined as follows
   *
   * 1. Make the base class (for the domain)
   * 2. For each of the data types (in flattened set) create a derived class
   * 3. Create the Visitor interface
   *
   * @param gdomain            The desired GenericModel.
   * @param domainSpecific     The top-level EIP with all logic.
   * @return                   Returns the whole project.
   */
  def implement(gdomain: GenericModel, domainSpecific: EvolutionImplementationProvider[self.type]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    import paradigm.projectCapabilities._

    val flatDomain = gdomain.flatten
    for {
      _ <- debug ("Processing Visitor")
      _ <- registerTypeMapping(flatDomain)
      _ <- domainSpecific.initialize(self)
      _ <- makeBase(flatDomain.baseDataType)
      _ <- forEach (flatDomain.typeCases.distinct) { tpeCase =>
        makeDerived(flatDomain.baseDataType, tpeCase, gdomain) 
      }

      _ <- addClassToProject(visitorSpecifics.makeVisitorInterface(flatDomain.typeCases.distinct), visitorClass)

      // Figure out which model to use for this operation so it aligns with EIPS. In fact, sending flatDomain is exactly wrong
      // if two predecessors (and cannot decide which one to take) then we are in charge otherwise we pick one branch that has latest one
      _ <- forEach (flatDomain.ops) { op => {
        addClassToProject(visitorSpecifics.makeOperationImplementation(gdomain, op, domainSpecific), names.mangle(names.conceptNameOf(op)))
      }
      }
    } yield ()
  }
}

object Visitor {
  type WithParadigm[P <: AnyParadigm] = Visitor { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   oo: ObjectOriented.WithBase[base.type],
   parametricPolymorphism: ParametricPolymorphism.WithBase[base.type])
  (generics: Generics.WithBase[base.type, oo.type, parametricPolymorphism.type]): Visitor.WithParadigm[base.type] =
    new Visitor {
      val paradigm: base.type = base
      val names: NameProvider[paradigm.syntax.Name] = nameProvider
      val ooParadigm: oo.type = oo

      val visitorSpecifics = new GenericVisitor {
        val polymorphics: parametricPolymorphism.type = parametricPolymorphism
        val genericsParadigm: generics.type = generics
      }
    }

  def withSideEffects[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   imp: Imperative.WithBase[base.MethodBodyContext, base.type],
   oo: ObjectOriented.WithBase[base.type]): Visitor.WithParadigm[base.type] =
    new Visitor {
      val paradigm: base.type = base
      val names: NameProvider[paradigm.syntax.Name] = nameProvider
      val ooParadigm: oo.type = oo

      val visitorSpecifics: VisitorSpecifics = new VisitorSideEffect {
        override val impParadigm: WithBase[paradigm.MethodBodyContext, paradigm.type] = imp
      }
    }
}
