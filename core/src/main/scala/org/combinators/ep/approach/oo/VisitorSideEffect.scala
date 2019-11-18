package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._
import org.combinators.ep.generator.paradigm._
import org.combinators.ep.generator.paradigm.control.Imperative

/**
 * Using visitor approach.
 *
 * Visitor implementation with side effects.
 */
abstract class VisitorSideEffect extends ApproachImplementationProvider with SharedVisitor {

  import ooParadigm._
  import paradigm._
  import syntax._
  val impParadigm: Imperative.WithBase[MethodBodyContext,paradigm.type]

  lazy val getValue: Name = names.mangle("getValue")
  lazy val value: Name = names.mangle("value")


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
   * @param message
   * @return
   */
  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._
    import polymorphics.methodBodyCapabilities._

    for {
      // In the "message.to" expression, invoke the 'accept' method with a visitor argument
      method <- getMember(message.to, accept)   // things which are code-generated use the '<-' handles unpacking results

      // the operation is encoded in its own class, which we must find to determine the visitor type
      op = message.request.op
      visitorType <- findClass(names.mangle(names.conceptNameOf(op)))     // each visitor is named based on operation
      _ <- resolveAndAddImport(visitorType)            // gives resulting import statement (if needed)

      // construct the visitor object for the given type (and parameters).
      visitor:Expression <- instantiateObject(visitorType, op.parameters.map(param => message.request.arguments(param)))
      fname <- freshName(names.mangle(names.instanceNameOf(op)))

      fvar <- declareVar(fname, visitorType, Some(visitor))

      // apply to method with the visitor, resulting in the expression
      callExpr <- apply(method, Seq(fvar))
      stmt <- liftExpression(callExpr)
      _ <- addBlockDefinitions(Seq(stmt))

      // obtain actual result expression by getting method and then invoking it (with empty seq)
      resultOfMethod <- getMember(fvar,  getValue)
      result <- apply(resultOfMethod, Seq.empty)

    // given result, call (and return) getValue()
    } yield result
  }

  /**
   * Instantiates an instance of the domain object.
   *
   * Same implementation for OO as for visitor.
   *
   * new Add(new Lit(new Double(1.0)), new Lit(new Double(2.0)))
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
      // access the constructor for the class associated with type case and invoke constructors with arguments.
      rt <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(rt)
      res <- instantiateObject(rt, args)
    } yield res
  }

  /** Create the accept method signature.
    * Override to remove type parameter.
    * {{{
    *   public void accept(Visitor v)
    * }}}
    * @return
    */
  override def makeAcceptSignature(): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities.{toTargetLanguageType => _, _}
    import polymorphics.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      unitTpe <- toTargetLanguageType(TypeRep.Unit)
      _ <- setReturnType(unitTpe)

      // identify Visitor<R>
      visitorType <- findClass(visitorClass)
      _ <- resolveAndAddImport(visitorType)

      visitParam <- freshName(names.mangle(visitorParameter))
      _ <- setParameters(Seq((visitParam, visitorType)))      // a pair (name,type) of only one sequence
    } yield ()
  }


  /** Create an accept implementation from the accept method signature.
   * {{{
   *  public void accept(Visitor v) {
   *     return v.visit(this);
   * }
   * }}}
   * @return
   */
  def makeAcceptImplementation(model:Model): Generator[ClassContext, Unit] = {
    val makeBody: Generator[MethodBodyContext, Option[Expression]] = {
      import ooParadigm.methodBodyCapabilities._
      import paradigm.methodBodyCapabilities._
      import impParadigm.imperativeCapabilities._

      for {
        // start from the accept signature and add a method body.
        _ <- makeAcceptSignature()
        args <- getArguments()   // get name, type, expression

        // invoke visit method on 'v' with 'this' as argument
        visitFunction <- getMember(args.head._3, visit)
        self <- selfReference()
        result <- apply(visitFunction, Seq(self)) // make the method invocation
        visitationStmt <- liftExpression(result)
        _ <- addBlockDefinitions(Seq(visitationStmt))
        unit <- this.reify(InstanceRep(TypeRep.Unit)(()))
      } yield Some(unit)
    }

    import ooParadigm.classCapabilities._
    addMethod(accept, makeBody)
  }


  // TODO: Update documentation
  /** Each operation is placed in its own class, with a 'visit' method for each known data type.
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
   * @param domain     Model for which all types are to be incorporated
   * @param op
   * @param domainSpecific
   * @return        The one invoking this method must be sure to add this class to project.
   */
  def makeOperationImplementation(domain:Model,
                                  op: Operation,
                                  domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    val makeClass: Generator[ClassContext, Unit] = {
      import genericsParadigm.classCapabilities._
      import ooParadigm.classCapabilities._
      for {
        visitorInterface <- findClass(visitorClass)
        _ <- resolveAndAddImport(visitorInterface)

        returnTpe <- toTargetLanguageType(op.returnType)
        _ <- resolveAndAddImport(returnTpe)

        _ <- addField(value, returnTpe)
        field <- getField(value)
        _ <- addMethod(getValue, returnValue(op, field))

        _ <- addImplemented(visitorInterface)
        _ <- addConstructor(makeOperationConstructor(op))
        _ <- forEach (domain.typeCases) { tpe =>
          addMethod(visit, makeImplementation(domain.baseDataType, tpe, op, domainSpecific))
        }
      } yield ()
    }

    makeClass
  }

  def returnValue(op: Operation, field:Expression): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for {
      rt <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(rt)
      _ <- setReturnType(rt)
      params <- forEach (op.parameters) { param: Parameter =>
        for {
          pt <- toTargetLanguageType(param.tpe)
          _ <- resolveAndAddImport(pt)
          pName <- freshName(names.mangle(param.name))
        } yield (pName, pt)
      }
      _ <- setParameters(params)
    } yield (Some(field))
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
  override def makeVisitorInterface(allTypes:Seq[DataTypeCase]): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._

    for {
      _ <- setInterface()
      unitTpe <- toTargetLanguageType(TypeRep.Unit)
      _ <- forEach (allTypes) { tpe => addAbstractMethod(visit, makeVisitSignature(tpe, unitTpe)) }
    } yield ()
  }



  /** Make a method body for each operation, which is a visit method for a defined data type
   *
   * {{{
   *     public Double visit(Sub e) {
   *         Eval evalleft = new Eval();
   *         e.getLeft().accept(evalleft);
   *         Eval evalright = new Eval();
   *         e.getRight().accept(evalright);
   *         return evalleft.getValue() - evalright.getValue();
   *     }
   * }}}
   *
   * @param tpe
   * @param tpeCase
   * @param op
   * @param domainSpecific
   * @return
   */
  override def makeImplementation(tpe: DataType,
                         tpeCase: DataTypeCase,
                         op: Operation,
                         domainSpecific: EvolutionImplementationProvider[this.type]
                        ): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._

    for {
      unitType <- toTargetLanguageType(TypeRep.Unit)
      _ <- makeVisitSignature(tpeCase, unitType)
      visitedRef <- getArguments().map(_.head._3)
      attAccessors: Seq[Expression] <- forEach (tpeCase.attributes) { att =>
        for {
          getter <- getMember(visitedRef, names.addPrefix("get", names.mangle(names.conceptNameOf(att))))
          getterCall <- apply(getter, Seq.empty)
        } yield getterCall
      }

      thisRef <- selfReference()
      args <- forEach (op.parameters) { param =>
        for {
          paramField <- getMember(thisRef, names.mangle(param.name))
        } yield (param, paramField)
      }

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

      // now need to store it. AND add those statements to the method body
      storedField <- getMember(thisRef, value)
      stmt <- assignVar(storedField, result.get)
      _ <- addBlockDefinitions(Seq(stmt))

      // Return unit (translates to no return/void in Java, real unit return in Scala)
      result <- this.reify(InstanceRep(TypeRep.Unit)(()))
    } yield Some(result)
  }

  def domainTypeLookup[Ctxt](dtpe: DataType)(implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]]): Generator[Ctxt, Type] = {
    FindClass(names.mangle(names.conceptNameOf(dtpe))).interpret(canFindClass)
  }

  def initializeApproach(domain: Model): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    import paradigm.projectContextCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import ooParadigm.classCapabilities._
    import ooParadigm.constructorCapabilities._
    val dtpeRep = TypeRep.DataType(domain.baseDataType)
    for {
      _ <- addTypeLookupForMethods(dtpeRep, domainTypeLookup(domain.baseDataType))
      _ <- addTypeLookupForClasses(dtpeRep, domainTypeLookup(domain.baseDataType))
      _ <- addTypeLookupForConstructors(dtpeRep, domainTypeLookup(domain.baseDataType))
    } yield ()
  }

  /**
   * The Visitor approach is defined as follows
   *
   * 1. Make the base class (for the domain)
   * 2. For each of the data types (in flattened set) create a derived class
   * 3. Create the Visitor interface
   *
   * @param domain
   * @param domainSpecific
   * @return
   */
  def implement(domain: Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val flatDomain = domain.flatten
    for {
      _ <- initializeApproach(flatDomain)
      _ <- domainSpecific.initialize(this)
      _ <- makeBase(flatDomain.baseDataType)
      _ <- addClassToProject(visitorClass, makeVisitorInterface(flatDomain.typeCases))

      _ <- forEach (flatDomain.typeCases) { tpeCase =>
        makeDerived(flatDomain.baseDataType, tpeCase, domain)   // used to have flatDomain.ops,
      }  // parentType: DataType, tpeCase: DataTypeCase, model: Model

      _ <- forEach (flatDomain.ops) { op =>
        addClassToProject(names.mangle(names.conceptNameOf(op)), makeOperationImplementation(flatDomain, op, domainSpecific))
      }
    } yield ()
  }
}


object VisitorSideEffect {
  type WithParadigm[P <: AnyParadigm] = VisitorSideEffect { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   imp: Imperative.WithBase[base.MethodBodyContext, base.type],
   oo: ObjectOriented.WithBase[base.type],
   parametricPolymorphism: ParametricPolymorphism.WithBase[base.type])
  (generics: Generics.WithBase[base.type, oo.type, parametricPolymorphism.type]): VisitorSideEffect.WithParadigm[base.type] =
    new VisitorSideEffect {
      val paradigm: base.type = base
      val names: NameProvider[paradigm.syntax.Name] = nameProvider
      val impParadigm: imp.type = imp
      val ooParadigm: oo.type = oo
      val polymorphics: parametricPolymorphism.type = parametricPolymorphism
      val genericsParadigm: generics.type = generics
    }
}
