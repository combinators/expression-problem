package org.combinators.ep.approach.oo    /*DI:LI:AD*/

import org.combinators.ep.domain.{GenericModel, Model}
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._
import org.combinators.ep.generator.paradigm._
import org.combinators.ep.generator.paradigm.control.Imperative

/** In the Visitor family, there is a variation in which the visitor implementation computes a side effect value
 * instead of returning the value as part of the visit method.
 *
 * This is done by overriding [[makeAcceptSignatureWithType]] to remove the type parameter. The result is that
 * the [[dispatch]] method is converted to compute expressions over the side effect value computed when accepting
 * a visitor.
 */
abstract class VisitorSideEffect extends OOApproachImplementationProvider with SharedVisitor {

  import ooParadigm._
  import paradigm._
  import syntax._
  val impParadigm: Imperative.WithBase[MethodBodyContext,paradigm.type]

  lazy val getValue: Name = names.mangle("getValue")
  lazy val visitImpl: Name = names.mangle("visitImpl")
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
      resultOfMethod <- getMember(fvar,  getValue)
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
  override def makeAcceptSignatureWithType(): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities.{toTargetLanguageType => _, _}
    import polymorphics.methodBodyCapabilities._
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
  def makeAcceptImplementation(model:Model): Generator[ClassContext, Unit] = {
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
   * @param domain     Model for which all types are to be incorporated
   * @param op
   * @param domainSpecific
   * @return        The one invoking this method must be sure to add this class to project.
   */
  def makeOperationImplementation(domain:Model,
                                  op: Operation,
                                  domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {

    import ooParadigm.classCapabilities._
    for {
      _ <- operationClass(visit, op, domain.typeCases, domain.baseDataType, domainSpecific)

      visitorInterface <- findClass(visitorClass)
      _ <- resolveAndAddImport(visitorInterface)

      returnTpe <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(returnTpe)

      _ <- addField(value, returnTpe)
      field <- getField(value)
      _ <- addMethod(getValue, returnValue(op.returnType, field))
      _ <- forEach (domain.typeCases) { tpe =>
        addMethod(visitImpl, super.makeImplementation(domain.baseDataType, tpe, op, domainSpecific))
      }
      _ <- addImplemented(visitorInterface)
    } yield ()
  }

  /** Create a method implementation that simply returns field as an expression, with appropriate return type for operation.
   *
   * {{{
   *   RETURNTYPE ???() { return FIELD; }
   * }}}
   *
   * @param op
   * @param field
   * @return
   */
  def returnValue(tpe: TypeRep, field:Expression): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for {
      rt <- toTargetLanguageType(tpe)
      _ <- resolveAndAddImport(rt)
      _ <- setReturnType(rt)
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
      thisRef <- selfReference()
      impl <- getMember(thisRef, visitImpl)
      args <- getArguments().map(_.map(_._3))
      result <- apply(impl, args)

      // now need to store it. AND add those statements to the method body
      storedField <- getMember(thisRef, value)
      stmt <- assignVar(storedField, result)
      _ <- addBlockDefinitions(Seq(stmt))

      // Return unit (translates to no return/void in Java, real unit return in Scala)
      result <- this.reify(InstanceRep(TypeRep.Unit)(()))
    } yield Some(result)
  }

  /**
   * The Visitor approach is defined as follows
   *
   * 1. Make the base class (for the domain)
   * 2. For each of the data types (in flattened set) create a derived class
   * 3. Create the Visitor interface
   *
   * @param gdomain
   * @param domainSpecific
   * @return
   */
  def implement(gdomain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val domain = gdomain match {
      case _:Model => gdomain.asInstanceOf[Model]
      case _ => gdomain.linearize
    }

    val flatDomain = domain.flatten
    for {
      _ <- registerTypeMapping(flatDomain)
      _ <- domainSpecific.initialize(this)
      _ <- makeBase(flatDomain.baseDataType)
      _ <- addClassToProject(makeVisitorInterface(flatDomain.typeCases), visitorClass)

      _ <- forEach (flatDomain.typeCases) { tpeCase =>
        makeDerived(flatDomain.baseDataType, tpeCase, domain)   // used to have flatDomain.ops,
      }  // parentType: DataType, tpeCase: DataTypeCase, model: Model

      _ <- forEach (flatDomain.ops) { op =>
        addClassToProject(makeOperationImplementation(flatDomain, op, domainSpecific), names.mangle(names.conceptNameOf(op)))
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
