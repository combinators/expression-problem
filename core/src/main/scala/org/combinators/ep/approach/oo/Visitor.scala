package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm._
import Command._
import AnyParadigm.syntax._
import org.combinators.ep.approach

/**
 * Using visitor approach.
 *
 * Have to decide whether to use side effects or Generics. This current implementation uses the Visitor<R> generics
 * approach, which can be adopted by different object oriented languages.
 */
abstract class Visitor extends OOApproachImplementationProvider with SharedVisitor {

  import paradigm._
  import ooParadigm._
  import syntax._

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
   * @param message
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

  /** Create an accept implementation from the accept method signature.
   * {{{
   *  public <R> R accept(Visitor<R> v) {
   *     return v.visit(this);
   * }
   * }}}
   * @return
   */
  def makeAcceptImplementation(model:Model): Generator[ClassContext, Unit] = {
    val makeBody: Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._

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
   * @param domain     Model for which all types are to be incorporated
   * @param op
   * @param domainSpecific
   * @return        The one invoking this method must be sure to add this class to project.
   */
  def makeOperationImplementation(domain:Model,
                                  op: Operation,
                                  domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._
    for {
      _ <- operationClass(visit, op, domain.typeCases, domain.baseDataType, domainSpecific)

      visitorInterface <- findClass(visitorClass)
      _ <- resolveAndAddImport(visitorInterface)
      returnTpe <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(returnTpe)
      visitorInterfaceWithReturnType <- applyType(visitorInterface, Seq(returnTpe))
      _ <- addImplemented(visitorInterfaceWithReturnType)
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
    import paradigm.projectContextCapabilities._
    val flatDomain = domain.flatten
    for {
      _ <- debug ("Processing Visitor")
      _ <- registerTypeMapping(flatDomain)
      _ <- domainSpecific.initialize(this)
      _ <- makeBase(flatDomain.baseDataType)
      _ <- forEach (flatDomain.typeCases) { tpeCase =>
        makeDerived(flatDomain.baseDataType, tpeCase, domain)   // used to have flatDomain.ops,
      }

      _ <- addClassToProject(visitorClass, makeVisitorInterface(flatDomain.typeCases))

      _ <- forEach (flatDomain.ops) { op =>
        addClassToProject(names.mangle(names.conceptNameOf(op)), makeOperationImplementation(flatDomain, op, domainSpecific))
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
      val polymorphics: parametricPolymorphism.type = parametricPolymorphism
      val genericsParadigm: generics.type = generics
    }
}
