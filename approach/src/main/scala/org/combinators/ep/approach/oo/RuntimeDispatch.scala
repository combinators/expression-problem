package org.combinators.ep.approach.oo

/*DI:LI:AD*/

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.AnyParadigm.syntax.*
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi.{Exceptions, Strings}
import org.combinators.cogen.paradigm.{AnyParadigm, ObjectOriented}
import org.combinators.cogen.{AbstractSyntax, Command, InstanceRep, NameProvider, TypeRep}
import org.combinators.ep.domain.GenericModel
import org.combinators.ep.domain.abstractions.*
import org.combinators.ep.domain.extensions.*
import org.combinators.ep.generator.*
import org.combinators.ep.generator.communication.*


/**
  * Runtime Dispatch
  *
  * Have to decide whether to use side effects or Generics. This current implementation uses the Visitor<R> generics
  * approach, which can be adopted by different object oriented languages.
  */
trait RuntimeDispatch extends SharedOO with OperationAsClass {
  val paradigm: AnyParadigm
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val names: NameProvider[paradigm.syntax.Name]

  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val exceptions: Exceptions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val strings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]

  import ooParadigm.*
  import paradigm.*
  import syntax.*

  val expParameter: String = "exp"

  /**
    * Instantiates an instance of the domain object.
    *
    * Same implementation for OO as for visitor.
    *
    * new Add(new Lit(new Double(1.0)), new Lit(new Double(2.0)))
    */
  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities.*
    import paradigm.methodBodyCapabilities.*
    for {
      // access the constructor for the class associated with type case and invoke constructors with arguments.
      rt <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(rt)
      res <- instantiateObject(rt, args)
    } yield res
  }

  /**
    * Dispatch creates a new dispatch object and invokes operation with attributes.
    *
    * new Eval().eval(e.getLeft()) + new Eval().eval(e.getRight())
    *
    * @param message the SendRequest
    * @return
    */
  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities.*
    import paradigm.methodBodyCapabilities.*
    val op = message.request.op
    for {

      // the operation is encoded in its own class, which we must find to determine the visitor type
      opType <- findClass(names.mangle(names.conceptNameOf(op))) // Each class is based on Operation
      _ <- resolveAndAddImport(opType) // gives resulting import statement (if needed)

      // construct the Operation object for the given type (and parameters)
      dispatcher <- instantiateObject(opType, op.parameters.map(param => message.request.arguments(param)))

      // In the 'message.to' expression, invoke the 'accept' method with a visitor argument
      method <- getMember(dispatcher, names.mangle(names.instanceNameOf(op))) // things which are code-generated use the '<-' handles unpacking results
      rt <- toTargetLanguageType(message.request.op.returnType)
      _ <- resolveAndAddImport(rt)

      // apply to method with the visitor, resulting in the expression
      result <- apply(method, Seq(message.to)) // has to be Seq[] just for syntax of calling the method
    } yield result
  }

  /**
    *
    * {{{
    *  public op.returnType ???(rt e);
    * }}}
    *
    * @return
    */
  def makeOperationSignature(paramType: Type, op: Operation): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities.*
    for {

      // this returns mangled visitTypeParameter name and gets list of all type parameters, for which there is only one, so we get head
      rt <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(rt)
      _ <- setReturnType(rt)

      expParam <- freshName(names.mangle(expParameter))
      _ <- setParameters(Seq((expParam, paramType))) // a pair (name,type) of only one sequence
    } yield ()
  }

  /**
    * Create an operation that dispatches accordingly to all known data types to helper method.
    * {{{
    *  public RT op(Exp exp) {
    *     if (exp instanceof Lit) {
    *return _eval((Lit) exp);
    *}
    *
    *if (exp instanceof Add) {
    *return _eval((Add) exp);
    *}
    *
    * }
    * }}}
    *
    * @return
    */
  def makeDispatchingOperation(model: GenericModel, op: Operation): Generator[ClassContext, Unit] = {
    def ifStmt(): Generator[MethodBodyContext, Option[Expression]] = {
      import exceptions.exceptionCapabilities.*
      import impParadigm.imperativeCapabilities.*
      import ooParadigm.methodBodyCapabilities.*
      import paradigm.methodBodyCapabilities.*

      for {
        _ <- forEach(model.flatten.typeCases) { tpe =>
          for {
            tpeCase <- findClass(names.mangle(names.conceptNameOf(tpe)))
            _ <- resolveAndAddImport(tpeCase)
            args <- getArguments()
            condExpr <- instanceOfType(tpeCase, args.head._3)
            self <- selfReference()
            innerFunction <- getMember(self, names.addPrefix("_", names.mangle(names.instanceNameOf(op))))

            casted <- castObject(tpeCase, args.head._3)
            result <- apply(innerFunction, Seq(casted))
            blockReturn <- returnStmt(result)
            ifs <- ifThenElse(condExpr, addBlockDefinitions(Seq(blockReturn)), Seq.empty)
            _ <- addBlockDefinitions(Seq(ifs))
          } yield None
        }

        expr <- this.reify(InstanceRep(TypeRep.String)("Missing Subclass Case!"))
        throws <- raise(expr)
        _ <- addBlockDefinitions(Seq(throws))
      } yield None
    }

    val makeBody: Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities.*

      for {
        rt <- toTargetLanguageType(DomainTpeRep.DataType(model.baseDataType))
        _ <- resolveAndAddImport(rt)

        _ <- makeOperationSignature(rt, op)
        _ <- ifStmt()
      } yield None
    }

    import ooParadigm.classCapabilities.*
    addMethod(names.mangle(names.instanceNameOf(op)), makeBody)
  }

  /**
    * Each operation is placed in its own class, with a 'visit' method for each known data type.
    *
    * Uses the generic 'operationClass' capability to create the structure of the class.
    *
    * {{{
    * class Eval  { }
    *
    *   public Double op(Exp e) {
    *
    *     }
    *
    *     public Double _op(Lit e) {
    *         return e.getValue();
    *     }
    *
    *     public Double _op(Add e) {
    *         return op(e.left) + op(e.right)
    *     }
    *   }
    * }}}
    *
    * @param domain         Model for which all types are to be incorporated
    * @param op             The operation whose implementation is needed
    * @param domainSpecific The EIP to gain access to the logic
    * @return The one invoking this method must be sure to add this class to project.
    */
  def makeOperationImplementation(
    domain: GenericModel, op: Operation,
    domainSpecific: EvolutionImplementationProvider[this.type]
  ): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities.*

    for {
      // this will ultimately invoke makeTypeCaseImplementation for all necessary typecases
      _ <- operationClass(names.addPrefix("_", names.mangle(names.instanceNameOf(op))), op, domain, domain.flatten.typeCases.distinct, domain.baseDataType, domainSpecific)

      returnTpe <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(returnTpe)

      _ <- makeDispatchingOperation(domain, op)
    } yield ()
  }

  /** Just the skeleton class holding structure, since operations are designated to their own classes. */
  def makeDerived(parentType: DataType, tpeCase: DataTypeCase): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities.*
    val makeClass: Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities.*
      for {
        parent <- toTargetLanguageType(DomainTpeRep.DataType(parentType))
        _ <- resolveAndAddImport(parent)
        _ <- addParent(parent)
        _ <- forEach(tpeCase.attributes) { att => makeField(att) }
        _ <- addConstructor(makeConstructor(tpeCase))
        _ <- forEach(tpeCase.attributes) { att => makeGetter(att) }

      } yield ()
    }
    addClassToProject(makeClass, names.mangle(names.conceptNameOf(tpeCase)))
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
    * @param tpe The DataType that needs a Base Class.
    * @return
    */
  def makeBase(tpe: DataType): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities.*

    val makeClass: Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities.*
      for {
        _ <- setAbstract()
      } yield ()
    }

    // adds the 'Exp' class, with a single accept method
    addClassToProject(makeClass, names.mangle(names.conceptNameOf(tpe)))
  }

  def targetExp: Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities.*
    import paradigm.methodBodyCapabilities.*

    for {
      expRef <- getArguments().map(_.head._3)
    } yield expRef
  }

  /** Make a method body for each operation, which dispatches to appropriate method
    *
    * {{{
    *     public class Eval {
    *public Eval() { }
    *public Double _eval(Add exp) {
    *return (new ep.Eval().eval(exp.getLeft()) + new ep.Eval().eval(exp.getRight()));
    *}
    *}
    * }}}
    *
    * A runtime dispatcher will direct to appropriate _op() method from the op() method in class
    *
    * @param tpe            Base Data Type.
    * @param tpeCase        The Data Type Case.
    * @param op             The operation at play.
    * @param domainSpecific The EIP which contains the logic.
    * @return Full implementation.
    */
  override def makeTypeCaseImplementation(
    tpe: DataType, tpeCase: DataTypeCase, op: Operation, model: GenericModel,
    domainSpecific: EvolutionImplementationProvider[this.type]
  ): Generator[MethodBodyContext, Option[Expression]] = {
    import ooParadigm.methodBodyCapabilities.*
    import paradigm.methodBodyCapabilities.*
    for {
      returnType <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(returnType)

      ptype <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(ptype)
      _ <- makeOperationSignature(ptype, op)

      result <- completeImplementationFromParameters(tpe, tpeCase, op, model, domainSpecific, target = targetExp, attributeAccess = attributeDispatchAccess)
    } yield result
  }

  /**
    * Access attributes using default getter methods via argument to method, exp
    *
    * @param attribute Data Type Case attribute to be accessed
    * @return
    */
  def attributeDispatchAccess(attribute: Attribute, tpeCase: DataTypeCase, domain: GenericModel, baseType: Option[paradigm.syntax.Type]): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities.*
    import paradigm.methodBodyCapabilities.*

    for {
      expRef <- getArguments().map(_.head._3)
      getterMethod <- getMember(expRef, getterName(attribute))
      getterCall <- apply(getterMethod, Seq.empty)
    } yield getterCall
  }

  /**
    * The Runtime Dispatch approach is defined as follows
    *
    * 1. Make the base class (for the domain)
    * 2. For each of the operations (in flattened set) create an operation class that has
    * dynamic runtime checks to dispatch to appropriate method
    * 3. For each data type (in flatted set) create data type class to hold information
    *
    * @param gdomain        Top-level domain.
    * @param domainSpecific Its corresponding EIP.
    * @return The whole project.
    */
  def implement(gdomain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities.*
    import paradigm.projectCapabilities.*

    val flat = gdomain.linearize.flatten
    for {
      _ <- debug("Processing RuntimeDispatching")
      _ <- strings.enable()
      _ <- exceptions.enable()
      _ <- registerTypeMapping(flat)
      _ <- domainSpecific.initialize(this)

      // singular Exp abstract class
      _ <- makeBase(flat.baseDataType)

      _ <- forEach(flat.typeCases) { tpeCase =>
        makeDerived(flat.baseDataType, tpeCase)
      }
      _ <- forEach(flat.ops) { op =>
        addClassToProject(makeOperationImplementation(gdomain, op, domainSpecific), names.mangle(names.conceptNameOf(op)))
      }
    } yield ()
  }
}

object RuntimeDispatch {
  type WithParadigm[P <: AnyParadigm] = RuntimeDispatch {val paradigm: P}
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
    (base: P)
      (
        nameProvider: NameProvider[base.syntax.Name],
        impParadigmProvider: Imperative.WithBase[base.MethodBodyContext, base.type],
        stringsProvider: Strings.WithBase[base.MethodBodyContext, base.type],
        exceptionsProvider: Exceptions.WithBase[base.MethodBodyContext, base.type],
        oo: ObjectOriented.WithBase[base.type]
      ): RuntimeDispatch.WithParadigm[base.type] = {
    case class RD(
      override val paradigm: base.type
    )(
      override val names: NameProvider[paradigm.syntax.Name],
      override val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type],
      override val strings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type],
      override val exceptions: Exceptions.WithBase[paradigm.MethodBodyContext, paradigm.type],
      override val ooParadigm: oo.type
    ) extends RuntimeDispatch
    RD(base)(nameProvider, impParadigmProvider, stringsProvider, exceptionsProvider, oo)
  }
}
