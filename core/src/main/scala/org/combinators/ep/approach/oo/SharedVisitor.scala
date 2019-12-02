package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, Operation, TypeRep}
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.communication.{ReceivedRequest, Request}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.{Generics, ObjectOriented, ParametricPolymorphism}

trait SharedVisitor extends OOApproachImplementationProvider with SharedOO with FieldDefinition with OperationAsClass {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]

  import paradigm._
  import ooParadigm._
  import syntax._

  // necessary constants used to ensure no typos
  lazy val accept: Name = names.mangle("accept")
  lazy val visit: Name = names.mangle("visit")
  lazy val visitorClass: Name = names.mangle("Visitor")
  val visitorParameter: String = "v"
  val expParameter: String = "exp"
  val visitTypeParameter: String = "R"

  // must be provided by the appropriate extension
  def makeAcceptImplementation(model: Model): Generator[ClassContext, Unit]

  // will need a visit method
  def makeOperationImplementation(domain:Model, op: Operation, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit]

  /**
   * At heart a visitor class has specific structure:
   *
   * {{{
   *   TBA TBA TBA
   * }}}
   * @param model
   * @param name
   * @param op
   * @param domainSpecific
   * @return
   */
  def visitorClass(model: Model, name:Name, op:Operation, domainSpecific: EvolutionImplementationProvider[this.type], useGenerics:Boolean): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._
    for {
      visitorInterface <- findClass(name)
      _ <- resolveAndAddImport(visitorInterface)

      returnTpe <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(returnTpe)

      implType <- if (useGenerics) {
        for {
          visitorInterfaceWithReturnType <- applyType(visitorInterface, Seq(returnTpe))
        } yield visitorInterfaceWithReturnType
      } else {
        for {
          vi <- findClass(name)   // unnecessary but can't seem to avoid it!
        } yield vi
      }

      visitorInterfaceWithReturnType <- applyType(visitorInterface, Seq(returnTpe))
      _ <- addImplemented(visitorInterfaceWithReturnType)

      _ <- addConstructor(makeOperationConstructor(op))

      _ <- forEach (model.typeCases) { tpe =>
        addMethod(visit, makeImplementation(model.baseDataType, tpe, op, domainSpecific))
      }
    } yield ()
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
      rt <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(rt)
      res <- instantiateObject(rt, args)
    } yield res
  }

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
    addClassToProject(names.mangle(names.conceptNameOf(tpe)), makeClass)
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
    for {
      returnType <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(returnType)
      _ <- makeVisitSignature(tpeCase, returnType)
      visitedRef <- getArguments().map(_.head._3)
      attAccessors: Seq[Expression] <- forEach (tpeCase.attributes) { att =>
        for {
          getter <- getMember(visitedRef, names.addPrefix("get", names.mangle(names.conceptNameOf(att))))
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
   * For data types that were added after an operation, this runtime check is demanded to protect the
   * downcast. Eventually the instanceof check and the throw exception will be added.
   *
   * public <R> R accept(Visitor<R> v) {
   *   if (v instanceof VisitorDivdMultNeg) {
   *     return ((VisitorDivdMultNeg<R>) v).visit(this);
   *   }
   *   throw new RuntimeException("Older visitor used with newer datatype variant.");
   * }
   */
  def makeDerived(parentType: DataType, tpeCase: DataTypeCase, model: Model): Generator[ProjectContext, Unit] = {
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
    addClassToProject(names.mangle(names.conceptNameOf(tpeCase)), makeClass)
  }
}
