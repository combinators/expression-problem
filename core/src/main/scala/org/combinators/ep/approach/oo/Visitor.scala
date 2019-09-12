package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm._
import Command._
import AnyParadigm.syntax._

/**
 * Using visitor approach.
 *
 * Have to decide whether to use side effects or Generics (start here)
 */
sealed trait Visitor extends ApproachImplementationProvider {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]

  val accept:String = "accept"
  val visit:String = "visit"
  val visitorClass:String = "Visitor"
  val visitorParameter:String = "v"
  val expParameter:String = "exp"
  val visitTypeParameter:String = "R"

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
    for {

      // In the "message.to" expression, invoke the 'accept' method with a visitor argument
      method <- getMember(message.to, names.mangle(accept))   // things which are code-generated use the '<-' handles unpacking results

      // the operation is encoded in its own class, which we must find to determine the visitor type
      op = message.request.op
      visitorType <- findClass(names.conceptNameOf(op))     // each visitor is named based on operation
      _ <- resolveAndAddImport(visitorType)            // gives resulting import statement (if needed)

      // construct the visitor object for the given type (and parameters)
      visitor <- instantiateObject(visitorType, op.parameters.map(param => message.request.arguments(param)))

      // apply to method with the visitor, resulting in the expression
      result <- apply(method, Seq(visitor))           // has to be Seq[] just for syntax of calling the method
    } yield result
  }

  /**
   * Sometimes code generator resolves a type (such as Scala Double to Java Double) but sometimes you have types
   * that are specific to the domain/approach (i.e., "Sub.java").
   *
   * Whenever you need to look up a DataType, the domain base type, this provides way to get around this tension.
   *
   * Must be able to get base type (i.e., Exp) of each individual data type, and this is the same for visitor
   * as for traditional OO. That is, it looks up the class which is named the same as the base type.
   *
   * This code relies on some advanced Scala capabilities, but is fairly boilerplate.
   */
  implicit def canLookupType[Ctxt](implicit canFindClass: Understands[Ctxt, FindClass[Type]]):
  Understands[Ctxt, GeneratedTypeLookupFunction[Ctxt]] = {
    new Understands[Ctxt, GeneratedTypeLookupFunction[Ctxt]] {
      def perform(context: Ctxt, command: GeneratedTypeLookupFunction[Ctxt]):
      (Ctxt, DataType => Generator[Ctxt, Type]) =
        (context, tpe => FindClass(names.conceptNameOf(tpe)).interpret(canFindClass))
    }
  }
  implicit val canLookupTypeInMethod = {
    import ooParadigm.methodBodyCapabilities._
    canLookupType[MethodBodyContext]
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
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      // access the constructor for the class associated with type case and invoke constructors with arguments.
      rt <- findClass(names.conceptNameOf(tpeCase))
      _ <- resolveAndAddImport(rt)
      ctor <- getConstructor(rt)
      res <- apply(ctor, args)
    } yield res
  }

  /**
   * Define the base class for Exp
   * {{{
   *  package visitor;
   *  public abstract class Exp {
   *    public abstract <R> R accept(Visitor<R> v);
   *  }
   * }}}
   * @param tpe
   * @return
   */
  def makeBase(tpe: DataType): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val makeClass: Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      for {
        _ <- setAbstract()
        visitorSignature <- makeAcceptSignature()
        _ <- addAbstractMethod(names.mangle(accept), visitorSignature)
      } yield ()
    }

    // adds the 'Exp' class, with a single accept method
    addClassToProject(names.conceptNameOf(tpe), makeClass)
  }

  /**
   * Not part of approach implementation provider but was needed for OO provider and was used to provide code
   * for making the actual signature of the
   * {{{
   *  public abstract <R> R accept(Visitor<R> v);
   * }}}
   * @return
   */
  def makeAcceptSignature(): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities.{toTargetLanguageType => _, _}
    import polymorphics.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      // R by itself, since not extending any other type parameter (hence Skip)
      _ <- addTypeParameter(names.mangle(visitTypeParameter), Command.skip)

      // this returns mangled visitTypeParameter name and gets list of all type parameters, for which there is only one, so we get head
      args <- getTypeArguments()
      _ <- setReturnType(args.head)

      // identify Visitor<R>
      visitorClassType <- findClass(names.mangle(visitorClass))
      visitorType  <- applyType (visitorClassType, args)

      _ <- setParameters(Seq((names.mangle(visitorParameter), visitorType)))      // a pair (name,type) of only one sequence
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
   * @param parentType
   * @param tpeCase
   * @param ops
   * @param domainSpecific
   * @return
   */
  def makeDerived(parentType: DataType, tpeCase: DataTypeCase, ops: Seq[Operation], domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      for {
        parent <- super.toTargetLanguageType(TypeRep.DataType(parentType))
        _ <- resolveAndAddImport(parent)
        _ <- addParent(parent)
        _ <- forEach (tpeCase.attributes) { att => makeField(att) }
        _ <- addConstructor(makeConstructor(tpeCase))
        _ <- forEach (tpeCase.attributes) { att => makeGetter(att) }
        _ <- makeAcceptImplementation()
      } yield ()
    }
    addClassToProject(names.conceptNameOf(tpeCase), makeClass)
  }

  /** Make field for the data type subtype.
   * {{{
   *   private Exp left;
   * }}}
   */
  def makeField(att: Attribute): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities.{toTargetLanguageType => _, _}
    for {
      ft <- toTargetLanguageType(att.tpe)
      _ <- resolveAndAddImport(ft)
      _ <- addField(names.instanceNameOf(att), ft)
    } yield ()
  }

  /** Make constructor for subtype.
   * {{{
   * public Add(Exp left, Exp right) {
   *   this.left = left;
   *   this.right = right;
   * }
   * }}}
   * @param tpeCase
   * @return
   */
  def makeConstructor(tpeCase: DataTypeCase): Generator[ConstructorContext, Unit] = {
    import ooParadigm.constructorCapabilities._

    for {
      // get all attributes and form a seuquence
      params <- forEach (tpeCase.attributes) { att: Attribute =>
        for {
          at <- super.toTargetLanguageType(att.tpe)
          _ <- resolveAndAddImport(at)
        } yield (names.instanceNameOf(att), at)
      }
      _ <- setParameters(params)

      // initialize this.XXX = XXX
      args <- getArguments()
      _ <- forEach(args) { case (name, _, exp) => initializeField(name, exp) }
    } yield ()
  }

  /** Make a single getter method for the 'att' attribute, such as:
   * {{{
   * public Exp getRight() {
   *   return this.right;
   * }
   *   }}}
   * @param att
   * @return
   */
  def makeGetter(att:Attribute): Generator[ClassContext, Unit] = {
    val makeBody: Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._

      for {
        rt <- toTargetLanguageType(att.tpe)
        _ <- resolveAndAddImport(rt)
        _ <- setReturnType(rt)

        // return // this.attribute name
        self <- selfReference()
        result <- getMember(self, names.instanceNameOf(att))
      } yield Some(result)
    }

    import ooParadigm.classCapabilities._
    addMethod("get" + names.conceptNameOf(att), makeBody)
  }

  /** Create an accept implementation from the accept method signature.
   * {{{
   *  public <R> R accept(Visitor<R> v) {
   *     return v.visit(this);
   * }
   * }}}
   * @return
   */
  def makeAcceptImplementation(): Generator[ClassContext, Unit] = {
    val makeBody: Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._

      for {
        // start from the accept signature and add a method body.
        _ <- makeAcceptSignature()
        args <- getArguments()   // get name, type, expression

        // invoke visit method on 'v' with 'this' as argyment
        visitFunction <- getMember(args.head._3, names.mangle(visit))
        self <- selfReference()
        result <- apply(visitFunction, Seq(self))  // make the method invocation
      } yield Some(result)
    }

    import ooParadigm.classCapabilities._
    addMethod(names.mangle("accept"), makeBody)
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
  def makeVisitorInterface(allTypes:Seq[DataTypeCase]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val makeClass: Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      import polymorphics.methodBodyCapabilities._
      for {
        _ <- setAbstract()
        _ <- addTypeParameter(names.mangle(visitTypeParameter), Command.skip)    // R by itself, since not extending any other type parameter (hence Skip)
        _ <- forEach (allTypes) { tpe => makeVisitSignature(tpe) }
      } yield ()
    }

    addClassToProject(visitorClass, makeClass)
  }

  /**
   * Creates the signature for the 'abstract R visit(DataType exp)' method which still has no body, and can
   * thus become an abstract interface declaration or form the basis for an implementation.
   * @return
   */
  def makeVisitSignature(tpe:DataTypeCase): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities.{toTargetLanguageType => _, _}
    import polymorphics.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      _ <- addTypeParameter(names.mangle(visit), Command.skip)    // R by itself, since not extending any other type parameter (hence Skip)
      args <- getTypeArguments()     // this returns mangled visitTypeParameter name and gets list of all type parameters
      _ <- setReturnType(args.head)  // only R
      visitorClassType <- findClass(names.conceptNameOf(tpe))
      _ <- setParameters(Seq((names.mangle(expParameter), visitorClassType)))      // a pair (name,type) of only one sequence
    } yield ()
  }



  /**
   * Prepare the signature for the method that performs a given operation.
   * {{{
   *   public Double visit(Lit e) {
   *         return e.getValue();
   *     }
   *
   *     public Double visit(Add e) {
   *         return e.getLeft().accept(new Eval()) + e.getRight().accept(new Eval());
   *     }
   * }}}
   *
   *  Note for visitor approach, the name of method is always 'visit', but for our purposes
   *  here, the method is not named. Not sure I have this right.
   *
   * @param op
   * @return
   */
  def makeSignature(op: Operation): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities.{toTargetLanguageType => _, _}

    for {
      rt <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(rt)
      _ <- setReturnType(rt)

      params <- forEach (op.parameters) { param: Parameter =>
        for {
          pt <- toTargetLanguageType(param.tpe)
          _ <- resolveAndAddImport(pt)
        } yield (names.mangle(param.name), pt)
      }
      _ <- setParameters(params)
    } yield ()
  }



  def makeImplementation(tpe: DataType,
                          tpeCase: DataTypeCase,
                          op: Operation,
                          domainSpecific: EvolutionImplementationProvider[this.type]
                        ): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      _ <- makeSignature(op)
      thisRef <- selfReference()
      attAccessors: Seq[Expression] <- forEach (tpeCase.attributes) { att =>
        GetMember(thisRef, names.instanceNameOf(att)).interpret
      }

      allArgs <- getArguments()
      args = allArgs.map { case (name, _, exp) => (name, exp) }.toMap

      // body of this implementation is the result of the individual domain-specific logic.
      result <-
        domainSpecific.logic(this)(
          ReceivedRequest(
            tpe,
            tpeCase,
            thisRef,
            tpeCase.attributes.zip(attAccessors).toMap,
            Request(op, op.parameters.map(param => (param, args(names.mangle(param.name)))).toMap)
          )
        )
    } yield Some(result)
  }

  /** Each operation is placed in its own class, with a 'visit' method for each known data type.
   * {{{
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
   * }}}
   *
   * @param allTypes
   * @param op
   * @param domainSpecific
   * @return
   */
  def makeOperationImplementation(allTypes:Seq[DataTypeCase],
                                  tpeCase: DataType,
                                  op: Operation,
                                  domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val makeClass: Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      import polymorphics.methodBodyCapabilities._
      for {
        _ <- setAbstract()
        _ <- addTypeParameter(names.mangle(visitTypeParameter), Command.skip)    // R by itself, since not extending any other type parameter (hence Skip)
        _ <- forEach (allTypes) { tpe =>
          makeImplementation(tpeCase, tpe, op, domainSpecific)
        }
      } yield ()
    }

    addClassToProject(visitorClass, makeClass)
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
    val flatDomain = domain.flatten
    for {
      _ <- makeBase(flatDomain.baseDataType)
      _ <- forEach (flatDomain.typeCases) { tpeCase =>
        makeDerived(flatDomain.baseDataType, tpeCase, flatDomain.ops, domainSpecific)
      }
      _ <- makeVisitorInterface(flatDomain.typeCases)
      _ <- forEach (flatDomain.ops) { op =>
        makeOperationImplementation(flatDomain.typeCases, flatDomain.baseDataType, op, domainSpecific)
      }
    } yield ()
  }
}

object Visitor {
  type WithParadigm[P <: AnyParadigm] = Traditional { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (nameProvider: NameProvider, base: P)
  (oo: ObjectOriented.WithBase[base.type]): Traditional.WithParadigm[base.type] =
    new Visitor {
      override val names: NameProvider = nameProvider
      override val paradigm: base.type = base
      override val ooParadigm: ObjectOriented.WithBase[paradigm.type] = oo
    }
}