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
abstract class Visitor extends ApproachImplementationProvider {
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

      // In the "message.to" expression, invoke the 'accept' method with a visitor argument
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
      rt <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(rt)
      res <- instantiateObject(rt, args)
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
        _ <- addAbstractMethod(accept, makeAcceptSignature())
      } yield ()
    }

    // adds the 'Exp' class, with a single accept method
    addClassToProject(names.mangle(names.conceptNameOf(tpe)), makeClass)
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
        parent <- toTargetLanguageType(TypeRep.DataType(parentType))
        _ <- resolveAndAddImport(parent)
        _ <- addParent(parent)
        _ <- forEach (tpeCase.attributes) { att => makeField(att) }
        _ <- addConstructor(makeConstructor(tpeCase))
        _ <- forEach (tpeCase.attributes) { att => makeGetter(att) }
        _ <- makeAcceptImplementation()
      } yield ()
    }
    addClassToProject(names.mangle(names.conceptNameOf(tpeCase)), makeClass)
  }

  /** Make field for the data type subtype.
   * {{{
   *   private Exp left;
   * }}}
   */
  def makeField(att: Attribute): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    for {
      ft <- toTargetLanguageType(att.tpe)
      _ <- resolveAndAddImport(ft)
      _ <- addField(names.mangle(names.instanceNameOf(att)), ft)
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
          at <- toTargetLanguageType(att.tpe)
          _ <- resolveAndAddImport(at)
          pName <- freshName(names.mangle(names.instanceNameOf(att)))
        } yield (pName, at)
      }
      _ <- setParameters(params)

      // initialize this.XXX = XXX
      args <- getArguments()
      _ <- forEach(tpeCase.attributes.zip(args)) { case (att, (_, _, exp)) =>
        initializeField(names.mangle(names.instanceNameOf(att)), exp)
      }
    } yield ()
  }

  /** Make a single getter method for the 'att' attribute, such as:
   * {{{
   * public Exp getRight() {
   *   return this.right;
   * }
   * }}}
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
        result <- getMember(self, names.mangle(names.instanceNameOf(att)))
      } yield Some(result)
    }

    import ooParadigm.classCapabilities._
    addMethod(names.addPrefix("get", names.mangle(names.conceptNameOf(att))), makeBody)
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
        visitFunction <- getMember(args.head._3, visit)
        self <- selfReference()
        result <- apply(visitFunction, Seq(self))  // make the method invocation
      } yield Some(result)
    }

    import ooParadigm.classCapabilities._
    addMethod(accept, makeBody)
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
      import genericsParadigm.classCapabilities._
      for {
        _ <- setInterface()
        visitTyParam <- freshName(names.mangle(visitTypeParameter))
        _ <- addTypeParameter(visitTyParam, Command.skip) // R by itself, since not extending any other type parameter (hence Skip)
        visitResultType <- getTypeArguments().map(_.head)
        _ <- forEach (allTypes) { tpe => addAbstractMethod(visit, makeVisitSignature(tpe, visitResultType)) }
      } yield ()
    }

    addClassToProject(visitorClass, makeClass)
  }

  /**
   * Creates the signature for the 'abstract R visit(DataType exp)' method which still has no body, and can
   * thus become an abstract interface declaration or form the basis for an implementation.
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




  /** Make a method body for each operation, which is a visit method for a defined data type
   *
   * {{{
   *     public Double visit(Sub e) {
   *         return e.getLeft().accept(new Eval()) - e.getRight().accept(new Eval());
   *     }
   * }}}
   *
   * @param tpe
   * @param tpeCase
   * @param op
   * @param domainSpecific
   * @return
   */
  def makeImplementation(tpe: DataType,
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

  def makeOperationConstructor(op: Operation): Generator[ConstructorContext, Unit] = {
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
      _ <- forEach (op.parameters.zip(args)) { case (param, (_, _, arg)) =>
        initializeField(names.mangle(param.name), arg)
      }
    } yield ()

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
   * @param domain     Model for which all types are to be incorporated
   * @param op
   * @param domainSpecific
   * @return        The one invoking this method must be sure to add this class to project.
   */
  def makeOperationImplementation(domain:Model,
                                  op: Operation,
                                  domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    val makeClass: Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._
      for {
        visitorInterface <- findClass(visitorClass)
        _ <- resolveAndAddImport(visitorInterface)
        returnTpe <- toTargetLanguageType(op.returnType)
        _ <- resolveAndAddImport(returnTpe)
        visitorInterfaceWithReturnType <- applyType(visitorInterface, Seq(returnTpe))
        _ <- addImplemented(visitorInterfaceWithReturnType)
        _ <- addConstructor(makeOperationConstructor(op))
        _ <- forEach (domain.typeCases) { tpe =>
          addMethod(visit, makeImplementation(domain.baseDataType, tpe, op, domainSpecific))
        }
      } yield ()
    }

    makeClass
//    addClassToProject(visitorClass, makeClass)
  }

  def domainTypeLookup[Ctxt](dtpe: DataType)(implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]]): Generator[Ctxt, Type] = {
    FindClass(names.mangle(names.conceptNameOf(dtpe))).interpret(canFindClass)
  }

  def initializeApproach(domain: Model): Generator[ProjectContext, Unit] = {
    import paradigm.projectContextCapabilities._
    import ooParadigm.projectCapabilities._
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
      _ <- forEach (flatDomain.typeCases) { tpeCase =>
        makeDerived(flatDomain.baseDataType, tpeCase, flatDomain.ops, domainSpecific)
      }
      _ <- makeVisitorInterface(flatDomain.typeCases)
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
