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
 * Synthesizing OO and Functional Design to promote Reuse
 * Shriram Krishnamurthi, Matthias Felleisen, Daniel Friedman
 * https://dl.acm.org/citation.cfm?id=679709
 *
 * TODO: Doesn't yet work for c1 merged, since it reuses code from visitor (constructors)
 * that need to be modified instead
 *
 * https://stackoverflow.com/questions/55501899/exception-in-intellijs-sbt-console-not-found-value-ideaport-ideaport-in-globa
 * problem with sbt...
 */
trait ExtensibleVisitor extends ApproachImplementationProvider {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]

  import paradigm._
  import ooParadigm._
  import syntax._

  // necessary constants used to ensure no typos
  lazy val accept: Name = names.mangle("accept")
  lazy val visit: Name = names.mangle("visit")
  lazy val visitorInstanceFactoryMethodPrefix: Name = names.mangle("make")
  lazy val visitorClass: Name = names.mangle("Visitor")
  val visitorParameter: String = "v"
  val expParameter: String = "exp"
  val visitTypeParameter: String = "R"



  /** Instantiate an implementation of the visitor. */
  def instantiateVisitor(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      method <-
        getMember(
          selfReference(),
          names.addSuffix(visitorInstanceFactoryMethodPrefix, names.conceptNameOf(message.request.op))
        )
      instance <- apply(method, Seq.empty)
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

      // construct the visitor object for the given type (and parameters)
      visitor <- instantiateVisitor(message)

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

  /** Compute the name for the visitor interface of the given model, if a new interface is required. */
  def visitorInterfaceName(model: Model): Option[Name] = {
    val sortedTypeCases = model.typeCases.sortWith(_.name < _.name)
    if (model == model.base) {
      Some(visitorClass)
    } else if (sortedTypeCases.isEmpty) {
      None
    } else {
      val name =
        sortedTypeCases.foldLeft(visitorClass) { case (name, tpeCase) =>
          names.addSuffix(name, names.conceptNameOf(tpeCase))
        }
      Some(name)
    }
  }

  /**
   * {{{
   *  public abstract <R> R accept(Visitor<R> v);
   * }}}
   */
  def makeAcceptSignature(model: Model): Generator[MethodBodyContext, Unit] = {
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
      visitorClassType <- findClass(visitorInterfaceName(model.base).get)
      _ <- resolveAndAddImport(visitorClassType)
      visitorType  <- applyType (visitorClassType, args)

      visitParam <- freshName(names.mangle(visitorParameter))
      _ <- setParameters(Seq((visitParam, visitorType)))      // a pair (name,type) of only one sequence
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
        _ <- makeAcceptImplementation(model)
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
   * //TODO: change this to perform instanceof check + cast
   */
  def makeAcceptImplementation(model: Model): Generator[ClassContext, Unit] = {
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


  /** Compute the name for the visitor implementation of the given model and operation, if an implementation
   *  is required. */
  def visitorClassName(model: Model, operation: Operation) : Option[Name] = {
    val sortedTypeCases = model.typeCases.sortWith(_.name < _.name)
    val operationName = names.mangle(names.conceptNameOf(operation))
    if (model == model.base) {
      Some(operationName)
    } else if (sortedTypeCases.isEmpty) {
      None
    } else {
      val name =
        sortedTypeCases.foldLeft(operationName) { case (name, tpeCase) =>
          names.addSuffix(name, names.conceptNameOf(tpeCase))
        }
      Some(name)
    }
  }









  // TODO: Rest of file..

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
  override def makeOperationImplementation(domain:Model,
                                  op: Operation,
                                  domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    val regularVisitor = super.makeOperationImplementation(domain, op, domainSpecific)

    val full:String = modelTypes(domain)
    val lastWithType:Option[Model] = if (domain.last.isEmpty) {
      None
    } else {
      domain.last.get.lastModelWithDataTypes
    }
    val lastOperation = if (lastWithType.isDefined) {
      lastWithType.get.findOperation(op)
    } else {
      None
    }

    // Must take care to ensure we don't mistakenly go back *before* where the operation was defined.
    // This is determined by looking for operations in the past.
    val last = if (lastWithType.isEmpty || lastOperation.isEmpty) {
      ""
    } else {
      modelTypes(lastWithType.get)
    }

    // add to regular visitor

    val makeClass: Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._
      for {
        rt <- toTargetLanguageType(op.returnType)  // TODO: How to convert return type into Double.

        // identify Visitor<R>
        visitorClassType <- findClass(visitorClass)
        _ <- resolveAndAddImport(visitorClassType)
        visitorType  <- applyType (visitorClassType, Seq(rt))

        _ <- addImplemented(visitorType)
        _ <- forEach (domain.typeCases) { tpe =>
          addMethod(names.mangle(names.instanceNameOf(tpe)), makeImplementation(domain.baseDataType, tpe, op, domainSpecific))
        }
      } yield ()
    }

    makeClass
    //    // if I want to override a super, this is a mistake since this will be added to project.
    //    addClassToProject(visitorClass, makeClass)
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
  def makeOperationsBase(tpe: DataType): Generator[ProjectContext, Unit] = {
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
   * This should create the following (when called three times):
   *
   * ```
   * public class Eval implements Visitor<Double> {
   *
   * public Double visit(Lit e) {
   * return e.getValue();
   * }
   *
   * public Double visit(Add e) {
   * return e.getLeft().accept(makeEval()) + e.getRight().accept(makeEval());
   * }
   *
   * Eval makeEval() {
   * return new Eval();
   * }
   * }```
   *
   * and then
   *
   * ```
   * public class EvalSub extends Eval implements VisitorSub<Double> {
   *
   * public Double visit(Sub e) {
   * return e.getLeft().accept(makeEval()) - e.getRight().accept(makeEval());
   * }
   *
   * EvalSub makeEval() {
   * return new EvalSub();
   * }
   * }
   * ```
   *
   * ```
   * package visitor;
   *
   * public class EvalDivdMultNeg extends EvalSub implements VisitorDivdMultNeg<Double> {
   *
   * public Double visit(Neg e) {
   * return -e.getInner().accept(makeEval());
   * }
   *
   * public Double visit(Mult e) {
   * return e.getLeft().accept(makeEval()) * e.getRight().accept(makeEval());
   * }
   *
   * public Double visit(Divd e) {
   * return e.getLeft().accept(makeEval()) / e.getRight().accept(makeEval());
   * }
   *
   * EvalDivdMultNeg makeEval() {
   * return new EvalDivdMultNeg();
   * }
   * }
   * ```
   *
   * I'm having trouble creating the method signatures for any of these....
   * @return
   */
  def makeFactory(model: Model): Generator[ProjectContext, Unit] = {
    val fullType: String = modelInterfaceName(model)
    val combinedOps: String = model.ops.sortWith(_.name < _.name).map(op => names.conceptNameOf(op)).mkString("")

    //    def typeConverterRelativeToHere(rep: TypeRep): Type = {
    //      import ooParadigm.classCapabilities._
    //      if (rep == model.baseDataType) { fullType }
    //      else findClass("Double") // TODO:       else findClass(rep)
    //    }

    import ooParadigm.projectCapabilities._
    import ooParadigm.classCapabilities._



    def makeClass(model:Model): Generator[ClassContext, Unit] = {
      val fullType: String = modelInterfaceName(model)
      import ooParadigm.classCapabilities._

      val makeClass: Generator[ClassContext, Unit] = {
        for {
          rt <- findClass(names.mangle(fullType))
//          _ <- forEach(model.typeCases) { tpe =>
//            addMethod(names.mangle(names.instanceNameOf(tpe)), factoryMethod(tpe, rt))
//          }
        } yield ()
      }

      makeClass
    }

    // adds the 'Exp' class, with a single accept method
    addClassToProject(names.mangle(s"${fullType}Factory"), makeClass(model))
  }

  /**
   * The Extensible Visitor approach is defined as follows
   *
   * 1. Make the base class (for the domain)
   * 2. For each of the data types (in flattened set) create a derived class
   * 3. Create the Visitor interface
   *
   * @param domain
   * @param domainSpecific
   * @return
   */
  override def implement(domain: Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val flatDomain = domain.flatten
    for {
      _ <- initializeApproach(flatDomain)
      _ <- domainSpecific.initialize(this)
      _ <- makeBase(flatDomain.baseDataType)                    // top-level Exp

      _ <- forEach (domain.inChronologicalOrder.filter(m => m.ops.nonEmpty)) { m =>
        makeOperationsBase(flatDomain.baseDataType)
      }
      _ <- forEach (domain.inChronologicalOrder.filter(m => m.ops.nonEmpty)) { m =>
        makeFactory(domain)
      }
      _ <- forEach (flatDomain.typeCases) { tpeCase =>
        makeDerived(flatDomain.baseDataType, tpeCase, flatDomain.ops, domainSpecific)
      }
      _ <- makeVisitorInterface(flatDomain.typeCases)
      _ <- forEach (flatDomain.ops) { op =>
        addClassToProject(names.mangle(names.conceptNameOf(op)), makeOperationImplementation(flatDomain, op, domainSpecific))
      }

      // cannot have extension for the FIRST model entry so that must be skipped.
      //_ <- makeOperatorExtension(op, m)
      models = domain.inChronologicalOrder
        .filter(m => m.typeCases.nonEmpty)
        .filter(m => m.last.nonEmpty)

      _ <- forEach (models) { m =>
        forEach (m.last.get.pastOperations) { op =>
          // THIS won't be right because name is based on Visitor$full<$opType>. How can we create a class
          // and name it? Hate to have to have separate method to redo the work
          addClassToProject(names.mangle(names.conceptNameOf(op)), makeOperationImplementation(m, op, domainSpecific))
        }
      }

//          addClassToProject(names.mangle(names.conceptNameOf(op)), makeOperationImplementation(flatDomain, op, domainSpecific))

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
