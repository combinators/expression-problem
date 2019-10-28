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
trait ExtensibleVisitor extends ApproachImplementationProvider with SharedVisitor {

  import paradigm._
  import ooParadigm._
  import syntax._

  lazy val visitorInstanceFactoryMethodPrefix: Name = names.mangle("make")

  /**
   * Instantiate an implementation of the visitor.
   *
   * Required capability of having makeXXX() within the extensible visitors.
   *
   * ```
   * Eval makeEval() {
   * return new Eval();
   * }
   * ```
   *
   * If/When the operation has parameters, then they would have to be defined and passed through from makeXXX() into the new XXXX()
   */
  def instantiateVisitor(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      self <- selfReference()
      method <-
        getMember(
          self, names.addSuffix(visitorInstanceFactoryMethodPrefix, names.conceptNameOf(message.request.op))
        )
      instance <- apply(method, Seq.empty)  // no arguments
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
   * ```
   * public Double mult(Mult exp) {
   *   return exp.getLeft().accept(new Eval()) * exp.getRight().accept(new Eval());
   * }
   *
   * public Double visit(Mult e) {
   *   return e.getLeft().accept(makeEval()) * e.getRight().accept(makeEval());
   * }
   * ```
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
      genericMethod <- getMember(message.to, accept)   // things which are code-generated use the '<-' handles unpacking results
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
   * Compute the name for the visitor interface of the given model, if a new interface is required.
   *
   * Base remains as {visitorClass}
   * sub-classes are {visitorClass}DataTypes...
   */
  def visitorInterfaceName(model: Model): Option[Name] = {
    val sortedTypeCases = model.typeCases.sortWith(_.name < _.name)
    if (model == model.base || model.last == Some(model.base)) {
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

  /** Concatenate all types in this model to form proper suffix for operation classes. */
  def modelTypes(model:Model) : String = {
    if (model.last.isEmpty) {
      ""
  } else {
      model.typeCases.sortWith(_.name < _.name).mkString("")
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
   * //TODO: change this to perform instanceof check + throw exceptions
   */
  def makeAcceptImplementation(model: Model): Generator[ClassContext, Unit] = {
    val makeBody: Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import polymorphics.methodBodyCapabilities._

      for {
        // start from the accept signature and add a method body.
        _ <- makeAcceptSignature()   // used to have model as parameter.
        args <- getArguments()   // get name, type, expression
        v = args.head._3
        vType = visitorInterfaceName(model).get              // convert Name to a class

        visitorClassType <- findClass(vType)
        tpeParam <- getTypeArguments()
        instVisitClassType <- applyType(visitorClassType, tpeParam)
        castV <- castObject(instVisitClassType, v)
        // invoke visit method on 'v' with 'this' as argyment
        visitFunction <- getMember(castV, visit)

        self <- selfReference()
        result <- apply(visitFunction, Seq(self))  // make the method invocation
      } yield Some(result)
    }

    import ooParadigm.classCapabilities._
    addMethod(accept, makeBody)
  }

  /**
   * Produces, for example, Some(EvalDivdMultNeg).
   *
   * Either (1) the operation is defined in the current model and so you don't need to append class name, but can
   * simply reply on past datatypes; or (2) concatenante and find recent one
   *
   * Compute the name for the visitor implementation of the given model and operation, if an implementation
   *  is required.
   */
  def visitorClassName(model: Model, operation: Operation) : Option[Name] = {
    val operationName = names.mangle(names.conceptNameOf(operation))

    if (model.ops.contains(operation)) {
      Some(operationName)
    } else {
      // must create a new name which concatenates
      val sortedTypeCases = model.typeCases.sortWith(_.name < _.name)

      // if no types then no additional class is necessary so we can return None; otherwise
      // there are new type cases so we need to concatenate the names and so we generate a new
      // visitor type case.
      if (sortedTypeCases.isEmpty) {
        None
      } else {
        val name =
          sortedTypeCases.foldLeft(operationName) { case (name, tpeCase) =>
            names.addSuffix(name, names.conceptNameOf(tpeCase))
          }
        Some(name)
      }
    }
  }

  /**
   *   take existing visitor generated by SHared and add (where needed) a "extends VisitorSub<R>"
   *
   * @param domain
   * @return
   */
  def makeExtensibleVisitorInterface(domain:Model): Generator[ClassContext, Unit] = {
    // ignore degenerate case where the first model only has an operation without any types
    def addParentInterface(): Generator[ClassContext, Unit] = if (domain.last == Some(domain.base)) {
      Command.lift(()) // this is EMPTY
    } else {
      import ooParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._

      val interfaceName = visitorInterfaceName(domain.last.get.lastModelWithDataTypes.get)

      for {
        // find former Op, such as "Eval" or "EvalSub"
        visitorInterfaceType <- findClass(interfaceName.get)
        _ <- resolveAndAddImport(visitorInterfaceType)
        visitTyParam <- getTypeArguments()   // can do this because we need this interfaces paramType
        modifiedType <- applyType(visitorInterfaceType, visitTyParam) // R by itself, since not extending any other type parameter (hence Skip)
        // applyType generates a fresh Type and that is the one that is returned
        _ <- addParent(modifiedType)
      } yield ()
    }

    for {
      _ <- makeVisitorInterface(domain.typeCases)
      _ <- addParentInterface()
    } yield ()
  }
//
//
//  // TODO: Rest of file..
//
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
  def makeOperationImplementation(domain:Model,
                                  op: Operation,
                                  domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {

    val factoryName = names.mangle("make" + names.conceptNameOf(op))

    // ignore degenerate case where the first model only has an operation without any types
    def addParentClass(): Generator[ClassContext, Unit] = if (domain.last == Some(domain.base)) {
        Command.lift(())  // this is EMPTY
      } else {
        import ooParadigm.classCapabilities._
        val className = visitorClassName(domain.last.get.lastModelWithDataTypes.get, op)

        for {
          // find former Op, such as "Eval" or "EvalSub"
          visitorClassType <- findClass(className.get)
          _ <- resolveAndAddImport(visitorClassType)
          _ <- addParent(visitorClassType)
        } yield ()
      }

     val makeClass: Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._

      for {
        rt <- toTargetLanguageType(op.returnType)  // TODO: How to convert return type into Double.

        // identify Visitor<R>
        visitorClassType <- findClass(visitorClass)
        _ <- resolveAndAddImport(visitorClassType)
        visitorType  <- applyType (visitorClassType, Seq(rt))

        // public class EvalSub extends Eval implements VisitorSub<Double> {
        _ <- addParentClass()

        _ <- addImplemented(visitorType)
        _ <- forEach (domain.typeCases) { tpe =>
          addMethod(visit, makeImplementation(domain.baseDataType, tpe, op, domainSpecific))
        }

        _ <- addMethod(factoryName, makeFactoryMethod(domain, op))
      } yield ()
    }

    makeClass
    //    // if I want to override a super, this is a mistake since this will be added to project.
    //    addClassToProject(visitorClass, makeClass)
  }


  /**
   * {{{
   * EvalDivdMultNeg makeEval() {
   *     return new EvalDivdMultNeg();
   * }
   * }}}
   *
   * TODO: What To Do With Parameters...
   * @return
   */
  def makeFactoryMethod(model:Model, op: Operation): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      opClass <- findClass(visitorClassName(model, op).get)    // should check!
      _ <- resolveAndAddImport(opClass)
      _ <- setReturnType(opClass)

      ctor <- getConstructor(opClass)
    } yield Some(ctor)
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
  def makeOperationsBase(model:Model): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val makeClass: Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      for {
        _ <- setAbstract()
        _ <- addAbstractMethod(accept, makeAcceptSignature())
      } yield ()
    }


    println("In MakeOperationBase:" + model.name + "," + names.mangle(names.conceptNameOf(model.baseDataType)))

    // adds the 'Exp' class, with a single accept method
    addClassToProject(names.mangle(names.conceptNameOf(model.baseDataType)), makeClass)
  }

//  /**
//   * This should create the following (when called three times):
//   *
//   * ```
//   * public class Eval implements Visitor<Double> {
//   *
//   * public Double visit(Lit e) {
//   * return e.getValue();
//   * }
//   *
//   * public Double visit(Add e) {
//   * return e.getLeft().accept(makeEval()) + e.getRight().accept(makeEval());
//   * }
//   *
//   * Eval makeEval() {
//   * return new Eval();
//   * }
//   * }```
//   *
//   * and then
//   *
//   * ```
//   * public class EvalSub extends Eval implements VisitorSub<Double> {
//   *
//   * public Double visit(Sub e) {
//   * return e.getLeft().accept(makeEval()) - e.getRight().accept(makeEval());
//   * }
//   *
//   * EvalSub makeEval() {
//   * return new EvalSub();
//   * }
//   * }
//   * ```
//   *
//   * ```
//   * package visitor;
//   *
//   * public class EvalDivdMultNeg extends EvalSub implements VisitorDivdMultNeg<Double> {
//   *
//   * public Double visit(Neg e) {
//   * return -e.getInner().accept(makeEval());
//   * }
//   *
//   * public Double visit(Mult e) {
//   * return e.getLeft().accept(makeEval()) * e.getRight().accept(makeEval());
//   * }
//   *
//   * public Double visit(Divd e) {
//   * return e.getLeft().accept(makeEval()) / e.getRight().accept(makeEval());
//   * }
//   *
//   * EvalDivdMultNeg makeEval() {
//   * return new EvalDivdMultNeg();
//   * }
//   * }
//   * ```
//   *
//   * I'm having trouble creating the method signatures for any of these....
//   * @return
//   */
//  def makeFactory(model: Model): Generator[ProjectContext, Unit] = {
//    val fullType: String = modelInterfaceName(model)
//    val combinedOps: String = model.ops.sortWith(_.name < _.name).map(op => names.conceptNameOf(op)).mkString("")
//
//    //    def typeConverterRelativeToHere(rep: TypeRep): Type = {
//    //      import ooParadigm.classCapabilities._
//    //      if (rep == model.baseDataType) { fullType }
//    //      else findClass("Double") // TODO:       else findClass(rep)
//    //    }
//
//    import ooParadigm.projectCapabilities._
//    import ooParadigm.classCapabilities._
//
//
//
//    def makeClass(model:Model): Generator[ClassContext, Unit] = {
//      val fullType: String = modelInterfaceName(model)
//      import ooParadigm.classCapabilities._
//
//      val makeClass: Generator[ClassContext, Unit] = {
//        for {
//          rt <- findClass(names.mangle(fullType))
////          _ <- forEach(model.typeCases) { tpe =>
////            addMethod(names.mangle(names.instanceNameOf(tpe)), factoryMethod(tpe, rt))
////          }
//        } yield ()
//      }
//
//      makeClass
//    }
//
//    // adds the 'Exp' class, with a single accept method
//    addClassToProject(names.mangle(s"${fullType}Factory"), makeClass(model))
//  }
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
   * The Extensible Visitor approach is defined as follows. This handles the code generation for the implementation
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
      _ <- makeBase(domain.baseDataType)                    // top-level Exp
      _ <- makeOperationsBase(domain)

      // WHEN new data types are added and there are existing operations in the past
      // need to run generators in sequence and when that happens you need to group with a for {...} yield()
      _ <- forEach (domain.inChronologicalOrder.filter(m => m.typeCases.nonEmpty)) { m =>
      for {
          // add the interfaces first
          _ <- addClassToProject(visitorInterfaceName(m).get, makeExtensibleVisitorInterface(m))

          // now come the classes
          _ <- forEach(m.pastOperations) { op =>
            // must ensure
            addClassToProject(visitorClassName(m, op).get, makeOperationImplementation(m, op, domainSpecific))
          }
        } yield ()
      }

      // WHEN new operations are added and there are existing data types in the past...
      _ <- forEach (domain.inChronologicalOrder.filter(m => m.ops.nonEmpty)) { m =>
         forEach(m.ops) { op =>
          // must ensure
          addClassToProject(visitorClassName(m, op).get, makeOperationImplementation(m, op, domainSpecific))
        }
      }

      //      _ <- forEach (domain.inChronologicalOrder.filter(m => m.ops.nonEmpty)) { m =>
//        makeFactory(domain)
//      }
      _ <- forEach (domain.inChronologicalOrder) { dm =>
            forEach (dm.typeCases) { tpeCase =>
              makeDerived(domain.baseDataType, tpeCase, dm)
            }
        }

//      _ <- makeVisitorInterface(flatDomain.typeCases)
//      _ <- forEach (flatDomain.ops) { op =>
//        addClassToProject(names.mangle(names.conceptNameOf(op)), makeOperationImplementation(flatDomain, op, domainSpecific))
//      }

      // cannot have extension for the FIRST model entry so that must be skipped.
      //_ <- makeOperatorExtension(op, m)
//      models = domain.inChronologicalOrder
//        .filter(m => m.typeCases.nonEmpty)
//        .filter(m => m.last.nonEmpty)
//
//      _ <- forEach (models) { m =>
//        forEach (m.last.get.pastOperations) { op =>
//          // THIS won't be right because name is based on Visitor$full<$opType>. How can we create a class
//          // and name it? Hate to have to have separate method to redo the work
//          addClassToProject(names.mangle(names.conceptNameOf(op)), makeOperationImplementation(m, op, domainSpecific))
//        }
//      }

//          addClassToProject(names.mangle(names.conceptNameOf(op)), makeOperationImplementation(flatDomain, op, domainSpecific))

    } yield ()
  }

  /** Adds tests to the project context */
  override def implement(tests: Map[Model, Seq[TestCase]], testImplementationProvider: TestImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    import projectContextCapabilities._
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

//          val ops = model.pastOperations
//          ops.foreach(op => makeFactoryMethod (model, op))

          import ooParadigm.testCapabilities._
          val compUnit = for {
            _ <- addTestCase(names.mangle("Test"), testCode)

            _ <- addMethod(names.mangle("makeSomething"), makeFactoryMethod (model, model.lastModelWithOperation.get.ops(0)))
          } yield()

          val testSuite = for {
            _ <- addTestSuite(
              names.addSuffix(names.mangle(names.conceptNameOf(model)), "Test"),
              compUnit
            )
          } yield ()

          addCompilationUnit(
            names.addSuffix(names.mangle(names.conceptNameOf(model)), "Test"),
            testSuite
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
