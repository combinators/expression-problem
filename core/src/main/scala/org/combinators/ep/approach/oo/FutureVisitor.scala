package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.communication.{ReceivedRequest, Request}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.{FindClass, Generics, ObjectOriented, ParametricPolymorphism}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider, Understands}

trait FutureVisitor extends ApproachImplementationProvider with FactoryConcepts {

  import paradigm._
  import syntax._

  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val impParadigm: Imperative.WithBase[MethodBodyContext,paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]

  import ooParadigm._
  lazy val Factory:Name = names.mangle("Factory")
  lazy val result = names.mangle("result")
  lazy val visitor = names.mangle("visitor")
  lazy val toConvert = names.mangle("other")   // looks better this way
  lazy val getResult: Name = names.mangle("getResult")
  lazy val accept: Name = names.mangle("accept")
  lazy val convert: Name = names.mangle("convert")
  lazy val visit: Name = names.mangle("visit")
  lazy val visitorClass: Name = names.mangle("Visitor")
  lazy val factoryClass: Name = names.mangle("Factory")
  lazy val visited: Name = names.mangle("visited")

  // HACK
  val finalized:Name    // sub package within each evolution that contains final classes

  // must be present!
  def registerLocally(tpe:DataType, paramType:Type) : Generator[ClassContext, Unit]

  // access type mapping capability
  def registerTypeMapping(model: Model): Generator[ProjectContext, Unit]

  /** Same as below but works without domain, providing you pass in the base type. */
  def computedBaseType[Context](ofBaseType:DataType)(implicit canFindClass: Understands[Context, FindClass[Name, Type]]): Generator[Context, Type] = {
    FindClass(Seq(names.mangle(names.conceptNameOf(ofBaseType)))).interpret(canFindClass)
  }

  /** Returns the base type of trivially. */
  def computedBaseType[Context](ofModel: Model)(implicit canFindClass: Understands[Context, FindClass[Name, Type]]): Generator[Context, Type] = {
    computedBaseType(ofModel.baseDataType)
  }

  // make Accept with abstract V
  /** Create standard signature to access the result of an operation
   * yields None since no return value for this void method
   */
  def makeAcceptSignature(genericType:Type): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for {
        _ <- setParameters(Seq((visitor, genericType)))
        voidType <- toTargetLanguageType(TypeRep.Unit)
        _ <- setReturnType(voidType)
      } yield None
  }

  /**
   * creates default implementation for accept using the customized Visitor that is created with each
   *
   * public void accept(${visitClass} visitor) {
   *    visitor.visit(this);
   * }
   *
   * @param visitClass
   * @return
   */
  def makeAcceptImplementation(visitClass:Type): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._
      for {
        // start from the accept signature and add a method body.
        _ <-  makeAcceptSignature(visitClass)
        args <- getArguments()   // get name, type, expression

        // invoke visit method on 'v' with 'this' as argument
        visitFunction <- getMember(args.head._3, visit)
        self <- selfReference()
        result <- apply(visitFunction, Seq(self))  // make the method invocation

        stmt <- liftExpression(result)
        _ <- addBlockDefinitions(Seq(stmt))
      } yield None

   }

  def addAcceptMethod(bodyGenerator:Generator[MethodBodyContext, Option[Expression]]): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    addMethod(accept, bodyGenerator)
  }

  /**
   *
   */
  def makeConvertSignature(from: Type, to:Type): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for {
      _ <- setParameters(Seq((toConvert, from)))
      _ <- setReturnType(to)
    } yield None
  }

  /**
   * public mi.Exp<Visitor> convert(m0.Exp<Visitor> exp) {
   *     Visitor visitor = new Visitor();
   *     exp.accept(visitor);
   *     return visitor.getResult();
   * }
   *
   *  NOW BECOMES
   *
   * public Exp<ep.m0.finalized.Visitor> convert(ep.Exp<ep.m0.finalized.Visitor> other) {
   *    Visitor visitor = new Visitor();
   *    other.accept(visitor);
   *    return visitor.getResult();
   * }
   *
   * @param visitClass
   * @return
   */
  def makeConvertImplementation(model:Model, visitClass:Type, baseExp:Type): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._

    import impParadigm.imperativeCapabilities._
    for {
      latestExp <- toTargetLanguageType(TypeRep.DataType(model.baseDataType))
      fromExp <- applyType(baseExp, Seq(visitClass))
      toExp <- applyType(latestExp, Seq(visitClass))
      _ <- makeConvertSignature(fromExp, toExp)

      args <- getArguments()   // get name, type, expression

      // construct the visitor object for the given type (and parameters).
      instantiateVisitor <- instantiateObject(visitClass, Seq.empty)
      fname <- freshName(visitor)

      visitorVar <- declareVar(fname, visitClass, Some(instantiateVisitor))
      method <- getMember(args.head._3, accept)   // dispatch 'accept' to 'fromExp'

      // apply to method with the visitor, resulting in the expression
      callExpr <- apply(method, Seq(visitorVar))
      stmt <- liftExpression(callExpr)
      _ <- addBlockDefinitions(Seq(stmt))

      // obtain actual result expression by getting method and then invoking it (with empty seq)
      resultOfMethod <- getMember(visitorVar,  getResult)
      result <- apply(resultOfMethod, Seq.empty)
    } yield Some(result)
  }

  /**
   * Adds a convert method for given context.
   *
   * @param bodyGenerator
   * @return
   */
  def addConvertMethod(bodyGenerator:Generator[MethodBodyContext, Option[Expression]]): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    addMethod(convert, bodyGenerator)
  }

  /**
   * public void $$(mi.Exp<Visitor> visited) {
   *   this.result = visited;
   * }
   */
  def makeVisitImplementation(resultType:Type) : Generator[MethodBodyContext, Option[Expression]] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._
    for {
      // this.result = to
      voidType <- toTargetLanguageType(TypeRep.Unit)
      _ <- setReturnType(voidType)
      _ <- setParameters(Seq((visited, resultType)))

      self <- selfReference()
      field <- getMember(self, result)
      args <- getArguments()
      stmt <- assignVar(field, args.head._3)
      _ <- addBlockDefinitions(Seq(stmt))
    } yield None
  }

  /** Create a method implementation that simply returns field as an expression, with appropriate return type for operation.
   *
   * {{{
   *   RETURNTYPE ???() { return FIELD; }
   * }}}
   *
   * @param field
   * @return
   */
  def returnValue(tpe: Type, field:Expression): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for {
      _ <- resolveAndAddImport(tpe)
      _ <- setReturnType(tpe)
    } yield (Some(field))
  }

  /**
   *??? lit(Double value) {
   *   return new Lit(value);
   * }
   *
   *??? (ep.Exp<ep.m0.finalized.Visitor> left, ep.Exp<ep.m0.finalized.Visitor> right) {
   *   return new Add(this.convert(left), this.convert(right));
   * }
   */
  def futureCreateFactoryDataTypeCase(model:Model, tpeCase:DataTypeCase, paramBaseClass:Type, tpe:Type, isStatic:Boolean = false): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    val definingModel = model.findTypeCase(tpeCase)

    for {
      _ <- registerTypeMapping(definingModel.getOrElse(model))
    } yield ()

    for {
      _ <- createFactorySignatureDataTypeCase(definingModel.getOrElse(model), tpeCase, paramBaseClass, tpe, isStatic)

      opInst <- findClass(factoryInstanceDataTypeCase(Some(model), tpeCase): _*)    // should check!
      _ <- resolveAndAddImport(opInst)

      // set method invocation to 'convert' with these arguments
      self <- selfReference()

      convertMethod <- getMember(self, convert)
      argSeq <- getArguments()

      convertedArgSeq <- forEach(argSeq) { arg =>
        if (tpeCase.isRecursive(model)) {
          apply(convertMethod, Seq(arg._3))
        } else {
          Command.lift[MethodBodyContext,Expression](arg._3)
        }
      }

      res <- instantiateObject(opInst, convertedArgSeq)
    } yield Some(res)
  }

  /**
   * Make Finalized factory which each finalized datatype implements. Now a class, something like:
   *
   * public class Factory implements ep.m0.Factory<ep.m0.finalized.Visitor> {
   *   public Exp<ep.m0.finalized.Visitor> lit(Double value) {
   *     return new Lit(value);
   *   }
   *
   *   public Exp<ep.m0.finalized.Visitor> add(ep.Exp<ep.m0.finalized.Visitor> left, ep.Exp<ep.m0.finalized.Visitor> right) {
   *     return new Add(this.convert(left), this.convert(right));
   *   }
   *
   *   public Exp<ep.m0.finalized.Visitor> convert(ep.Exp<ep.m0.finalized.Visitor> other) {
   *     Visitor visitor = new Visitor();
   *     other.accept(visitor);
   *     return visitor.getResult();
   *   }
   * }
   *
   * Each instance of Exp must be parameterized with ep.m#.finalized.Visitor
   *
   * CAN'T FORGET to call 'convert' for those recursive datatypes to be sure they work.
   */
  def makeFinalizedFactory(domain:Model): Generator[ClassContext, Unit] = {

    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._
    for {

      // even though this expressly calls for ep.m#.Exp it becomes just Exp
      // so missing the import.
      resultTpe <- findClass(names.mangle(domain.name), names.mangle(domain.baseDataType.name))
      _ <- resolveAndAddImport(resultTpe)

      visitorTpe <- findClass(names.mangle(domain.name), finalized, visitorClass)
      _ <- resolveAndAddImport(visitorTpe)

      // extends Exp<Visitor>
      tt <- computedBaseType(domain)
      topLevelType <- applyType(tt, Seq(visitorTpe))

      returnType <- applyType(resultTpe, Seq[Type](visitorTpe))
      factory <- findClass(names.mangle(domain.name), Factory)
      factoryType <- applyType(factory,Seq[Type](visitorTpe))

      _ <- addImplemented(factoryType)

      _ <- forEach(domain.flatten.typeCases) { tpeCase => {
        for {
          // These methods with recursive values must call convert; in addition, they must be properly
          // defined to use appropriate ep.m#.Exp based on where the data type was defined... TRICK
          _ <- addMethod (names.mangle(names.instanceNameOf(tpeCase)), futureCreateFactoryDataTypeCase(domain, tpeCase, topLevelType, returnType, false))
        } yield ()
      }
      }

      _ <- addConvertMethod(makeConvertImplementation(domain, visitorTpe, topLevelType))
//      // do this last so methods are declared as default: NOW A CLASS!
//      _ <- setInterface()
    } yield ()
  }

  /**
   * Make Visitor class in each finalized package.
   *
   * Each instance of Exp must be parameterized with ep.m#.finalized.Visitor
   */
  def makeFinalizedVisitor(domain:Model): Generator[ClassContext, Unit] = {

    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._
    for {
      // even though this expressly calls for ep.m#.Exp it becomes just Exp
      // so missing the import.
      resultTpe <- findClass(names.mangle(domain.name), names.mangle(domain.baseDataType.name))

      _ <- resolveAndAddImport(resultTpe)
      visitorTpe <- findClass(names.mangle(domain.name), finalized, visitorClass)

      parameterizedTpe <- applyType(resultTpe, Seq[Type](visitorTpe))

      _ <- addField(result, parameterizedTpe)
      field <- getField(result)

      _ <- addMethod(getResult, returnValue(parameterizedTpe, field))

      _ <- addMethod(visit, makeVisitImplementation(parameterizedTpe))
    } yield ()
  }

  /**
   * Adds an accept method for given context.
   *
   * @param bodyGenerator
   * @return
   */
  def addVisitorToProject(domain:Model, bodyGenerator:Generator[ClassContext, Unit]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    addClassToProject(bodyGenerator, names.mangle(domain.name), finalized, visitorClass)
  }

  /**
   * Adds an accept method for given context.
   *
   * @param bodyGenerator
   * @return
   */
  def addFactoryToProject(domain:Model, bodyGenerator:Generator[ClassContext, Unit]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    addClassToProject(bodyGenerator, names.mangle(domain.name), finalized, factoryClass)
  }

  // factories for all data types, as they are added in...

  // also need finalized visitors with side effects to be able to retrieve result.
}
