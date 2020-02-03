package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.communication.{ReceivedRequest, Request}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.{Generics, ObjectOriented, ParametricPolymorphism}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

trait FutureVisitor extends ApproachImplementationProvider {

  import paradigm._
  import syntax._

  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val impParadigm: Imperative.WithBase[MethodBodyContext,paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]

  import ooParadigm._

  lazy val result = names.mangle("result")
  lazy val visitor = names.mangle("visitor")
  lazy val toConvert = names.mangle("toConvert")
  lazy val getResult: Name = names.mangle("getResult")
  lazy val accept: Name = names.mangle("accept")
  lazy val convert: Name = names.mangle("convert")
  lazy val visit: Name = names.mangle("visit")
  lazy val visitorClass: Name = names.mangle("Visitor")
  lazy val visited: Name = names.mangle("visited")

  // HACK
  val finalized:Name    // sub package within each evolution that contains final classes

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

  /**
   * Adds an accept method for given context.
   *
   * @param bodyGenerator
   * @return
   */
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
   *
   * public mi.Exp<Visitor> convert(m0.Exp<Visitor> exp) {
   *     Visitor visitor = new Visitor();
   *     exp.accept(visitor);
   *     return visitor.getResult();
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

  // make Visitor class in each finalized package.
  def makeFinalizedVisitor(domain:Model): Generator[ClassContext, Unit] = {

    import ooParadigm.classCapabilities._
    import genericsParadigm.classCapabilities._
    for {
      resultTpe <- toTargetLanguageType(TypeRep.DataType(domain.baseDataType))
      _ <- resolveAndAddImport(resultTpe)
      visitorTpe <- findClass(visitorClass)

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
  def addFinalizedVisitor(domain:Model, bodyGenerator:Generator[ClassContext, Unit]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    addClassToProject(bodyGenerator, names.mangle(domain.name), finalized, visitorClass)
  }

  // factories for all data types, as they are added in...

  // also need finalized visitors with side effects to be able to retrieve result.
}
