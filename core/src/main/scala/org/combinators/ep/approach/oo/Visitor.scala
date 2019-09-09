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

  import paradigm._
  import ooParadigm._
  import polymorphics._
  import genericsParadigm._

  import syntax._

  /**
   * Dispatch in visitor we need to find context on which to accept a visitor.
   *
   * That is, e.getLeft().accept(new Eval()) + e.getRight().accept(new Eval());
   *
   *

  override def dispatch(expr:Expression, op:Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    Java(s"$expr.accept(new ${op.concept}($args))").expression()
  }

   * @param message
   * @return
   */
  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {

      method <- getMember(message.to, names.mangle("accept"))   // things which are code-generated use the '<-' handles unpacking results
      op = message.request.op
      visitorType <- findClass(names.conceptNameOf(op))    // each visitor is named based on operation
      _ <- resolveAndAddImport(visitorType)              // gives resulting import statement (if needed)
      visitor <- instantiateObject(visitorType, op.parameters.map(param => message.request.arguments(param)))

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
   * assertEquals(new Double(3.0), new Add(new Lit(new Double(1.0)), new Lit(new Double(2.0))).accept(new Eval()));
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
      rt <- findClass(names.conceptNameOf(tpeCase))
      _ <- resolveAndAddImport(rt)
      ctor <- getConstructor(rt)
      res <- apply(ctor, args)
    } yield res
  }

  /**
   * Not part of approach implementation provider but was needed for OO provider and was used to provide code
   * for making the actual signature of the
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

  /**
   * Not part of approach implementation provider but was needed for OO provider and was used to provide code
   * for making the actual signature of the
   *
   *  public abstract <R> R accept(Visitor<R> v);
   *
   * @return
   */
  def makeAcceptSignature(): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities.{toTargetLanguageType => _, _}
    import polymorphics.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      _ <- addTypeParameter(names.mangle("R"), Command.skip)    // R by itself, since not extending any other type parameter (hence Skip)
      args <- getTypeArguments()    // this returns mangled "R" name and gets list of all type parameters
      _ <- setReturnType(args.head)
      visitorClassType <- findClass(names.mangle("Visitor"))
      visitorType  <- applyType (visitorClassType, args)   // works because only one
      _ <- setParameters(Seq((names.mangle("v"), visitorType)))      // a pair (name,type) of only one sequence

    } yield ()
  }

  /**
   *

  public <R> R accept(Visitor<R> v) {
        return v.visit(this);
    }

   * @return
   */
  def makeAcceptImplementation(): Generator[ClassContext, Unit] = {
    val makeBody: Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._

      for {
        _ <- makeAcceptSignature()
        self <- selfReference()
        args <- getArguments()   // get name, type, expression

        visitFunction <- getMember(args.head._3, names.mangle("visit"))
        result <- apply(visitFunction, Seq(self))  // make the method invocation
      } yield Some(result)
    }

    import ooParadigm.classCapabilities._
    addMethod(names.mangle("accept"), makeBody)
  }


  /**
   * Define the base class for Exp

    package visitor;
    public abstract class Exp {
       public abstract <R> R accept(Visitor<R> v);
    }

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
        _ <- addAbstractMethod(names.mangle("accept"), visitorSignature)
      } yield ()
    }

    addClassToProject(names.conceptNameOf(tpe), makeClass)
  }

  def makeField(att: Attribute): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities.{toTargetLanguageType => _, _}
    for {
      ft <- toTargetLanguageType(att.tpe)
      _ <- resolveAndAddImport(ft)
      _ <- addField(names.instanceNameOf(att), ft)
    } yield ()
  }

  def makeImplementation(
                          tpe: DataType,
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
      atts = tpeCase.attributes.zip(attAccessors).toMap
      allArgs <- getArguments()
      args = allArgs.map { case (name, _, exp) => (name, exp) }.toMap
      result <-
        domainSpecific.logic(this)(
          ReceivedRequest(
            tpe,
            tpeCase,
            thisRef,
            atts,
            Request(op, op.parameters.map(param => (param, args(names.mangle(param.name)))).toMap)
          )
        )
    } yield Some(result)
  }

  def makeConstructor(tpeCase: DataTypeCase): Generator[ConstructorContext, Unit] = {
    import ooParadigm.constructorCapabilities._
    for {
      params <- forEach (tpeCase.attributes) { att: Attribute =>
        for {
          at <- super.toTargetLanguageType(att.tpe)
          _ <- resolveAndAddImport(at)
        } yield (names.instanceNameOf(att), at)
      }
      _ <- setParameters(params)
      args <- getArguments()
      _ <- forEach(args) { case (name, _, exp) => initializeField(name, exp) }
    } yield ()
  }

  def makeDerived(tpe: DataType, tpeCase: DataTypeCase, ops: Seq[Operation], domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      for {
        pt <- super.toTargetLanguageType(TypeRep.DataType(tpe))
        _ <- resolveAndAddImport(pt)
        _ <- addParent(pt)
        _ <- forEach (tpeCase.attributes) { att => makeField(att) }
        _ <- addConstructor(makeConstructor(tpeCase))
        _ <- forEach (tpeCase.attributes) { att => makeGetter(att) }
        _ <- makeAcceptImplementation()
      } yield ()
    }
    addClassToProject(names.conceptNameOf(tpeCase), makeClass)
  }

  /**
   *

    public Exp getRight() {
      return this.right;
    }

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
        self <- selfReference()
        result <- getMember(self, names.instanceNameOf(att))
      } yield Some(result)
    }

    import ooParadigm.classCapabilities._
    addMethod("get" + names.conceptNameOf(att), makeBody)
  }

  /**
   * This places all units into the Project Context.
   *
   * @param domain
   * @param domainSpecific
   * @return
   */
  def implement(domain: Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    val flatDomain = domain.flatten
    for {
      _ <- makeBase(flatDomain.baseDataType, flatDomain.ops)
      _ <- forEach (flatDomain.typeCases) { tpeCase =>
        makeDerived(flatDomain.baseDataType, tpeCase, flatDomain.ops, domainSpecific)
      }
      _ <- makeVisitorInterface()
      _ <- forEach (flatDomain.ops) { op =>
        makeDerived(flatDomain.baseDataType, tpeCase, flatDomain.ops, domainSpecific)
      }
      _ <- makeVisitImplementation()
    } yield ()
  }
}

object Visitor {
  type WithParadigm[P <: AnyParadigm] = Traditional { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (nameProvider: NameProvider, base: P)
  (oo: ObjectOriented.WithBase[base.type]): Traditional.WithParadigm[base.type] =
    new Traditional {
      override val names: NameProvider = nameProvider
      override val paradigm: base.type = base
      override val ooParadigm: ObjectOriented.WithBase[paradigm.type] = oo
    }
}