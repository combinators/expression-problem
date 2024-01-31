package org.combinators.ep.generator   /*DI:LI:AI*/

import cats.kernel.Monoid
import org.combinators.ep.domain.abstractions.{DataType, EqualsCompositeTestCase, EqualsTestCase, NotEqualsTestCase, Operation, TestCase, TypeRep}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.communication.{Request, SendRequest}
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Assertions, Booleans, Equality, Strings}

trait TestImplementationProvider[-AIP <: ApproachImplementationProvider] {
  def initialize(forApproach: AIP): Generator[forApproach.paradigm.ProjectContext, Unit]
  def test(forApproach: AIP)(testCase: TestCase): Generator[forApproach.paradigm.MethodBodyContext, Seq[forApproach.paradigm.syntax.Expression]]
}

object TestImplementationProvider {
  /** Allows to combine multiple [[TestImplementationProvider]] objects into one. */
  implicit def monoidInstance[AIP <: ApproachImplementationProvider]: Monoid[TestImplementationProvider[AIP]] =
    new Monoid[TestImplementationProvider[AIP]] {
      /** Returns an [[TestImplementationProvider]] which does not provide any implementation, and instead skips all
        * tests */
      def empty: TestImplementationProvider[AIP] = new TestImplementationProvider[AIP] {
        def initialize(forApproach: AIP): Generator[forApproach.paradigm.ProjectContext, Unit] = Command.skip
        def test(forApproach: AIP)(testCase: TestCase): Generator[forApproach.paradigm.MethodBodyContext, Seq[forApproach.paradigm.syntax.Expression]] =
          Command.lift(Seq.empty)
      }

      /** Combines two [[TestImplementationProvider]] objects by running test generation sequentially. */
      def combine(
        first: TestImplementationProvider[AIP],
        second: TestImplementationProvider[AIP]
      ): TestImplementationProvider[AIP] = new TestImplementationProvider[AIP] {
        def initialize(forApproach: AIP): Generator[forApproach.paradigm.ProjectContext, Unit] = {
          for {
            _ <- first.initialize(forApproach)
            _ <- second.initialize(forApproach)
          } yield ()
        }

        def test(forApproach: AIP)(testCase: TestCase):
          Generator[forApproach.paradigm.MethodBodyContext, Seq[forApproach.paradigm.syntax.Expression]] = {
          for {
            firstRes <- first.test(forApproach)(testCase)
            secondRes <- second.test(forApproach)(testCase)
          } yield firstRes ++ secondRes
        }
      }
    }

  def defaultAssertionBasedTests[P <: AnyParadigm, AIP[P <: AnyParadigm] <: ApproachImplementationProvider.WithParadigm[P]]
      (paradigm: P)
      (ffiAssertions: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type],
        ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type],
        ffiBooleans: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type],
        ffiStrings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]
      ): TestImplementationProvider[AIP[paradigm.type]] =
    new TestImplementationProvider[AIP[paradigm.type]] {
      def initialize(forApproach: AIP[paradigm.type]): Generator[forApproach.paradigm.ProjectContext, Unit] = {
        for {
          _ <- ffiAssertions.enable()
          _ <- ffiEquality.enable()
          _ <- ffiBooleans.enable()
          _ <- ffiStrings.enable()     // because test cases have names
        } yield ()
      }

      def prepareTestCase(forApproach: AIP[paradigm.type])
        (baseTpe: DataType,
          domainObject: DataTypeInstance,
          op: Operation,
          expected: InstanceRep,
          argInsts: Seq[InstanceRep]
        ): Generator[forApproach.paradigm.MethodBodyContext, (forApproach.paradigm.syntax.Type, forApproach.paradigm.syntax.Expression, forApproach.paradigm.syntax.Expression)] = {
        import forApproach.paradigm.methodBodyCapabilities._
        import AnyParadigm.syntax._
        for {
          tpe <- toTargetLanguageType(TypeRep.DataType(baseTpe))
          _ <- forApproach.resolveAndAddImport(tpe)
          exp <- forApproach.reify(expected)
          inst <- forApproach.instantiate(baseTpe, domainObject)
          args <- forEach (argInsts) { param => forApproach.reify(param) }
          requestArgs = op.parameters.zip(args).toMap
          resTpe <- toTargetLanguageType(op.returnType)
          _ <- forApproach.resolveAndAddImport(resTpe)
          res <- forApproach.dispatch(SendRequest(inst, baseTpe, Request(op, requestArgs)))
        } yield (resTpe, res, exp)
      }

      def makeEqualsTestCase(forApproach: AIP[paradigm.type])(testCase: EqualsTestCase):
        Generator[forApproach.paradigm.MethodBodyContext, Seq[forApproach.paradigm.syntax.Expression]] = {
        import ffiAssertions.assertionCapabilities._
        import ffiEquality.equalityCapabilities._
        for {
          (tpe, res, exp) <- prepareTestCase(forApproach)(testCase.baseTpe, testCase.domainObject, testCase.op, testCase.expected, testCase.params)
          assertion <- assertEquals(tpe, res, exp)
        } yield Seq(assertion)
      }

      def makeNotEqualsTestCase(forApproach: AIP[paradigm.type])(testCase: NotEqualsTestCase):
        Generator[forApproach.paradigm.MethodBodyContext, Seq[forApproach.paradigm.syntax.Expression]] = {
        import ffiAssertions.assertionCapabilities._
        import ffiEquality.equalityCapabilities._
        import ffiBooleans.booleanCapabilities._
        for {
          (tpe, res, exp) <- prepareTestCase(forApproach)(testCase.baseTpe, testCase.domainObject, testCase.op, testCase.expected, testCase.params)
          assertion <- assertNotEquals(tpe, res, exp)
        } yield Seq(assertion)
      }

      def makeEqualsCompositeTestCase(forApproach: AIP[paradigm.type])(testCase: EqualsCompositeTestCase):
        Generator[forApproach.paradigm.MethodBodyContext, Seq[forApproach.paradigm.syntax.Expression]] = {
        import ffiAssertions.assertionCapabilities._
        import ffiEquality.equalityCapabilities._
        import forApproach.paradigm._
        import methodBodyCapabilities._
        import syntax._
        import AnyParadigm.syntax._

        def makeChain(tpe: TypeRep, obj: Expression, ops: Seq[(Operation, Seq[InstanceRep])]): Generator[MethodBodyContext, (Type, Expression)] =
          ops match {
            case (op, argInsts) +: ops =>
              for {
                args <- forEach (argInsts) { param => forApproach.reify(param) }
                requestArgs = op.parameters.zip(args).toMap
                nextObj <-
                  tpe match {
                    case TypeRep.DataType(dtpe) =>
                      forApproach.dispatch(SendRequest(obj, dtpe, Request(op, requestArgs)))
                    case _ if ops.nonEmpty =>
                      throw new RuntimeException(s"Intermediate results in composite test cases must be domain data types (cannot dispatch to: ${tpe})")
                  }
                res <- makeChain(op.returnType, nextObj, ops)
              } yield res
            case Seq() =>
              for {
                resTpe <- toTargetLanguageType(tpe)
              } yield (resTpe, obj)
          }

        for {
          tpe <- toTargetLanguageType(TypeRep.DataType(testCase.baseTpe))
          _ <- forApproach.resolveAndAddImport(tpe)
          exp <- forApproach.reify(testCase.expected)
          inst <- forApproach.instantiate(testCase.baseTpe, testCase.startObject)
          (resTpe, res) <- makeChain(TypeRep.DataType(testCase.baseTpe), inst, testCase.ops)
          assertion <- assertEquals(resTpe, res, exp)
        } yield Seq(assertion)
      }

      def test(forApproach: AIP[paradigm.type])(testCase: TestCase):
        Generator[forApproach.paradigm.MethodBodyContext, Seq[forApproach.paradigm.syntax.Expression]] = {
        testCase match {
          case eq: EqualsTestCase => makeEqualsTestCase(forApproach)(eq)
          case neq: NotEqualsTestCase => makeNotEqualsTestCase(forApproach)(neq)
          case ceq: EqualsCompositeTestCase => makeEqualsCompositeTestCase(forApproach)(ceq)
          case _ => Command.lift(Seq.empty)
        }
      }
    }
}
