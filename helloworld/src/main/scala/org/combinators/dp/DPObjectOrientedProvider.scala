package org.combinators.dp

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Booleans, Console, Equality, RealArithmetic, Strings}
import org.combinators.ep.generator.paradigm.{Generics, ObjectOriented, ParametricPolymorphism}
import org.combinators.ep.generator.NameProvider
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.model.Model

/** Any OO approach will need to properly register type mappings and provide a default mechanism for finding a class
 * in a variety of contexts. This trait provides that capability
 */
trait DPObjectOrientedProvider extends DPProvider with Utility with TopDownStrategy with BottomUpStrategy {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]

  val names: NameProvider[paradigm.syntax.Name]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val arithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val realArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val console: Console.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val strings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val booleans: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type]

  import paradigm._
  import syntax._
  import ooParadigm._

  // if not memo, then this will be defined and added
  lazy val resultVarName = names.mangle("result")

  /**
   * Method creates a compute() method with no arguments that invokes helper method:
   *
   *   public Integer compute() {
   *     return this.helper(this.n);
   *   }
   */
  def make_compute_method(model:Model): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    for {
      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setReturnType(intType)
      self <- ooParadigm.methodBodyCapabilities.selfReference()
      helperMethod <- ooParadigm.methodBodyCapabilities.getMember(self, helperName)

      // convert arguments into Iteration values
      args <- forEach(model.bounds) { boundExpr => for {
          arg1 <- max_bound_in_method(boundExpr)
        } yield arg1
      }
      //field <- ooParadigm.methodBodyCapabilities.getMember(self, nName)
      invocation <- apply(helperMethod, args)
    } yield Some(invocation)
  }

  // must be implemented by another -- the test cases refer to the class name passed in as the implementation
  def makeTestCase(implementation:String): Generator[TestContext, Unit]

  /** Trying out some new capabilities */
  def implement(model: Model, option:GenerationOption): Generator[ProjectContext, Unit] = {
    // new stuff goes here.
    // handle Top/Bottom and properly set memo when TD
    var isTopDown = false
    var useMemo = false
    option match {
      case td:TopDown =>
        useMemo = td.memo
        isTopDown = true

      case _:BottomUp =>
        isTopDown = false
    }

    for {
      // The code below generates the actual class, based on model.problem
      _ <- if (isTopDown) {
        make_top_down(useMemo, model)
      } else {
        make_bottom_up(model)
      }

      _ <- paradigm.projectCapabilities.addCompilationUnit(
        paradigm.compilationUnitCapabilities.addTestSuite(names.mangle("Test" + model.problem), makeTestCase(model.problem))
      )
    } yield ()
  }

}
