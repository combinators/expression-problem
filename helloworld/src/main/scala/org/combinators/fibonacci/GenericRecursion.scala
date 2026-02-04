package org.combinators.fibonacci

/**
 * Not yet completed.
 *
 * This effort will eventually yield the ability to generate a recursive function simply by
 * identifying (a) the base cases; and (b) the recursive call structure.
 */

import org.combinators._
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Assertions, Booleans, Equality, RealArithmetic}
import org.combinators.ep.generator.paradigm.{AnyParadigm, Functional, ObjectOriented, control}
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider}

/***

Want to get to this.... memoization

package fibonacci;
import java.util.*;

public class Fib {
    Hashtable<Integer,Integer> results = new Hashtable<>();
    public Integer fib(Integer n) {
        if (results.containsKey(n)) { return results.get(n); }
        if ((n <= 0)) {
            return 0;
        } else if ((n <= 1)) {
            return 1;
        } else {
            int stored = (this.fib((n - 1)) + this.fib((n - 2)));
            results.put(n, stored);
            return stored;
        }
    }
}




 * @param label
 */


trait GenericRecursion extends Expansion {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]

  val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int]
  val ffiRealArithmetic : RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val ffiBooleans : Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ffiAssertions : Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ffiEquality : Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]

  val nminus1 = Sub(VariableFormula("n", TypeRep.Int), ConstantFormula(1, TypeRep.Int))
  val nminus2 = Sub(VariableFormula("n", TypeRep.Int), ConstantFormula(2, TypeRep.Int))

  val cond0 = LE(VariableFormula("n", TypeRep.Int), ConstantFormula(0, TypeRep.Int))
  val cond1 = LE(VariableFormula("n", TypeRep.Int), ConstantFormula(1, TypeRep.Int))
  val fib_n_minus_one = UnaryFunctionCall(fibRecursive, nminus1)
  val fib_n_minus_two = UnaryFunctionCall(fibRecursive, nminus2)
  val add = Add(fib_n_minus_one, fib_n_minus_two)

  // names.mangle(...)
  lazy val fibRecursive:String  = "fib"
  lazy val fibName:String = "fib_lucas"
  lazy val lucasName:String = "lucas"

  lazy val testFibName:paradigm.syntax.Name  = names.mangle("fibTest")
  lazy val nName:paradigm.syntax.Name  = names.mangle("n")

  type IfBlockType

  def if_then_else(cond: paradigm.syntax.Expression,
                   ifBlock: Generator[paradigm.MethodBodyContext, IfBlockType],
                   ifElseBlocks: Seq[(paradigm.syntax.Expression, Generator[paradigm.MethodBodyContext, IfBlockType])],
                   elseBlock: Generator[paradigm.MethodBodyContext, IfBlockType]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]

  def cast_double_to_int(inner:paradigm.syntax.Expression) :  Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression]

  def find_method_recursive(name:paradigm.syntax.Name) : Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression]

  // used for tests
  def find_method(name:paradigm.syntax.Name) : Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression]

  // add method (in OO this would require construction of a class first...)
  def add_methods(methods:Map[String, Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]]):
      Generator[paradigm.CompilationUnitContext, Unit]

  def return_in_if(toReturn:Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression]): Generator[paradigm.MethodBodyContext, IfBlockType]

  def make_fibonacci(): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import paradigm.methodBodyCapabilities._

    val frame = new Structure(
      conditions = Seq((cond0, ConstantFormula(0, TypeRep.Int)), (cond1, ConstantFormula(1, TypeRep.Int))),
      default = add)

    for {
      // NEED to do first so these are arguments ready
      intType <- toTargetLanguageType(TypeRep.Int)

      _ <- paradigm.methodBodyCapabilities.setParameters(Seq((nName, intType)))
      _ <- paradigm.methodBodyCapabilities.setReturnType(intType)

      res <- finish(frame)
    } yield res
  }

  def finish(frame:Structure): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach

    for {
      condition_sequence <- frame.conditions
        .foldLeft(Command.lift[paradigm.MethodBodyContext,Seq[(paradigm.syntax.Expression, Generator[paradigm.MethodBodyContext, IfBlockType])]](Seq.empty))
        { case (result,cond) =>
          for {
            fc <- expand(cond._1)
            ff <- expand(cond._2)
            old_result <- result
          } yield old_result :+ (fc,return_in_if(Command.lift(ff)))
        }

      // the default recursive case is isolated
      fdefault <- Command.lift(expand(frame.default))

      res <- if_then_else(condition_sequence.head._1,
        condition_sequence.head._2,
        condition_sequence.tail,
        return_in_if(fdefault))

    } yield res
  }

  def make_fibonacci_with_lucas(lucas_name:String, fib_lucas_name:String): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import paradigm.methodBodyCapabilities._

    /**
     * Take advantage of observation that:
     *
     *  Fib(x+y) = [(Fib(x)*Lucas(y) + Fib(y)*Locas(x)] / 2
     *
     *  With this you can declare
     *
     *   n1 = floor(n/2)
     *   n2 = n - n1
     *   Fib(n) = [(Fib(n1)*Lucas(n2) + Fib(n2)*Lucas(n1)] / 2
     *
     */
    val n1 = DoubleToInt(Floor(Div(VariableFormula("n", TypeRep.Int), ConstantFormula(2, TypeRep.Int))))
    val n2 = Sub(VariableFormula("n", TypeRep.Int), n1)

    val fib_n1 = UnaryFunctionCall(fib_lucas_name, n1)
    val fib_n2 = UnaryFunctionCall(fib_lucas_name, n2)
    val lucas_n1 = UnaryFunctionCall(lucas_name, n1)
    val lucas_n2 = UnaryFunctionCall(lucas_name, n2)

    val fibRecursion = Div(Add(Mult(fib_n1, lucas_n2),Mult(fib_n2, lucas_n1)), ConstantFormula(2, TypeRep.Int))

    val frame = new Structure(
      conditions = Seq(
        (cond0, ConstantFormula(0, TypeRep.Int)),
        (cond1, ConstantFormula(1, TypeRep.Int)),
        (LE(VariableFormula("n", TypeRep.Int), ConstantFormula(2, TypeRep.Int)), ConstantFormula(1, TypeRep.Int)),
        (LE(VariableFormula("n", TypeRep.Int), ConstantFormula(3, TypeRep.Int)), ConstantFormula(2, TypeRep.Int)),
      ),
      default = fibRecursion
    )

    for {
      // NEED to do first so these are arguments ready
      intType <- toTargetLanguageType(TypeRep.Int)

      _ <- paradigm.methodBodyCapabilities.setParameters(Seq((nName, intType)))
      _ <- paradigm.methodBodyCapabilities.setReturnType(intType)

      res <- finish(frame)
    } yield res
  }

  def make_lucas_with_fibonacci(fib_lucas_name:String): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import paradigm.methodBodyCapabilities._

    /**
     * Take advantage of observation that:
     *
     *   Lucas(n) = Fib(n-1) + F(n+1) for n > 1
     */
    val nplus1 = Add(VariableFormula("n", TypeRep.Int), ConstantFormula(1, TypeRep.Int))
    val fib_n_plus_one = UnaryFunctionCall(fib_lucas_name, nplus1)
    val fib_n_minus_one = UnaryFunctionCall(fib_lucas_name, nminus1)
    val lucasRecursion = Add(fib_n_plus_one, fib_n_minus_one)

    val frame = new Structure(
      conditions = Seq(
        (cond0, ConstantFormula(2, TypeRep.Int)),
        (cond1, ConstantFormula(1, TypeRep.Int))),
      default = lucasRecursion
    )

    for {
      // NEED to do first so these are arguments ready
      intType <- toTargetLanguageType(TypeRep.Int)

      _ <- paradigm.methodBodyCapabilities.setParameters(Seq((nName, intType)))
      _ <- paradigm.methodBodyCapabilities.setReturnType(intType)

      res <- finish(frame)
    } yield res
  }

  def make_unit(): Generator[paradigm.CompilationUnitContext, Unit] = {
    add_methods(Map(
      fibRecursive -> make_fibonacci(),
      fibName -> make_fibonacci_with_lucas(lucasName, fibName),
      lucasName -> make_lucas_with_fibonacci(fibName)
    ))
  }

  def make_fibonacci_test(): Generator[paradigm.MethodBodyContext, Seq[paradigm.syntax.Expression]] = {
    import ffiEquality.equalityCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      intType <- toTargetLanguageType(TypeRep.Int)
      func_straight_fib <- find_method(names.mangle(fibRecursive))
      func_lucas_fib <- find_method(names.mangle(fibName))

      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)

      seven <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 7)
      thirteen <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 13)
      twentynine <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 29)

      total = Seq((zero,zero), (one,one), (seven, thirteen))

      tests <- total
        .foldLeft(Command.lift[paradigm.MethodBodyContext,Seq[paradigm.syntax.Expression]](Seq.empty))
          { case (result,(input,actual)) =>
            for {
              check <- apply(func_straight_fib, Seq(input))
              assertcheck <- ffiAssertions.assertionCapabilities.assertEquals(intType, check, actual)
              old_result <- result   // needed to "extract out the generator"

              check2 <- apply(func_lucas_fib, Seq(input))
              assertcheck2 <- ffiAssertions.assertionCapabilities.assertEquals(intType, check2, actual)

            } yield old_result :+ assertcheck :+ assertcheck2
          }

    } yield tests
  }

  def make_test() : Generator[paradigm.TestContext, Unit] = {
    for {
      _ <- paradigm.testCapabilities.addTestCase(make_fibonacci_test(), testFibName)
    } yield ()
  }

  def make_project(): Generator[paradigm.ProjectContext, Unit] = {
    for {
      _ <- paradigm.projectCapabilities.addCompilationUnit(make_unit(), names.mangle(fibRecursive))
      _ <- paradigm.projectCapabilities.addCompilationUnit(paradigm.compilationUnitCapabilities.addTestSuite(testFibName, make_test()), testFibName)
    } yield ()
  }
}

// below ensures we can generate both functional and imperative solutions

object GenericRecursionProvider {
  type WithParadigm[P <: AnyParadigm] = GenericRecursion { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def functional[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   func:  Functional.WithBase[base.type],
   c1: control.Functional.WithBase[base.MethodBodyContext, base.type],
   c2: Arithmetic.WithBase[base.MethodBodyContext, base.type, Int],
   c3: Assertions.WithBase[base.MethodBodyContext, base.type],
   c4: Equality.WithBase[base.MethodBodyContext, base.type],
   c5: Booleans.WithBase[base.MethodBodyContext, base.type],
   c6: RealArithmetic.WithBase[base.MethodBodyContext, base.type, Double]
  )
  : GenericRecursionProvider.WithParadigm[base.type] =
    new GenericRecursion {
      override val paradigm: base.type = base
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider

      override val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int] = c2
      override val ffiAssertions: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type] = c3
      override val ffiEquality : Equality.WithBase[paradigm.MethodBodyContext, paradigm.type] = c4
      override val ffiBooleans : Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type] = c5
      override val ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double] = c6

      override type IfBlockType = paradigm.syntax.Expression

      // nothing to do
      override def cast_double_to_int(inner:paradigm.syntax.Expression):  Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
         for {
          res <- Command.lift(inner)
        } yield res
      }

      override def if_then_else(cond: paradigm.syntax.Expression, ifBlock: Generator[paradigm.MethodBodyContext, IfBlockType], ifElseBlocks: Seq[(paradigm.syntax.Expression, Generator[paradigm.MethodBodyContext, IfBlockType])], elseBlock: Generator[paradigm.MethodBodyContext, IfBlockType]): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        for {
          res <- c1.functionalCapabilities.ifThenElse(cond, ifBlock, ifElseBlocks, elseBlock)
        } yield Some(res)
      }

      override def find_method_recursive(name: paradigm.syntax.Name): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = find_method(name)
      override def find_method(name: paradigm.syntax.Name): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
        func.methodBodyCapabilities.findMethod(Seq(name))
      }

      override def add_methods(methods: Map[String, Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]]): Generator[paradigm.CompilationUnitContext, Unit] = {
        import AnyParadigm.syntax._
        for {
          _ <- forEach(methods.toList) { case (name, spec) =>
            func.compilationUnitCapabilities.addMethod(names.mangle(name), spec.map(x => x.get))
          }
        } yield ()
      }

      override def return_in_if(toReturn: Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression]): Generator[paradigm.MethodBodyContext, IfBlockType] = {
        toReturn
      }
    }

  def imperative[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   obj:  ObjectOriented.WithBase[base.type],
   c1: control.Imperative.WithBase[base.MethodBodyContext, base.type],
   c2: Arithmetic.WithBase[base.MethodBodyContext, base.type, Int],
   c3: Assertions.WithBase[base.MethodBodyContext, base.type],
   c4: Equality.WithBase[base.MethodBodyContext, base.type],
   c5: Booleans.WithBase[base.MethodBodyContext, base.type],
   c6: RealArithmetic.WithBase[base.MethodBodyContext, base.type, Double]
  )
  : GenericRecursionProvider.WithParadigm[base.type] =
    new GenericRecursion {
      override val paradigm: base.type = base
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider

      override val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int] = c2
      override val ffiAssertions: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type] = c3
      override val ffiEquality : Equality.WithBase[paradigm.MethodBodyContext, paradigm.type] = c4
      override val ffiBooleans : Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type] = c5
      override val ffiRealArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double] = c6

      override type IfBlockType = Unit
      lazy val fibClass:paradigm.syntax.Name  = names.mangle("Fib")

      override def cast_double_to_int(inner:paradigm.syntax.Expression):  Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
        import paradigm.methodBodyCapabilities._
        import obj.methodBodyCapabilities._
          for {
            itype <- toTargetLanguageType(TypeRep.Int)
            asInt <- castObject(itype, inner)
          } yield asInt
      }

      override def if_then_else(cond: paradigm.syntax.Expression, ifBlock: Generator[paradigm.MethodBodyContext, IfBlockType], ifElseBlocks: Seq[(paradigm.syntax.Expression, Generator[paradigm.MethodBodyContext, IfBlockType])], elseBlock: Generator[paradigm.MethodBodyContext, IfBlockType]): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        for {
          resultStmt <- c1.imperativeCapabilities.ifThenElse(cond, ifBlock, ifElseBlocks, Some(elseBlock))
          _ <- paradigm.methodBodyCapabilities.addBlockDefinitions(Seq(resultStmt))
        } yield None
      }

      override def find_method_recursive(name: paradigm.syntax.Name): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
        for {
          self <- obj.methodBodyCapabilities.selfReference()
          res <- obj.methodBodyCapabilities.getMember(self, name)
        } yield res
      }

      override def find_method(name: paradigm.syntax.Name): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
        for {
          typ <- obj.methodBodyCapabilities.findClass(fibClass)
          fib <- obj.methodBodyCapabilities.instantiateObject(typ, Seq.empty)
          res <- obj.methodBodyCapabilities.getMember(fib, name)
        } yield res
      }

      override def add_methods(methods:Map[String, Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]]) :
        Generator[paradigm.CompilationUnitContext, Unit] = {
        import AnyParadigm.syntax._

          def makeMethods(): Generator[obj.ClassContext, Unit] = {
            for {
              _ <- forEach(methods.toList) { case (name, spec) =>
                obj.classCapabilities.addMethod(names.mangle(name), spec)
              }
            } yield ()
          }

          obj.compilationUnitCapabilities.addClass(fibClass, makeMethods())
      }

      override def return_in_if(toReturn: Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression]): Generator[paradigm.MethodBodyContext, IfBlockType] = {
        for {
          resultExp <- toReturn
          resultStmt <- c1.imperativeCapabilities.returnStmt(resultExp)
          _ <- paradigm.methodBodyCapabilities.addBlockDefinitions(Seq(resultStmt))
        } yield None
      }
    }
}
