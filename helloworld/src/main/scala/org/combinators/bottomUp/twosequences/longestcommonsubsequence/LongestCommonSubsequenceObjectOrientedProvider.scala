package org.combinators.bottomUp.twosequences.longestcommonsubsequence

import org.combinators.bottomUp.twosequences.TwoSequencesUtility
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, ObjectOriented}
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider, Understands}
import org.combinators.model.{AdditionExpression, Assignment, Formula, Model, Relation, StringEqualCondition}

trait LongestCommonSubsequenceObjectOrientedProvider extends LongestCommonSubsequenceProvider with TwoSequencesUtility {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val names: NameProvider[paradigm.syntax.Name]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val arithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val console: Console.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]

  import ooParadigm._
  import paradigm._
  import syntax._

  lazy val message: String = "message"
  lazy val main: String = "main"
  lazy val testName = names.mangle("TestSuite")

  def getter(attr: String): String = {
    "get" + attr.capitalize
  }

//  class TwoSequenceStringDP extends Model {
//    override val baseType:String = "String"
//    override val dimensionality:Int = 2
//
//    // if s1[r] == s1[c], dp[r+1][c+1] = dp[r][c] + 1
//
//    override val relation = new Relation(,
//      new StringEqualCondition(new Expression("s1", "r") , new Expression("s2", "c")),
//      new AdditionExpression(),
//      new Formula(1, "s2"))
//  }


  def domainTypeLookup[Ctxt](dtpe: DataType)(implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]]): Generator[Ctxt, Type] = {
    FindClass(Seq(names.mangle(names.conceptNameOf(dtpe)))).interpret(canFindClass)
  }

  //  def registerTypeMapping(tpe: DataType): Generator[ProjectContext, Unit] = {
  //    import ooParadigm.projectCapabilities.{addTypeLookupForClasses, addTypeLookupForConstructors}
  //    import paradigm.projectCapabilities.addTypeLookupForMethods
  //
  //    val dtpe = TypeRep.DataType(tpe)
  //
  //    for {
  //      _ <- addTypeLookupForMethods(dtpe, domainTypeLookup(tpe))
  //      _ <- addTypeLookupForClasses(dtpe, domainTypeLookup(tpe))
  //      _ <- addTypeLookupForConstructors(dtpe, domainTypeLookup(tpe))
  //    } yield ()
  //  }

  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      rt <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(rt)

      res <- instantiateObject(rt, args)
    } yield res
  }

  def make_compute_method_signature(): Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._

    for {
      stringType <- toTargetLanguageType(TypeRep.String)
      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setParameters(Seq((names.mangle("s1"), stringType), (names.mangle("s2"), stringType)))
      _ <- setReturnType(intType)
    } yield ()
  }

  //  public class LongestCommonSubsequence {
  //    public int solution(String s1, String s2) {
  //      /**
  //       * Initialization
  //       */
  //      int len1 = s1.length();
  //      int len2 = s2.length();
  //
  //      int[][] dp = new int[len1 + 1][len2 + 1];
  //
  //      /**
  //       * Iterative solution
  //       */
  //      for(int r = 0; r < len1; r++) {
  //        for(int c = 0; c < len2; c++) {
  //          if(s1.charAt(r) == s2.charAt(c)) {
  //            dp[r + 1][c + 1] = dp[r][c] + 1;
  //          } else {
  //            dp[r + 1][c + 1] = Math.max(dp[r][c + 1], dp[r + 1][c]);
  //          }
  //        }
  //      }
  //
  //      /**
  //       * Return bottom right element
  //       */
  //      return dp[len1][len2];
  //    }
  //  }
  def make_compute_method(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for {
      stringType <- toTargetLanguageType(TypeRep.String)
      intType <- toTargetLanguageType(TypeRep.Int)
      array2dType <- toTargetLanguageType(TypeRep.Array(TypeRep.Array(TypeRep.Int)))
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)

      _ <- make_compute_method_signature()

      args <- getArguments()

      (names1, tpes1, s1) = args.head
      (names2, tpes2, s2) = args.tail.head

      /**
       * initialization
       */
      s1Length <- ooParadigm.methodBodyCapabilities.getMember(s1, names.mangle("length"))
      len1 <- declare_and_inst_variable("len1", intType, apply(s1Length, Seq.empty))
      len1PlusOne <- arithmetic.arithmeticCapabilities.add(len1, one)

      s2Length <- ooParadigm.methodBodyCapabilities.getMember(s2, names.mangle("length"))
      len2 <- declare_and_inst_variable("len2", intType, apply(s2Length, Seq.empty))
      len2PlusOne <- arithmetic.arithmeticCapabilities.add(len2, one)

      instantiated <- ooParadigm.methodBodyCapabilities.instantiateObject(
        array2dType,
        Seq(len1PlusOne, len2PlusOne),
        None
      )

      dp <- declare_and_inst_variable("dp", array2dType, instantiated)

      /**
       * optimization step definition
       */
      r <- declare_and_inst_variable("r", intType, zero)
      c <- declare_and_inst_variable("c", intType, zero)

      s1_charAt_r <- char_at(s1, r)
      s2_charAt_c <- char_at(s2, c)

      optimization_condition <- eqls.equalityCapabilities.areEqual(stringType, s1_charAt_r, s2_charAt_c)
      optimization_body <- impParadigm.imperativeCapabilities.ifThenElse(
        optimization_condition,
        for {
          rPlus1 <- arithmetic.arithmeticCapabilities.add(r, one)
          cPlus1 <- arithmetic.arithmeticCapabilities.add(c, one)

          dpOfr1c1 <- get_matrix_element(dp, rPlus1, cPlus1)

          dpOfrc <- get_matrix_element(dp, r, c)
          dpOfrc_PlusOne <- arithmetic.arithmeticCapabilities.add(dpOfrc, one)

          assignStmt <- impParadigm.imperativeCapabilities.assignVar(dpOfr1c1, dpOfrc_PlusOne)
          _ <- addBlockDefinitions(Seq(assignStmt))
        } yield (),
        Seq.empty,
        Some(
          for {
            _ <- Command.skip[paradigm.MethodBodyContext]
          } yield ()
        )
      )

      /**
       * solution generation
       */
      solution <- make_solution(len1, len2, dp, r, c, optimization_body)

    } yield solution
  }

  def makeSimpleDP(): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- addMethod(names.mangle("compute"), make_compute_method())
      } yield None
    }

    addClassToProject(makeClass, names.mangle("LongestCommonSubsequence"))
  }

  //  todo: make test cases
  //  def makeTestCase(): Generator[MethodBodyContext, Seq[Expression]] = {
  //    import paradigm.methodBodyCapabilities._
  //    import eqls.equalityCapabilities._
  //
  //    for {
  //      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
  //    } yield ()
  //  }

  def implement(): Generator[ProjectContext, Unit] = {

    for {
      _ <- makeSimpleDP()
      //      _ <- paradigm.projectCapabilities.addCompilationUnit(
      //        paradigm.compilationUnitCapabilities.addTestSuite(testName, makeTestCase("DP"))
      //      )
    } yield ()
  }
}

object LongestCommonSubsequenceObjectOrientedProvider {
  type WithParadigm[P <: AnyParadigm] = LongestCommonSubsequenceObjectOrientedProvider {val paradigm: P}
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   imp: Imperative.WithBase[base.MethodBodyContext, base.type],
   ffiArithmetic: Arithmetic.WithBase[base.MethodBodyContext, base.type, Double],
   oo: ObjectOriented.WithBase[base.type],
   con: Console.WithBase[base.MethodBodyContext, base.type],
   arr: Arrays.WithBase[base.MethodBodyContext, base.type],
   assertsIn: Assertions.WithBase[base.MethodBodyContext, base.type],
   eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type]
  )
  : LongestCommonSubsequenceObjectOrientedProvider.WithParadigm[base.type] =
    new LongestCommonSubsequenceObjectOrientedProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
      val arithmetic: ffiArithmetic.type = ffiArithmetic
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val ooParadigm: ObjectOriented.WithBase[paradigm.type] = oo
      override val console: Console.WithBase[base.MethodBodyContext, paradigm.type] = con
      override val array: Arrays.WithBase[base.MethodBodyContext, paradigm.type] = arr
      override val asserts: Assertions.WithBase[base.MethodBodyContext, paradigm.type] = assertsIn
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
    }
}