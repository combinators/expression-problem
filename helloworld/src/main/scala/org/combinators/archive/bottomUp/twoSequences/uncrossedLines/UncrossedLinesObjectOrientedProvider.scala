package org.combinators.archive.bottomUp.twoSequences.uncrossedLines

import org.combinators.archive.bottomUp.twoSequences.TwoSequencesUtility
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality, RealArithmetic, Strings}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, ObjectOriented}
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider, Understands}

trait UncrossedLinesObjectOrientedProvider extends UncrossedLinesProvider with TwoSequencesUtility {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val names: NameProvider[paradigm.syntax.Name]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val arithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val console: Console.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val strings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]
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

  /**
   * Defines the signature of the <a href=https://leetcode.com/problems/uncrossed-lines/>Uncrossed Lines<a> solution method:
   * Uncrossed Lines takes in two integer arrays `nums1` and `nums2` and returns an integer
   * representing the number of uncrossed lines.
   *
   * @return a generator of unit, which represents the instructions to add the method signature to the class.
   *         Rather than returning something, this function
   *         is used to add the method signature to the class in place.
   */
  def make_compute_method_signature(): Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._

    for {
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      intType <- toTargetLanguageType(TypeRep.Int)

      _ <- setParameters(Seq((names.mangle("nums1"), arrayType), (names.mangle("nums2"), arrayType)))
      _ <- setReturnType(intType)
    } yield ()
  }

  // todo: figure out how to use this
  def initialize(): Generator[paradigm.MethodBodyContext, Seq[Expression]] = {

    for {
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)

      dp <- instantiate_dp(one, one)
    } yield Seq(dp, one)
  }

  def make_compute_method(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for {
      _ <- make_compute_method_signature()
      args <- getArguments()

      (names1, tpes1, nums1) = args.head
      (names2, tpes2, nums2) = args.tail.head

      intType <- toTargetLanguageType(TypeRep.Int)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      array2dType <- toTargetLanguageType(TypeRep.Array(TypeRep.Array(TypeRep.Int)))
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)

      /**
       * initialization
       */
      len1Value <- ooParadigm.methodBodyCapabilities.getMember(nums1, names.mangle("length"))
      len2Value <- ooParadigm.methodBodyCapabilities.getMember(nums2, names.mangle("length"))
      len1 <- declare_and_inst_variable("len1", intType, len1Value)
      len2 <- declare_and_inst_variable("len2", intType, len2Value)

      len1PlusOne <- arithmetic.arithmeticCapabilities.add(len1, one)
      len2PlusOne <- arithmetic.arithmeticCapabilities.add(len2, one)

      dp <- instantiate_dp(len1PlusOne, len2PlusOne)

      r <- declare_and_inst_variable("r", intType, zero)
      c <- declare_and_inst_variable("c", intType, zero)

      /**
       * optimization
       */
      outer_guard <- arithmetic.arithmeticCapabilities.lt(r, len1)
      outer_update <- arithmetic.arithmeticCapabilities.add(r, one)

      inner_guard <- arithmetic.arithmeticCapabilities.lt(c, len2)
      inner_update <- arithmetic.arithmeticCapabilities.add(c, one)

      nums1OfR <- array.arrayCapabilities.get(nums1, r)
      nums2OfC <- array.arrayCapabilities.get(nums2, c)

      // todo: replace lt with equals or a substitute for equals
      optimization_condition <- arithmetic.arithmeticCapabilities.lt(nums1OfR, nums2OfC)
      body <- impParadigm.imperativeCapabilities.ifThenElse(optimization_condition,
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

      empty <- for {
        _ <- Command.skip[paradigm.MethodBodyContext]
      } yield Seq.empty

      optimization <- make_nested_for_loop(r, outer_guard, outer_update, c, inner_guard, inner_update, Seq(body), empty)
      _ <- addBlockDefinitions(Seq(optimization))

      /**
       * return the bottom right element
       */
      dpBottomRight <- get_bottom_right_dp_element(dp, len1, len2)
    } yield Some(dpBottomRight)
  }

  def make_class(): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._

      for {
        _ <- addMethod(names.mangle("compute"), make_compute_method())
      } yield None
    }

    addClassToProject(makeClass, names.mangle("UncrossedLines"))
  }

  def implement(): Generator[ProjectContext, Unit] = {
    for {
      _ <- make_class()
    } yield ()
  }
}

object UncrossedLinesObjectOrientedProvider {
  type WithParadigm[P <: AnyParadigm] = UncrossedLinesObjectOrientedProvider {val paradigm: P}
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   imp: Imperative.WithBase[base.MethodBodyContext, base.type],
   ffiArithmetic: Arithmetic.WithBase[base.MethodBodyContext, base.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[base.MethodBodyContext, base.type, Double],
   oo: ObjectOriented.WithBase[base.type],
   con: Console.WithBase[base.MethodBodyContext, base.type],
   arr: Arrays.WithBase[base.MethodBodyContext, base.type],
   assertsIn: Assertions.WithBase[base.MethodBodyContext, base.type],
   stringsIn: Strings.WithBase[base.MethodBodyContext, base.type],
   eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type]
  ): UncrossedLinesObjectOrientedProvider.WithParadigm[base.type] =
    new UncrossedLinesObjectOrientedProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
      val arithmetic: ffiArithmetic.type = ffiArithmetic
      val realArithmetic: ffiRealArithmetic.type = ffiRealArithmetic
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val ooParadigm: ObjectOriented.WithBase[paradigm.type] = oo
      override val console: Console.WithBase[base.MethodBodyContext, paradigm.type] = con
      override val array: Arrays.WithBase[base.MethodBodyContext, paradigm.type] = arr
      override val asserts: Assertions.WithBase[base.MethodBodyContext, paradigm.type] = assertsIn
      override val strings: Strings.WithBase[base.MethodBodyContext, paradigm.type] = stringsIn
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
    }
}