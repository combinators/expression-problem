package org.combinators.archive.bottomUp.grid.pascalBU

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Booleans, Console, Equality, RealArithmetic, Strings}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, ObjectOriented}
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider, Understands}
import org.combinators.dp.{TestExample, Utility}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.model.{LiteralInt, LiteralPair, UnitExpression}


/** Any OO approach will need to properly register type mappings and provide a default mechanism for finding a class
 * in a variety of contexts. This trait provides that capability
 */
trait PascalObjectOrientedProvider extends PascalProvider with Utility{
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val names: NameProvider[paradigm.syntax.Name]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val arithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val console: Console.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val strings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val booleans: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type]

  import ooParadigm._
  import paradigm._
  import syntax._

  lazy val message:String = "message"
  lazy val main:String = "main"
  lazy val testName = names.mangle("TestSuite")
  lazy val compute = names.mangle("compute")

  def getter(attr:String) : String = {
    "get" + attr.capitalize
  }

  /**
   * Default registration for findClass, which works with each registerTypeMapping for the different approaches.
   *
   * Sometimes the mapping is fixed for an EP approach, but sometimes it matters when a particular class is requested
   * in the evolution of the system over time.
   */
  def domainTypeLookup[Ctxt](dtpe: DataType)(implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]]): Generator[Ctxt, Type] = {
    FindClass(Seq(names.mangle(names.conceptNameOf(dtpe)))).interpret(canFindClass)
  }

  /** Provides meaningful default solution to find the base data type in many object-oriented approaches.
   *
   * This enables target-language classes to be retrieved from within the code generator in the Method, Class or Constructor contexts.
   */
//  def registerTypeMapping(tpe:DataType): Generator[ProjectContext, Unit] = {
//    import ooParadigm.projectCapabilities.{addTypeLookupForClasses, addTypeLookupForConstructors}
//    import paradigm.projectCapabilities.addTypeLookupForMethods    // must be present, regardless of IntelliJ
//    val dtpe = TypeRep.DataType(tpe)
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
      intType <- toTargetLanguageType(TypeRep.Int)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      _ <- setParameters(Seq((names.mangle("r"), intType),(names.mangle("c"), intType)))
      _ <- setReturnType(intType)

    } yield ()
  }

  def make_compute_method(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      _ <- make_compute_method_signature()
      args <- getArguments()



      intType <- toTargetLanguageType(TypeRep.Int)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      array2dType <- toTargetLanguageType(TypeRep.Array(TypeRep.Array(TypeRep.Int)))


      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)

      (namer,tper,r) = args.head
      (namec,tpec,c) = args.tail.head
      rp1 <- arithmetic.arithmeticCapabilities.add(r, one)
      cp1 <- arithmetic.arithmeticCapabilities.add(c, one)
      instantiated <- ooParadigm.methodBodyCapabilities.instantiateObject(
        array2dType,
        Seq(rp1, cp1),
        None
      )
      dpName <- freshName(names.mangle("dp"))
      dpVar <- impParadigm.imperativeCapabilities.declareVar(dpName, array2dType, Some(instantiated))

      iName <- freshName(names.mangle("i"))
      iVar <- impParadigm.imperativeCapabilities.declareVar(iName, intType, Some(zero))
      outerCond <- arithmetic.arithmeticCapabilities.lt(iVar, rp1);
      outerLoop <-impParadigm.imperativeCapabilities.whileLoop(outerCond,
        for {
          jName <- freshName(names.mangle("j"))
          jVar <- impParadigm.imperativeCapabilities.declareVar(jName, intType, Some(zero))
          innerCond <- arithmetic.arithmeticCapabilities.lt(jVar, cp1);
          innerLoop <-impParadigm.imperativeCapabilities.whileLoop(innerCond,
            for {
              condExpr1 <- arithmetic.arithmeticCapabilities.le(jVar, zero)
              condExpr2 <- arithmetic.arithmeticCapabilities.le(jVar, zero)
              dpi <- array.arrayCapabilities.get(dpVar, iVar)
              dpj <- array.arrayCapabilities.get(dpi, jVar)

              im1 <- arithmetic.arithmeticCapabilities.sub(iVar,one)
              jm1 <- arithmetic.arithmeticCapabilities.sub(jVar,one)
              dpim1 <-array.arrayCapabilities.get(dpVar, im1)
              dpim1jm1 <-array.arrayCapabilities.get(dpim1, jm1)
              dpim1j <-array.arrayCapabilities.get(dpim1, iVar)
              sum <- arithmetic.arithmeticCapabilities.add(dpim1j,dpim1jm1)


              ifStmt <- impParadigm.imperativeCapabilities.ifThenElse(condExpr1,
                for {
                  assign <- impParadigm.imperativeCapabilities.assignVar(dpj, one)

                  _ <- addBlockDefinitions(Seq(assign))
                } yield (),
                Seq.empty,
                Some(for{
                  ifStmt <- impParadigm.imperativeCapabilities.ifThenElse(condExpr2,
                        for {
                          assign <- impParadigm.imperativeCapabilities.assignVar(dpj, zero)
                          _ <- addBlockDefinitions(Seq(assign))
                        } yield (),
                    Seq.empty,
                        Some(for{
                          assign <- impParadigm.imperativeCapabilities.assignVar(dpj, sum)
                          _ <- addBlockDefinitions(Seq(assign))
                        }yield())
                  )
                  _ <- addBlockDefinitions(Seq(ifStmt))

                }yield())
              )
              _ <- addBlockDefinitions(Seq(ifStmt))
            }yield())
          _ <- addBlockDefinitions(Seq(innerLoop))
      }yield())

      dpr <- array.arrayCapabilities.get(dpVar, r)
      dprc <- array.arrayCapabilities.get(dpr, c)

      _ <- addBlockDefinitions(Seq(outerLoop))

    } yield Some(dprc)
  }
  /**

  public Integer compute(Integer r, Integer c) {
      Integer[][] dp = new Integer[(r + 1)][(c + 1)];
      for(int i = 0;i<dp.length;i++){
          for(int j=0;j<dp[i].length;j++){
              if ((j <= 0)) {
                  dp[i][j] = 1;
              } else if ((i <= 0)) {
                  dp[i][j] = 0;
              } else {
                  dp[i][j] = (this.compute((r - 1), c) + this.compute((r - 1), (c - 1)));
              }
          }
      }
      return dp[r][c];
  }

   */
  def makeSimpleDP(): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- addMethod(names.mangle("compute"), make_compute_method())
      } yield None
    }

    addClassToProject(makeClass, names.mangle("Pascal"))
  }


  def makeTestCase(): Generator[MethodBodyContext, Seq[Expression]] = {
    import eqls.equalityCapabilities._
    import paradigm.methodBodyCapabilities._

    val tests = Seq(
      new TestExample("pasc11", new LiteralPair(1,1), new LiteralInt(1), new UnitExpression),
      new TestExample("pasc32", new LiteralPair(3,2), new LiteralInt(3), new UnitExpression),
      new TestExample("pasc63", new LiteralPair(6,3), new LiteralInt(20), new UnitExpression),
      new TestExample("pasc2013", new LiteralPair(20,13), new LiteralInt(77520), new UnitExpression),
    )

    for {
      assert_statements <- forEach(tests) { example =>

        val pair = example.inputType match {
          case lp:LiteralPair => (lp.val1, lp.val2)
          case _ => ???
        }

        val expected_value = example.answer match {
          case lit:LiteralInt => lit.literal
          case _ => ???
        }

        for {
          pascType <- ooParadigm.methodBodyCapabilities.findClass(names.mangle("Pascal"))
          r_value <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, pair._1)
          c_value <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, pair._2)
          sol <- ooParadigm.methodBodyCapabilities.instantiateObject(pascType, Seq(r_value,c_value))
          computeMethod <- ooParadigm.methodBodyCapabilities.getMember(sol, compute)

          intType <- toTargetLanguageType(TypeRep.Int)
          pascrc_value <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, expected_value)
          pascrc_actual <- apply(computeMethod, Seq.empty)
          asserteq_fib <- asserts.assertionCapabilities.assertEquals(intType, pascrc_actual, pascrc_value)

        } yield asserteq_fib
      }
    } yield assert_statements
  }

  def makeTestCase(clazzName:String): Generator[TestContext, Unit] = {
    for {
      _ <- paradigm.testCapabilities.addTestCase(makeTestCase(), names.mangle(clazzName))
    } yield ()
  }

  def implement(): Generator[ProjectContext, Unit] = {

    for {
      _ <- makeSimpleDP()
      _ <- paradigm.projectCapabilities.addCompilationUnit(
        paradigm.compilationUnitCapabilities.addTestSuite(testName, makeTestCase("DP"))
      )
    } yield ()
  }
}

object PascalObjectOrientedProvider {
  type WithParadigm[P <: AnyParadigm] = PascalObjectOrientedProvider { val paradigm: P }
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
   eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type],
   booleansIn: Booleans.WithBase[base.MethodBodyContext, base.type]
  )
  : PascalObjectOrientedProvider.WithParadigm[base.type] =
    new PascalObjectOrientedProvider {
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
      override val booleans: Booleans.WithBase[base.MethodBodyContext, paradigm.type] = booleansIn
    }
}
