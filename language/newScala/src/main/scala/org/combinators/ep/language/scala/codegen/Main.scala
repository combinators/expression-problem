package org.combinators.ep.language.scala.codegen    /*DD:LD:AD*/

import cats.effect.{ExitCode, IO, IOApp}
import org.apache.commons.io.FileUtils
import org.combinators.ep.approach.oo._
import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math._
import org.combinators.ep.domain.math.eips.systemX
import org.combinators.ep.domain.math.systemD.{D1, D1D2, D2, D3}
import org.combinators.ep.domain.math.systemI.{I1, I2}
import org.combinators.ep.domain.math.systemJ.{J1, J2, J3, J4, J5, J6}
import org.combinators.ep.domain.math.systemJK.{J7, J8, K2J6}
import org.combinators.ep.domain.math.systemK.{K1, K2}
import org.combinators.ep.domain.math.systemO.{O1, O1OA, O2, OA, OD1, OD2, OD3, OO1, OO2, OO3}
import org.combinators.ep.domain.math.systemX.{X1, X2, X2X3, X3, X4}
import org.combinators.ep.generator.ApproachImplementationProvider.WithParadigm
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider, FileWithPath, FileWithPathPersistable, TestImplementationProvider}
import org.combinators.jgitserv.{BranchTransaction, GitService}

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class Main(choice:String, select:String) {
  val generator: CodeGenerator = CodeGenerator(M0.getModel.base.name.toLowerCase)

  val functionalApproach: WithParadigm[generator.paradigm.type] = org.combinators.ep.approach.functional.Traditional[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.functional, generator.functionalControl)

  val ooApproach: WithParadigm[generator.paradigm.type] = Traditional[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.ooParadigm)
  // can't have all of these together
  val visitorApproach: Visitor.WithParadigm[generator.paradigm.type] = Visitor[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val visitorSideEffectApproach: Visitor.WithParadigm[generator.paradigm.type] = Visitor.withSideEffects[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.imperative, generator.ooParadigm)
  val extensibleVisitorApproach: ExtensibleVisitor.WithParadigm[generator.paradigm.type] = ExtensibleVisitor[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val interpreterApproach: Interpreter.WithParadigm[generator.paradigm.type] = Interpreter[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

  val cocoCleanApproach: CoCoClean.WithParadigm[generator.paradigm.type] = CoCoClean[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val triviallyCleanApproach: TriviallyClean.WithParadigm[generator.paradigm.type] = TriviallyClean[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.ooParadigm)

  //val dispatchApproach = RuntimeDispatch[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.imperative, generator.strings, generator.exceptions, generator.ooParadigm)
  val algebraApproach: ObjectAlgebras.WithParadigm[generator.paradigm.type] = ObjectAlgebras[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

  val visualizeApproach: Visualize.WithParadigm[generator.paradigm.type] = Visualize[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.ooParadigm)

  // select one here
  val approach = choice match {
    case "functional" => functionalApproach
    case "graphviz" => visualizeApproach
    case "oo" => ooApproach
    case "visitor" => visitorApproach
    case "visitorSideEffect" => visitorSideEffectApproach
    case "extensibleVisitor" => extensibleVisitorApproach
    case "interpreter" => interpreterApproach
    case "coco" => cocoCleanApproach
    case "trivially" => triviallyCleanApproach
    case "dispatch" => ???  // not yet implemented
    case "algebra" => algebraApproach

    case _ => ???
  }

  val evolutions: Seq[Evolution] = select match {
    case "M0" => Seq(M0)
    case "M1" => Seq(M0, M1)
    case "M2" => Seq(M0, M1, M2)
    case "M3" => Seq(M0, M1, M2, M3)
    case "M4" => Seq(M0, M1, M2, M3, M4)
    case "M5" => Seq(M0, M1, M2, M3, M4, M5)
    case "M6" => Seq(M0, M1, M2, M3, M4, M5, M6)
    case "M7" => Seq(M0, M1, M2, M3, M4, M5, M7)
    case "M7I2" => Seq(M0, M1, M2, M3, M4, M5, M7, I1, I2, M7I2)
    case "M8" => Seq(M0, M1, M2, M3, M4, M5, M7, I1, I2, M7I2, M8)
    case "M9" => Seq(M0, M1, M2, M3, M4, M5, M7, I1, I2, M7I2, M8, M9)

    case "I1" => Seq(M0, M1, M2, I1)
    case "A1" => Seq(M0, M1, M2, I1, A1)
    case "A1M3" => Seq(M0, M1, M2, M3, I1, A1, A1M3)
    case "A1M3I2" => Seq(M0, M1, M2, M3, I1, A1, A1M3, I2, A1M3I2)
    case "A3" => Seq(M0, M1, M2, M3, I1, A1, A1M3, I2, A1M3I2, A3)

    case "I2" => Seq(M0, M1, M2, I1, I2)

    case "O1" => Seq(M0, M1, M2, O1)
    case "OA" => Seq(M0, M1, M2, OA)
    case "O1OA" => Seq(M0, M1, M2, O1, OA, O1OA)
    case "O2" => Seq(M0, M1, M2, O1, O2)

    case "OD1" => Seq(M0, M1, M2, OD1)
    case "OD2" => Seq(M0, M1, M2, OD2)
    case "OD3" => Seq(M0, M1, M2, OD1, OD2, OD3)

    case "OO1" => Seq(M0, M1, M2, OO1)
    case "OO2" => Seq(M0, M1, M2, OO2)
    case "OO3" => Seq(M0, M1, M2, OO1, OO2, OO3)

    case "J1" => Seq(M0, J1)
    case "J2" => Seq(M0, J1, J2)
    case "J3" => Seq(M0, J1, J2, J3)
    case "K1" => Seq(M0, J1, J2, K1)
    case "K2" => Seq(M0, J1, J2, K1, K2)
    case "J4" => Seq(M0, J1, J2, J3, J4)
    case "J5" => Seq(M0, J1, J2, J3, J4, J5)
    case "J6" => Seq(M0, J1, J2, J3, J4, J5, J6)
    case "K2J6" => Seq(M0, J1, J2, K1, K2, J3, J4, J5, J6, K2J6)
    case "J7" => Seq(M0, J1, J2, K1, K2, J3, J4, J5, J6, K2J6, J7)
    case "J8" => Seq(M0, J1, J2, K1, K2, J3, J4, J5, J6, K2J6, J7, J8)

    case "W1" => Seq(M0, M1, W1)
    case "M3W1" => Seq(M0, M1, M2, M3, W1, M3W1)
    case "Q1" => Seq(M0, M1, M2, M3, W1, M3W1, Q1)
    case "C2" => Seq(M0, M1, M2, M3, W1, M3W1, Q1, C2)
    case "V1" => Seq(M0, M1, M2, M3, W1, M3W1, Q1, C2, V1)

    case "X1" => Seq(M0, X1)
    case "X2" => Seq(M0, X1, X2)
    case "X3" => Seq(M0, X1, X3)
    case "X2X3" => Seq(M0, X1, X2, X3, X2X3)
    case "X4" => Seq(M0, X1, X2, X3, X2X3, X4)

    case "N1" => Seq(M0, M1, M3, N1)
    case "M2_ABS" => Seq(M0, M1, M2, M2_ABS)
    case "M3I1" => Seq(M0, M1, M2, M3, I1, M3I1)
    case "I2M3I1N1" => Seq(M0, M1, M2, M3, I1, M3I1, I2, N1, I2M3I1N1)

    case "D1" => Seq(M0, M1, D1)
    case "D2" => Seq(M0, M1, D2)
    case "D1D2" => Seq(M0, M1, D1, D2, D1D2)
    case "D3" => Seq(M0, M1, D1, D2, D1D2, D3)

    case _ => ???
  }


  val m0_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M0(approach.paradigm)(generator.doubles,generator.strings)
  val m1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M1(approach.paradigm)(m0_eip)(generator.doubles)
  val m2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M2(approach.paradigm)(m1_eip)(generator.doubles, generator.strings)

  val m3_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M3(approach.paradigm)(m2_eip)(generator.doubles, generator.strings)

  val m4_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      eips.M4.functional[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(m3_eip)(
        generator.functionalControl, generator.doubles, generator.booleans, generator.strings, generator.listsInMethod, generator.equality)
    } else {
      eips.M4.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(m3_eip)(
        generator.imperative, generator.doubles, generator.booleans, generator.strings, generator.listsInMethod, generator.equality)
    }

  val m5_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M5(approach.paradigm)(m4_eip)(generator.ints,generator.treesInMethod)
  val m6_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M6(approach.paradigm)(m5_eip)(generator.equality, generator.booleans)
  val m7_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M7funct(approach.paradigm)(m6_eip)(generator.doubles, generator.realDoubles, generator.strings, generator.functional)
//    if (choice == "functional") {
//      m0_eip // not compatible for functional
//    } else {
      eips.M7(approach.paradigm)(m6_eip)(generator.doubles, generator.realDoubles, generator.strings, generator.imperative)
//    }
  val i1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      m0_eip // not compatible for functional
    } else {
      eips.systemI.I1(approach.paradigm)(m2_eip)(generator.doubles, generator.realDoubles, generator.strings, generator.imperative)
    }
  val i2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      m0_eip // not compatible for functional
    } else {
      eips.systemI.I2(approach.paradigm)(i1_eip)(generator.doubles, generator.realDoubles, generator.strings, generator.imperative)
    }

  val m7i2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      eips.M7I2.functional[approach.paradigm.type, ApproachImplementationProvider.WithParadigm](approach.paradigm)(m7_eip, i2_eip)(
        generator.functionalControl, generator.doubles, generator.booleans, generator.equality)
    } else {
      eips.M7I2.imperative[approach.paradigm.type, ApproachImplementationProvider.WithParadigm](approach.paradigm)(m7_eip, i2_eip)(
        generator.imperative, generator.doubles, generator.booleans, generator.equality)
    }
  val m8_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      eips.M8.functional[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(m7i2_eip)(
        generator.functionalControl, generator.doubles, generator.booleans, generator.strings, generator.equality)
    } else {
      eips.M8.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(m7i2_eip)(
        generator.imperative, generator.doubles, generator.booleans, generator.strings, generator.equality)
    }

  val m9_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      m0_eip // not compatible for functional
    } else {
      eips.M9(approach.paradigm)(m8_eip)(generator.doubles, generator.realDoubles, generator.imperative)
    }

  val a1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.A1(approach.paradigm)(i1_eip)(generator.doubles, generator.strings)
  val a1m3_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.A1M3(approach.paradigm)(m3_eip, a1_eip)(generator.strings)

  val a1m3i2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.A1M3I2(approach.paradigm)(a1m3_eip, i2_eip)(generator.strings)
  val a3_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.A3(approach.paradigm)(a1m3i2_eip)(generator.doubles, generator.strings)

  val o1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.O1(approach.paradigm)(m2_eip)(generator.doubles, generator.strings)
  val oa_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.OA(approach.paradigm)(m2_eip)(generator.doubles)
  val o1oa_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.O1OA(approach.paradigm)(o1_eip, oa_eip)
  val o2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.O2(approach.paradigm)(o1_eip)(generator.doubles, generator.strings)

  val od1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.OD1(approach.paradigm)(m2_eip)(generator.doubles, generator.strings)
  val od2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.OD2(approach.paradigm)(m2_eip)(generator.doubles, generator.strings)
  val od3_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.OD3(approach.paradigm)(od1_eip, od2_eip)

  val oo1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.OO1(approach.paradigm)(m2_eip)(generator.doubles, generator.realDoubles, generator.strings)
  val oo2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.OO2(approach.paradigm)(m2_eip)(generator.doubles, generator.realDoubles, generator.strings)
  val oo3_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.OO3(approach.paradigm)(oo1_eip, oo2_eip)

  val j1_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      m0_eip // not compatible for functional
    } else {
      eips.systemJ.J1(approach.paradigm)(m0_eip)(generator.doubles, generator.realDoubles, generator.strings, generator.imperative)
    }
  val j2_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    eips.systemJ.J2(approach.paradigm)(j1_eip)(generator.doubles, generator.booleans, generator.equality)
  val j3_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    eips.systemJ.J3(approach.paradigm)(j2_eip)(generator.doubles, generator.booleans, generator.strings)
  val k1_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      m0_eip // not compatible for functional
    } else {
      eips.systemK.K1(approach.paradigm)(j2_eip)(generator.doubles, generator.realDoubles, generator.booleans, generator.strings, generator.imperative)
    }
  val j4_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    eips.systemJ.J4(approach.paradigm)(j3_eip)(generator.ints, generator.treesInMethod)
  val j5_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    eips.systemJ.J5(approach.paradigm)(j4_eip)(generator.equality, generator.booleans)
  val j6_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      m0_eip // not compatible for functional
    } else {
      eips.systemJ.J6(approach.paradigm)(j5_eip)(generator.doubles, generator.realDoubles, generator.strings, generator.imperative)
    }

  val k2_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      eips.systemK.K2.functional[approach.paradigm.type, ApproachImplementationProvider.WithParadigm](approach.paradigm)(k1_eip)(
        generator.functionalControl, generator.doubles, generator.booleans, generator.strings, generator.listsInMethod, generator.equality)
    } else {
      eips.systemK.K2.imperative[approach.paradigm.type, ApproachImplementationProvider.WithParadigm](approach.paradigm)(k1_eip)(
        generator.imperative, generator.doubles, generator.booleans, generator.strings, generator.listsInMethod, generator.equality)
    }

  val k2j6_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      eips.systemJK.K2J6.functional[approach.paradigm.type, ApproachImplementationProvider.WithParadigm](approach.paradigm)(j6_eip, k2_eip)(
        generator.functionalControl, generator.doubles, generator.booleans, generator.strings, generator.equality)
    } else {
      eips.systemJK.K2J6.imperative[approach.paradigm.type, ApproachImplementationProvider.WithParadigm](approach.paradigm)(j6_eip, k2_eip)(
        generator.imperative, generator.doubles, generator.booleans, generator.strings, generator.equality)
    }
  val j7_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      eips.systemJK.J7.functional[approach.paradigm.type, ApproachImplementationProvider.WithParadigm](approach.paradigm)(k2j6_eip)(
        generator.functionalControl, generator.doubles, generator.booleans, generator.strings, generator.equality)
    } else {
      eips.systemJK.J7.imperative[approach.paradigm.type, ApproachImplementationProvider.WithParadigm](approach.paradigm)(k2j6_eip)(
        generator.imperative, generator.doubles, generator.booleans, generator.strings, generator.equality)
    }

  val j8_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
  if (choice == "functional") {
    m0_eip // not compatible for functional
  } else {
    eips.systemJK.J8(approach.paradigm)(j7_eip)(generator.doubles, generator.realDoubles, generator.imperative)
  }

  val w1_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    eips.W1(approach.paradigm)(m1_eip)(generator.doubles, generator.realDoubles, generator.strings)

  val m3w1_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      eips.M3W1.functional[approach.paradigm.type, ApproachImplementationProvider.WithParadigm](approach.paradigm)(m3_eip, w1_eip)(
        generator.functionalControl, generator.doubles, generator.booleans, generator.equality, generator.strings)
    } else {
      eips.M3W1.imperative[approach.paradigm.type, ApproachImplementationProvider.WithParadigm](approach.paradigm)(m3_eip, w1_eip)(
        generator.imperative, generator.doubles, generator.booleans, generator.equality, generator.strings)
    }
  val q1_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    eips.Q1(approach.paradigm)(m3w1_eip)(generator.ints, generator.realDoubles, generator.treesInMethod, generator.strings)

  val c2_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      eips.C2.functional[approach.paradigm.type, ApproachImplementationProvider.WithParadigm](approach.paradigm)(q1_eip)(
        generator.functionalControl, generator.doubles, generator.booleans, generator.strings, generator.listsInMethod, generator.equality)
    } else {
      eips.C2.imperative[approach.paradigm.type, ApproachImplementationProvider.WithParadigm](approach.paradigm)(q1_eip)(
        generator.imperative, generator.doubles, generator.booleans, generator.strings, generator.listsInMethod, generator.equality)
    }
  val v1_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      eips.V1.functional[approach.paradigm.type, ApproachImplementationProvider.WithParadigm](approach.paradigm)(c2_eip)(
        generator.functionalControl, generator.doubles, generator.booleans, generator.strings, generator.equality)
    } else {
      eips.V1.imperative[approach.paradigm.type, ApproachImplementationProvider.WithParadigm](approach.paradigm)(c2_eip)(
        generator.imperative, generator.doubles, generator.booleans, generator.strings, generator.equality)
    }

  val x1_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      m0_eip // not compatible for functional
    } else {
      systemX.X1(approach.paradigm)(m0_eip)(generator.doubles, generator.realDoubles, generator.strings, generator.imperative)
    }
  val x2_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    systemX.X2(approach.paradigm)(x1_eip)(generator.doubles, generator.strings)
  val x3_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    systemX.X3(approach.paradigm)(x1_eip)(generator.doubles, generator.strings)

  val x2x3_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    systemX.X2X3(approach.paradigm)(x2_eip, x3_eip)

  val x4_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    systemX.X4(approach.paradigm)(x2x3_eip)(generator.doubles, generator.strings)

  val m2_abs_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      m0_eip // not compatible for functional
    } else {
      eips.M2_ABS(approach.paradigm)(m2_eip)(generator.doubles, generator.imperative, generator.strings)
    }

  val m3i1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      eips.M3I1.functional[approach.paradigm.type, ApproachImplementationProvider.WithParadigm](approach.paradigm)(m3_eip, i1_eip)(
        generator.functionalControl, generator.booleans, generator.equality, generator.strings)
    } else {
      eips.M3I1.imperative[approach.paradigm.type, ApproachImplementationProvider.WithParadigm](approach.paradigm)(m3_eip, i1_eip)(
        generator.imperative, generator.booleans, generator.equality, generator.strings)
    }

  val n1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      m0_eip // not compatible for functional
    } else {
      eips.N1(approach.paradigm)(m3_eip)(generator.doubles, generator.realDoubles, generator.strings, generator.imperative)
    }

  val i2m3i1n1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      eips.I2M3I1N1.functional[approach.paradigm.type, ApproachImplementationProvider.WithParadigm](approach.paradigm)(i2_eip, m3i1_eip, n1_eip)(
        generator.booleans, generator.equality, generator.strings)
    } else {
      eips.I2M3I1N1.imperative[approach.paradigm.type, ApproachImplementationProvider.WithParadigm](approach.paradigm)(i2_eip, m3i1_eip, n1_eip)(
        generator.booleans, generator.equality, generator.strings)
    }

  val d1_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
  if (choice == "functional") {
    m0_eip // not compatible for functional
  } else {
    eips.systemD.D1(approach.paradigm)(m1_eip)(generator.doubles, generator.realDoubles, generator.strings, generator.imperative)
  }
  val d2_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    eips.systemD.D2(approach.paradigm)(m1_eip)(generator.doubles, generator.strings)

  val d1d2_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    if (choice == "functional") {
      eips.systemD.D1D2.functional[approach.paradigm.type, ApproachImplementationProvider.WithParadigm](approach.paradigm)(d1_eip, d2_eip)(
        generator.functionalControl, generator.doubles, generator.booleans, generator.equality)
    } else {
      eips.systemD.D1D2.imperative[approach.paradigm.type, ApproachImplementationProvider.WithParadigm](approach.paradigm)(d1_eip, d2_eip)(
        generator.imperative, generator.doubles, generator.booleans, generator.equality)
    }

  val d3_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    eips.systemD.D3(approach.paradigm)(d1d2_eip)(generator.doubles, generator.strings)

  val eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] = select match {

    case "M0" => m0_eip
    case "M1" => m1_eip
    case "M2" => m2_eip
    case "M3" => m3_eip
    case "M4" => m4_eip
    case "M5" => m5_eip
    case "M6" => m6_eip
    case "M7" => m7_eip
    case "M7I2" => m7i2_eip
    case "M8" => m8_eip
    case "M9" => m9_eip

    case "I1" => i1_eip
    case "A1" => a1_eip
    case "A1M3" => a1m3_eip
    case "A1M3I2" => a1m3i2_eip
    case "A3" => a3_eip

    case "I2" => i2_eip
    case "O1" => o1_eip
    case "OA" => oa_eip
    case "O1OA" => o1oa_eip
    case "O2" => o2_eip

    case "OD1" => od1_eip
    case "OD2" => od2_eip
    case "OD3" => od3_eip

    case "OO1" => oo1_eip
    case "OO2" => oo2_eip
    case "OO3" => oo3_eip

    case "J1" => j1_eip
    case "J2" => j2_eip
    case "J3" => j3_eip
    case "K1" => k1_eip
    case "K2" => k2_eip
    case "J4" => j4_eip
    case "J5" => j5_eip
    case "J6" => j6_eip
    case "K2J6" => k2j6_eip
    case "J7" => j7_eip
    case "J8" => j8_eip

    case "W1" => w1_eip
    case "M3W1" => m3w1_eip
    case "Q1" => q1_eip
    case "C2" => c2_eip
    case "V1" => v1_eip

    case "X1" => x1_eip
    case "X2" => x2_eip
    case "X3" => x3_eip
    case "X2X3" => x2x3_eip
    case "X4" => x4_eip

    case "N1" => n1_eip
    case "M2_ABS" => m2_abs_eip
    case "M3I1" => m3i1_eip
    case "I2M3I1N1" => i2m3i1n1_eip

    case "D1" => d1_eip
    case "D2" => d2_eip
    case "D1D2" => d1d2_eip
    case "D3" => d3_eip

    case _ => ???
  }

  def transaction[T](initialTransaction: T, addToTransaction: (T, String, () => Seq[FileWithPath]) => T): T = {
    evolutions.foldLeft(initialTransaction) { case (transaction, evolution) =>
      val impl =
        () => generator.paradigm.runGenerator {
          for {
            _ <- approach.implement(evolution.getModel, eip)
            _ <- approach.implement(
              evolution.allTests,
              TestImplementationProvider.defaultAssertionBasedTests(approach.paradigm)(generator.assertionsInMethod, generator.equality, generator.booleans, generator.strings)
            )
          } yield ()
        }
      addToTransaction(transaction, evolution.getModel.name, impl)
    }
  }

  val persistable = FileWithPathPersistable[FileWithPath]

  def gitTransaction: Option[BranchTransaction] =
    transaction[Option[BranchTransaction]](Option.empty, (transaction, evolutionName, files) => {
      val nextTransaction =
        transaction.map(_.fork(evolutionName).deleteAllFiles)
          .getOrElse(BranchTransaction.empty(evolutionName))
      Some(nextTransaction.persist(files())(persistable).commit("Adding next evolution"))
    })

  def directToDiskTransaction(targetDirectory: Path): IO[Unit] = {
    transaction[IO[Unit]](IO.unit, (transaction, evolutionName, files) => IO {
      print("Computing Files...")
      val computed = files()
      println("[OK]")
      if (targetDirectory.toFile.exists()) {
        print(s"Cleaning Target Directory (${targetDirectory})...")
        FileUtils.deleteDirectory(targetDirectory.toFile)
        println("[OK]")
      }
      print("Persisting Files...")
      computed.foreach(file => persistable.persistOverwriting(targetDirectory, file))
      println("[OK]")
    })
  }

  def runGit(args: List[String]): IO[ExitCode] = {
    val name = evolutions.head.getModel.base.name
    for {
      _ <- IO { System.out.println(s"Use: git clone http://127.0.0.1:8081/$name ${evolutions.last.getModel.name}") }
      exitCode <- new GitService(gitTransaction.toSeq, name).run(args)
      //exitCode <- new GitService(transaction.toSeq, name).runProcess(Seq(s"sbt", "test"))
    } yield exitCode
  }

  def runDirectToDisc(targetDirectory: Path): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory)
    } yield ExitCode.Success
  }
}

object GitMain extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    val approach = if (args.isEmpty) "interpreter" else args.head
    val selection = if (args.isEmpty || args.tail.isEmpty) "M7I2" else args.tail.head
    new Main(approach, selection).runGit(args)
  }
}

object DirectToDiskMain extends IOApp {
  val targetDirectory: Path = Paths.get("target", "ep3")

  def run(args: List[String]): IO[ExitCode] = {
    // won't work for functional after M6 because of imperative-focused EIPs
    val approach = if (args.isEmpty) "functional" else args.head
    if (approach == "exit") { sys.exit(0) }
    val selection = if (args.isEmpty || args.tail.isEmpty) "M7" else args.tail.head
    println("Generating " + approach + " for " + selection)
    val main = new Main(approach, selection)

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { main }

      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory)
      _ <- IO { println(s"DONE, in ${targetDirectory} you can now run: sbt scalafmt Test/scalafmt test") }
    } yield result
  }
}

object GenerateAll extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    GenerateAllMain.run(List.empty)
    GenerateAllProducer.run(List.empty)
    GenerateAllThirdAlternate.run(List.empty)
    GenerateAllD1D2.run(List.empty)
    GenerateAllJournal.run(List.empty)
    GenerateAllJ.run(List.empty)
  }
}

object GenerateAllMain extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    val approaches = Seq("oo","visitor","visitorSideEffect","extensibleVisitor","interpreter","coco","trivially","algebra")
    val evolutions = Seq("M0","M1","M2","M3","M4","M5","M6","M7","M7I2","M8","M9","I1","A1","A1M3","A1M3I2","A3","I2",
      "O1","O2","OA","O1OA","OD1","OD2","OD3","OO1","OO2","OO3")

    approaches.foreach(approach => {
      println("Generating " + approach + "...")
      evolutions.foreach(selection => {
        println("   " + selection)

        val targetDirectory = Paths.get("target", "ep-scala", approach, selection)
        val program :IO[Unit] = {
          for {
            _ <- IO { print("Initializing Generator...") }
            main <- IO {  new Main(approach, selection) }

            _ <- IO { println("[OK]") }
            _ <- main.runDirectToDisc(targetDirectory)
          } yield ()
        }

        // execute above as a stand-alone program
        program.unsafeRunSync()

        // TBD:  Would be nice to launch 'sbt' in each of these generated directories
      })
    })

    for {
      _ <- IO { print(s"DONE, in your target directory you can now run: sbt scalafmt Test/scalafmt test") }
    } yield ExitCode.Success

  }
}

object GenerateAllProducer extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    val approaches = Seq("oo","visitor","visitorSideEffect","extensibleVisitor","interpreter","coco","trivially","algebra")
    val evolutions = Seq("M0","M1","M2","M3", "W1", "M3W1", "Q1", "C2", "V1")

    approaches.foreach(approach => {
      println("Generating " + approach + "...")
      evolutions.foreach(selection => {
        println("   " + selection)

        val targetDirectory = Paths.get("target", "ep-scala-producer", approach, selection)
        val program :IO[Unit] = {
          for {
            _ <- IO { print("Initializing Generator...") }
            main <- IO {  new Main(approach, selection) }

            _ <- IO { println("[OK]") }
            _ <- main.runDirectToDisc(targetDirectory)
          } yield ()
        }

        // execute above as a stand-alone program
        program.unsafeRunSync()

        // TBD:  Would be nice to launch 'sbt' in each of these generated directories
      })
    })

    for {
      _ <- IO { print("DONE") }
    } yield ExitCode.Success

  }
}

object GenerateAllThirdAlternate extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    val approaches = Seq("oo","visitor","visitorSideEffect","extensibleVisitor","interpreter","coco","trivially","algebra")
    val evolutions = Seq("M0", "X1", "X2", "X3", "X2X3", "X4")

    approaches.foreach(approach => {
      println("Generating " + approach + "...")
      evolutions.foreach(selection => {
        println("   " + selection)

        val targetDirectory = Paths.get("target", "ep-scala-third-alternate", approach, selection)
        val program :IO[Unit] = {
          for {
            _ <- IO { print("Initializing Generator...") }
            main <- IO {  new Main(approach, selection) }

            _ <- IO { println("[OK]") }
            _ <- main.runDirectToDisc(targetDirectory)
          } yield ()
        }

        // execute above as a stand-alone program
        program.unsafeRunSync()

        // TBD:  Would be nice to launch 'sbt' in each of these generated directories
      })
    })

    for {
      _ <- IO { print("DONE") }
    } yield ExitCode.Success

  }
}

object GenerateAllD1D2 extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    val approaches = Seq("oo","visitor","visitorSideEffect","extensibleVisitor","interpreter","coco","trivially","algebra")
    val evolutions = Seq("M0", "M1", "D1", "D2", "D1D2", "D3")

    approaches.foreach(approach => {
      println("Generating " + approach + "...")
      evolutions.foreach(selection => {
        println("   " + selection)

        val targetDirectory = Paths.get("target", "ep-scala-d1d2", approach, selection)
        val program :IO[Unit] = {
          for {
            _ <- IO { print("Initializing Generator...") }
            main <- IO {  new Main(approach, selection) }

            _ <- IO { println("[OK]") }
            _ <- main.runDirectToDisc(targetDirectory)
          } yield ()
        }

        // execute above as a stand-alone program
        program.unsafeRunSync()

        // TBD:  Would be nice to launch 'sbt' in each of these generated directories
      })
    })

    for {
      _ <- IO { print("DONE") }
    } yield ExitCode.Success

  }
}

object GenerateAllJournal extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    val approaches = Seq("oo","visitor","visitorSideEffect","extensibleVisitor","interpreter","coco","trivially","algebra")
    val evolutions = Seq("M0","M1","M2","I1","I2","N1","M2_ABS","M3","M3I1","I2M3I1N1")
    approaches.foreach(approach => {
      println("Generating " + approach + "...")
      evolutions.foreach(selection => {
        println("   " + selection)

        val targetDirectory = Paths.get("target", "ep-scala-journal", approach, selection)
        val program :IO[Unit] = {
          for {
            _ <- IO { print("Initializing Generator...") }
            main <- IO {  new Main(approach, selection) }

            _ <- IO { println("[OK]") }
            _ <- main.runDirectToDisc(targetDirectory)
          } yield ()
        }

        // execute above as a stand-alone program
        program.unsafeRunSync()

        // TBD:  Would be nice to launch 'sbt' in each of these generated directories
      })
    })

    for {
      _ <- IO { print("DONE") }
    } yield ExitCode.Success

  }
}

object GenerateAllJ extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    val approaches = Seq("oo","visitor","visitorSideEffect","extensibleVisitor","interpreter","coco","trivially","algebra")
    val evolutions = Seq("M0","J1","J2","J3","K1","K2","J4","J5","J6","K2J6","J7","J8")

    approaches.foreach(approach => {
      println("Generating " + approach + "...")
      evolutions.foreach(selection => {
        println("   " + selection)

        val targetDirectory = Paths.get("target", "ep-scala-j", approach, selection)
        val program :IO[Unit] = {
          for {
            _ <- IO { print("Initializing Generator...") }
            main <- IO {  new Main(approach, selection) }

            _ <- IO { println("[OK]") }
            _ <- main.runDirectToDisc(targetDirectory)
          } yield ()
        }

        // execute above as a stand-alone program
        program.unsafeRunSync()

        // TBD:  Would be nice to launch 'sbt' in each of these generated directories
      })
    })

    for {
      _ <- IO { print("DONE") }
    } yield ExitCode.Success

  }
}
