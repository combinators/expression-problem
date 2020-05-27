package org.combinators.ep.language.java

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.ep.approach.oo.{Algebra, ExtensibleVisitor, Interpreter, Traditional, Trivially, ViTA, Visitor, VisitorSideEffect}
import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.math._
import org.combinators.ep.generator.TestImplementationProvider
import org.combinators.jgitserv.{BranchTransaction, GitService}
import org.combinators.ep.generator.FileWithPathPersistable._

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
object Main extends IOApp {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed))

  val ooApproach = Traditional[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)
  // can't have all of these together
  val visitorApproach = Visitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val visitorSideEffectApproach = VisitorSideEffect[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val extensibleVisitorApproach = ExtensibleVisitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val interpreterApproach = Interpreter[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val triviallyApproach = Trivially[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val algebraApproach = Algebra[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

  val vitaApproach = ViTA[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

  // select one here.
  // val approach = ooApproach // WORKS!
  // val approach = visitorApproach  // WORKS!
  // val approach = visitorSideEffectApproach // WORKS!
  // val approach = extensibleVisitorApproach // WORKS!
  // val approach = triviallyApproach // triviallyApproach // WORKS!
  val approach = vitaApproach

  //val evolutions = Seq(M0, M1, M2, I1, I2)    // , I2 //       M3, M4, M5, M6) // ) // , M4, M5, M6)
  val evolutions = Seq(M0, M1, M2, M3, M4, M5, M6, M7)

//  val eip = eips.I2(approach.paradigm)(generator.doublesInMethod, generator.realDoublesInMethod,
//    generator.stringsInMethod, generator.imperativeInMethod)
//  // how do I just use M2 instead of this? HACK
  val m4eip =
    eips.M4.imperative(approach.paradigm)(
      generator.imperativeInMethod,
      generator.doublesInMethod,
      generator.booleansInMethod,
      generator.stringsInMethod,
      generator.listsInMethod,
      generator.equalityInMethod)
  val m5eip = eips.M5(approach.paradigm)(m4eip)(
    generator.intsInMethod,
    generator.treesInMethod)
  val m6eip = eips.M6(approach.paradigm)(m5eip)(
    generator.equalityInMethod
  )
  val eip = eips.M7(approach.paradigm)(m6eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod)

  val tests = evolutions.scanLeft(Map.empty[Model, Seq[TestCase]]) { case (m, evolution) =>
    m + (evolution.getModel -> evolution.tests)
  }.tail

  val transaction =
    evolutions.zip(tests).foldLeft(Option.empty[BranchTransaction]) {
      case (transaction, (evolution, tests)) =>
    val impl =
      for {
        _ <- approach.implement(evolution.getModel, eip)
        _ <- approach.implement(
          tests,
          TestImplementationProvider.defaultAssertionBasedTests(approach.paradigm)(generator.assertionsInMethod, generator.equalityInMethod, generator.booleansInMethod, generator.stringsInMethod)
        )
      } yield ()
    val nextTransaction =
      transaction.map(_.fork(evolution.getModel.name).deleteAllFiles)
        .getOrElse(BranchTransaction.empty(evolution.getModel.name))
    Some(nextTransaction.persist(generator.paradigm.runGenerator(impl)).commit("Adding next evolution"))
  }

  def run(args: List[String]): IO[ExitCode] = {
    val name = evolutions.head.getModel.base.name
    for {
      _ <- IO { System.out.println(s"Use: git clone http://127.0.0.1:8081/$name ${evolutions.last.getModel.name}") }
      exitCode <- new GitService(transaction.toSeq, name).run(args)
      //exitCode <- new GitService(transaction.toSeq, name).runProcess(Seq(s"sbt", "test"))
    } yield exitCode
  }
}
