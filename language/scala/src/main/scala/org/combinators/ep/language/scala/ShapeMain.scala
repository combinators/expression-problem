package org.combinators.ep.language.scala     /*DI:LD:AD*/

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.ep.approach.functional
import org.combinators.ep.approach.oo._
import org.combinators.ep.language.scala.{ Syntax, ScalaNameProvider }
import org.combinators.ep.domain.{GenericModel, Model}
import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.shape.{S0, _}
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.generator.TestImplementationProvider
import org.combinators.jgitserv.{BranchTransaction, GitService}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
object ShapeMain extends IOApp {
  val generator = CodeGenerator(CodeGenerator.defaultConfig)

  val traditionalApproach = functional.Traditional[Syntax.default.type, generator.paradigm.type](generator.paradigm)(ScalaNameProvider, generator.functional, generator.functionalInMethod)
  // can't have all of these together
  /*val ooApproach = Traditional(generator.paradigm)(ScalaNameProvider, generator.ooParadigm)
  val visitorApproach = Visitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val visitorSideEffectApproach = VisitorSideEffect[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val extensibleVisitorApproach = ExtensibleVisitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val interpreterApproach = Interpreter[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val triviallyApproach = Trivially[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val algebraApproach = Algebra[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)*/

  // select one here.
  // val approach = ooApproach // Not yet
  // val approach = visitorApproach  // Not yet
  // val approach = visitorSideEffectApproach // Not yet
  // val approach = extensibleVisitorApproach // Not yet
  // val approach = triviallyApproach // Not yet
  val approach = traditionalApproach

  val evolutions = Seq(S0, S1) // ) // , M4, M5, M6)
  //val m4eip =
  val s0eip =
    eips.S0(approach.paradigm)(
      ffiArithmetic = generator.doublesInMethod,
      generator.realDoublesInMethod,
      generator.booleansInMethod,
      generator.stringsInMethod
  )
  val s1eip = eips.S1(approach.paradigm)(s0eip)(
    generator.doublesInMethod,
    generator.booleansInMethod
  )
  val eip = s1eip


  val tests = evolutions.scanLeft(Map.empty[GenericModel, Seq[TestCase]]) { case (m, evolution) =>
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
