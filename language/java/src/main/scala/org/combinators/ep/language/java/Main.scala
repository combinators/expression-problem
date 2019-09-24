package org.combinators.ep.language.java

import java.nio.file.Paths

import org.combinators.ep.approach.oo
import org.combinators.ep.approach.oo.{Traditional, Visitor}
import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.math._
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider, TestImplementationProvider}
import org.combinators.jgitserv.BranchTransaction
import org.eclipse.jgit.api.Git

object Main extends App {
  val targetDirectory = Paths.get("target", "ep-generated")


  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = CodeGenerator.PartiallyBoxed))
  //val approach = Traditional[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)
  val approach = Visitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val directory = Paths.get(targetDirectory.toString, approach.getClass.getSimpleName)
  //val git = Git.init().setDirectory(directory.toFile).call()
  val evolutions = Seq(M0, M1, M2, M3)
  val tests = evolutions.scanLeft(Map.empty[Model, Seq[TestCase]]) { case (m, evolution) =>
    m + ((evolution.getModel -> evolution.tests))
  }

  evolutions.zip(tests).foreach { case (evolution, tests) =>

    val impl =
      for {
        _ <- approach.implement(
          evolution.getModel,
          eips.M3.apply(approach.paradigm)(generator.doublesInMethod, generator.stringsInMethod)
        )
        _ <- approach.implement(
          tests,
          TestImplementationProvider.defaultAssertionBasedTests(approach.paradigm)(generator.assertionsInMethod, generator.equalityInMethod, generator.booleansInMethod)
        )
      } yield ()

    generator.paradigm.runGenerator(impl).foreach(file =>
      System.out.println(new String(file.rawBytes)))
      //val transaction = BranchTransaction.empty(evolution.logic())
  }
}
