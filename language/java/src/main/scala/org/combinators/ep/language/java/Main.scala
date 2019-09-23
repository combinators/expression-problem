package org.combinators.ep.language.java

import java.nio.file.Paths

import org.combinators.ep.approach.oo
import org.combinators.ep.approach.oo.Traditional
import org.combinators.ep.domain.math._
import org.combinators.ep.generator.ApproachImplementationProvider
import org.combinators.jgitserv.BranchTransaction
import org.eclipse.jgit.api.Git

object Main extends App {
  val targetDirectory = Paths.get("target", "ep-generated")


  val generator = CodeGenerator()
  val approach = Traditional[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)
  val directory = Paths.get(targetDirectory.toString, approach.getClass.getSimpleName)
  val git = Git.init().setDirectory(directory.toFile).call()
  val evolutions = Seq(M0, M1, M2, M3)

  evolutions.foreach { evolution =>

    val impl = approach.implement(
          evolution.getModel,
          eips.M3.apply[approach.paradigm.type, Traditional.WithParadigm](approach.paradigm)(generator.doublesInMethod, generator.stringsInMethod)
        )

      //val transaction = BranchTransaction.empty(evolution.logic())

    }
  }


}
