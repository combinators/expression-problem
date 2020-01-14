package org.combinators.ep.approach.oo

import org.combinators.ep.domain.abstractions.{DataType, Operation}
import org.combinators.ep.generator.ApproachImplementationProvider
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.ObjectOriented

/**
 * DataTypes become interfaces
 */
trait BaseDataTypeAsInterface extends ApproachImplementationProvider {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]

  import ooParadigm._
  import paradigm._

  /**
   * Base Exp interface with no methods (for now).
   *
   * {{{
   *   public interface Exp {
   *     public tree.Tree astree();    // only when needed
   * }
   * }}}
   *
   * Eventually will have some work here for producer/binary methods
   *
   * Override traditional OO use where this is a class; here it is an interface
   *
   * @param tpe
   * @param ops -- ignored in this overridden capability
   * @return
   */
  def makeBase(tpe: DataType, ops: Seq[Operation]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- setInterface()
      } yield ()
    }

    addClassToProject(makeClass, names.mangle(names.conceptNameOf(tpe)))
  }
}
