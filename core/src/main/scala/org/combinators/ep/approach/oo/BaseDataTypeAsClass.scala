package org.combinators.ep.approach.oo

import org.combinators.ep.domain.abstractions.{DataType, Operation}
import org.combinators.ep.generator.ApproachImplementationProvider
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.ObjectOriented

/**
 * DataTypes become classes
 */
trait BaseDataTypeAsClass extends ApproachImplementationProvider {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]

  import ooParadigm._
  import paradigm._
  import syntax._

  /** Requires capability of defining the signature of a method associated with the given operation. */
  def makeSignature(op: Operation): Generator[MethodBodyContext, Unit]

  def makeBase(tpe: DataType, ops: Seq[Operation]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- setAbstract()
        _ <- forEach(ops) { op => addAbstractMethod(names.mangle(names.instanceNameOf(op)), makeSignature(op)) }
      } yield ()
    }
    addClassToProject(names.mangle(names.conceptNameOf(tpe)), makeClass)
  }
}
