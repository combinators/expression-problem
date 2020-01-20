package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions.{DataType, Operation, TypeRep}
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.ObjectOriented

/**
 * Ability to create a chain of interfaces, each one specifying operations.
 *
 *  {{{
 *     public interface Exp {}                              // marker interface
 *     public interface ExpEval extends Exp { ... }
 *     public interface ExpPrettyp extends ExpEval { ... }
 * }}}
 *
 * Where 'Exp' comes from the BaseDataType of the domain.
 */
trait OperationInterfaceChain extends ApproachImplementationProvider  {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]

  import ooParadigm._
  import paradigm._
  import syntax._

  /** Requires capability of defining the signature of a method associated with the given operation. */
  def makeSignature(op: Operation): Generator[MethodBodyContext, Unit]

//  def baseInterfaceNamesPrefix(ops: Seq[Operation], suffix:Name): Name = {
//    // Note: foldLeft requires swap of comparison operation because....
//    ops.sortWith(_.name > _.name).map(op => names.conceptNameOf(op)).foldLeft(suffix){ case (n,s) => names.addPrefix(s, n) }
//  }

  def baseInterfaceNames(domain: Model): Seq[Name] = {
    Seq(names.mangle(domain.name), names.mangle(domain.baseDataType.name))
  }

  // extends Exp [first one] or ExpEval [previous one]
  // Works for both Exp* interface declarations as well as DataTypeOp declarations
  def getParentInterface(domain: Model, tpe: DataType): Generator[ClassContext, Type] = {
    import classCapabilities._

    if (domain.isEmpty || domain.lastModelWithOperation.isEmpty) {
      findClass(names.mangle(domain.baseDataType.name))
    } else {
      findClass(baseInterfaceNames(domain) : _*)
    }
  }


  /**
   * Create intermediate interfaces that form a chain of operation definitions.
   * Now this is created EVEN when an evolution doesn't create an operation
   * @param domainSpecific
   * @return
   */
  def makeIntermediateInterface(domain:Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    // create class which is an interface containing abstract methods
    val makeInterface: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- setInterface()
        _ <- if (domain.last.isDefined) {   // avoid when the first one
          for {
            parent <- getParentInterface(domain.last.get, domain.baseDataType)
            _ <- resolveAndAddImport(parent)
            _ <- addParent(parent)
          } yield ()
        } else {
          Command.skip[ClassContext]
        }
        _ <- forEach (domain.ops) { op => addAbstractMethod(names.mangle(names.instanceNameOf(op)), makeSignature(op)) }
      } yield ()
    }

    addClassToProject(makeInterface, baseInterfaceNames(domain) : _*)  }
}
