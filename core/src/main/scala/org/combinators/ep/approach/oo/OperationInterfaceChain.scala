package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions.{DataType, Operation, TypeRep}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}
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
trait OperationInterfaceChain extends ApproachImplementationProvider with SharedOO {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]

  import ooParadigm._
  import paradigm._
  import syntax._

  def baseInterfaceNamesPrefix(ops: Seq[Operation], suffix:Name): Name = {
    // Note: foldLeft requires swap of comparison operation because....
    ops.sortWith(_.name > _.name).map(op => names.conceptNameOf(op)).foldLeft(suffix){ case (n,s) => names.addPrefix(s, n) }
  }

  def baseInterfaceNames(domain: Model, ops: Seq[Operation]): Name = {
    baseInterfaceNamesPrefix(ops, names.mangle(domain.baseDataType.name))
  }

  // extends Exp [first one] or ExpEval [previous one]
  // Works for both Exp* interface declarations as well as DataTypeOp declarations
  def getParentInterface(domain: Model, tpe: DataType): Generator[ClassContext, Type] = {
    import classCapabilities._

    if (domain.isEmpty || domain.lastModelWithOperation.isEmpty) {
      findClass(names.mangle(domain.baseDataType.name))
    } else {
      findClass(baseInterfaceNames(domain, domain.lastModelWithOperation.get.ops))
    }
  }

  /**
   * Base Exp interface with no methods (for now).
   *
   * {{{
   *   public interface Exp {
   *     public tree.Tree astree();    // only when needed
   * }
   * }}}
   * Eventually will have some here for producer/binary methods
   *
   * Override traditional OO use where this is a class; here it is an interface
   *
   * @param tpe
   * @param ops -- ignored in this overridden capability
   * @return
   */
  override def makeBase(tpe: DataType, ops: Seq[Operation]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- setInterface()
      } yield ()
    }
    addClassToProject(names.mangle(names.conceptNameOf(tpe)), makeClass)
  }

  /**
   * Create intermediate interfaces that form a chain of operation definitions.
   *
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
        parent <- getParentInterface(domain.last.get, domain.baseDataType)
        _ <- resolveAndAddImport(parent)
        _ <- addParent(parent)
        _ <- forEach (domain.ops) { op => addAbstractMethod(names.mangle(names.instanceNameOf(op)), makeSignature(op)) }
      } yield ()
    }

    addClassToProject(baseInterfaceNames(domain, domain.ops), makeInterface)
  }
}
