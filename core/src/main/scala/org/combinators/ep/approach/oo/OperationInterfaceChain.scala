package org.combinators.ep.approach.oo    /*DI:LI:AD*/

import org.combinators.ep.domain.{GenericModel, Model}
import org.combinators.ep.domain.abstractions.{DataType, Operation, TypeRep}
import org.combinators.ep.generator.{ApproachImplementationProvider, Command, EvolutionImplementationProvider}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.{Generics, ObjectOriented, ParametricPolymorphism}

/**
 * Ability to create a chain of interfaces, each one specifying operations.
 * Depends (for now) on lastModelWithOperation
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
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]

  import ooParadigm._
  import paradigm._
  import syntax._

  /** Requires capability of defining the signature of a method associated with the given operation. */
  def makeSignature(op: Operation): Generator[MethodBodyContext, Unit]

//  def baseInterfaceNamesPrefix(ops: Seq[Operation], suffix:Name): Name = {
//    // Note: foldLeft requires swap of comparison operation because....
//    ops.sortWith(_.name > _.name).map(op => names.conceptNameOf(op)).foldLeft(suffix){ case (n,s) => names.addPrefix(s, n) }
//  }

  def baseInterfaceNames(domain: GenericModel): Seq[Name] = {
    if (domain.isDomainBase) {
      // ignore MathDomain, for example, and just grab name...
      Seq(names.mangle(domain.baseDataType.name))
    } else {
      Seq(names.mangle(domain.name), names.mangle(domain.baseDataType.name))
    }
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

  /** Returns the prior model declaring the necessary interface. To be overridden as needed (i.e., Interpreter). */
  def modelDeclaringInterfaceParent(model:Model) : Model = {
     model.last.get
  }

  /**
   * UGLY code because of optional arguments and need to deal with generics.
    */
  def makeInterface(domain:Model, domainSpecific: EvolutionImplementationProvider[this.type], typeParameter:Option[Name] = None): Generator[ClassContext, Unit] = {
    // create class which is an interface containing abstract methods
     import classCapabilities._
      import genericsParadigm.classCapabilities._

      for {
        _ <- setInterface()

        _ <- if (typeParameter.isDefined) {
          for {
            _ <- addTypeParameter(typeParameter.get, Command.skip)
          } yield ()
        } else {
          Command.skip[ClassContext]
        }

        _ <- if (domain.last.isDefined && domain.last.get.name != domain.base.name) {  // can't be first one (HACK)
          for {
            parent <- getParentInterface(modelDeclaringInterfaceParent(domain), domain.baseDataType)

            // AWKWARD! Have to grab the type parameter from the current class since I can't seem
            // to just convert a string like "V" into a Type... That would be useful!
            _ <- if (typeParameter.isDefined) {
              for {
                justV <- getTypeArguments().map(_.head)
                paramType <- applyType(parent, Seq(justV))

                _ <- addParent(paramType)
              } yield ()
            } else {
              for {
                _ <- resolveAndAddImport(parent)
                _ <- addParent(parent)
              } yield ()
            }

          } yield ()
        } else {
          Command.skip[ClassContext]
        }

        _ <- forEach (domain.ops) { op => addAbstractMethod(names.mangle(names.instanceNameOf(op)), makeSignature(op)) }
      } yield ()
    }

  /**
   * Create intermediate interfaces that form a chain of operation definitions.
   * Now this is created EVEN when an evolution doesn't create an operation
   * @param domainSpecific
   * @return
   */
  def addIntermediateInterfaceToProject(domain:Model, domainSpecific: EvolutionImplementationProvider[this.type], typeParameter:Option[Name] = None): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    addClassToProject(makeInterface(domain, domainSpecific, typeParameter), baseInterfaceNames(domain) : _*)  }
}
