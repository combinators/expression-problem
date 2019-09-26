package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm._
import Command._
import AnyParadigm.syntax._
import org.combinators.ep.approach

/**
 * Synthesizing OO and Functional Design to promote Reuse
 * Shriram Krishnamurthi, Matthias Felleisen, Daniel Friedman
 * https://dl.acm.org/citation.cfm?id=679709
 *
 * TODO: Doesn't yet work for c1 merged, since it reuses code from visitor (constructors)
 * that need to be modified instead
 */
trait ExtensibleVisitor extends Visitor {
  import paradigm._
  import ooParadigm._

  import syntax._

  /** Concatenate all types in this model to form proper suffix for operation classes. */
  def modelTypes(model:Model) : String = {
    if (model.last.isEmpty) {
      ""
    } else {
      model.typeCases.sortWith(_.name < _.name).mkString("")
    }
  }

  /** Each operation is placed in its own class, with a 'visit' method for newly defined types.
   *
   * {{{
   *   public class EvalSub extends Eval implements VisitorSub<Double> {
   *
   *     public Double visit(Sub e) {
   *         return e.getLeft().accept(makeEval()) - e.getRight().accept(makeEval());
   *     }
   *
   *     EvalSub makeEval() {
   *         return new EvalSub();
   *     }
   * }
   * }}}
   *
   * @param domain     Model for which new types are to be incorporated
   * @param op
   * @param domainSpecific
   * @return           Returns class context without actually adding to ProjectContext; this is job of caller of this function
   */
  override def makeOperationImplementation(domain:Model,
                                  op: Operation,
                                  domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    val regularVisitor = super.makeOperationImplementation(domain, op, domainSpecific)

    val full:String = modelTypes(domain)
    val lastWithType:Option[Model] = if (domain.last.isEmpty) {
      None
    } else {
      domain.last.get.lastModelWithDataTypes
    }
    val lastOperation = if (lastWithType.isDefined) {
      lastWithType.get.findOperation(op)
    } else {
      None
    }

    // Must take care to ensure we don't mistakenly go back *before* where the operation was defined.
    // This is determined by looking for operations in the past.
    val last = if (lastWithType.isEmpty || lastOperation.isEmpty) {
      ""
    } else {
      modelTypes(lastWithType.get)
    }

    // add to regular visitor

    val makeClass: Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      import genericsParadigm.classCapabilities._
      for {
        _ <- setAbstract()
        _ <- addTypeParameter(names.mangle(visitTypeParameter), Command.skip)    // R by itself, since not extending any other type parameter (hence Skip)
        _ <- forEach (domain.typeCases) { tpe =>
          addMethod(names.mangle(names.instanceNameOf(tpe)), makeImplementation(domain.baseDataType, tpe, op, domainSpecific))
        }
      } yield ()
    }

    makeClass
    //    // if I want to override a super, this is a mistake since this will be added to project.
    //    addClassToProject(visitorClass, makeClass)
  }




  /**
   * The Extensible Visitor approach is defined as follows
   *
   * 1. Make the base class (for the domain)
   * 2. For each of the data types (in flattened set) create a derived class
   * 3. Create the Visitor interface
   *
   * @param domain
   * @param domainSpecific
   * @return
   */
  override def implement(domain: Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val flatDomain = domain.flatten
    for {
      _ <- initializeApproach(flatDomain)
      _ <- domainSpecific.initialize(this)
      _ <- makeBase(flatDomain.baseDataType)
      _ <- forEach (flatDomain.typeCases) { tpeCase =>
        makeDerived(flatDomain.baseDataType, tpeCase, flatDomain.ops, domainSpecific)
      }
      _ <- makeVisitorInterface(flatDomain.typeCases)
      _ <- forEach (flatDomain.ops) { op =>
        addClassToProject(names.mangle(names.conceptNameOf(op)), makeOperationImplementation(flatDomain, op, domainSpecific))
      }

      // cannot have extension for the FIRST model entry so that must be skipped.
      //_ <- makeOperatorExtension(op, m)
      models = domain.inChronologicalOrder
        .filter(m => m.typeCases.nonEmpty)
        .filter(m => m.last.nonEmpty)

      _ <- forEach (models) { m =>
        forEach (m.last.get.pastOperations) { op =>
          // THIS won't be right because name is based on Visitor$full<$opType>. How can we create a class
          // and name it? Hate to have to have separate method to redo the work
          addClassToProject(names.mangle(names.conceptNameOf(op)), makeOperationImplementation(m, op, domainSpecific))
        }
      }

//          addClassToProject(names.mangle(names.conceptNameOf(op)), makeOperationImplementation(flatDomain, op, domainSpecific))

    } yield ()
  }

}

object ExtensibleVisitor {
  type WithParadigm[P <: AnyParadigm] = ExtensibleVisitor { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   oo: ObjectOriented.WithBase[base.type],
   params: ParametricPolymorphism.WithBase[base.type])
  (generics: Generics.WithBase[base.type,oo.type,params.type]): ExtensibleVisitor.WithParadigm[base.type] =
    new ExtensibleVisitor {
      val paradigm: base.type = base
      val names: NameProvider[paradigm.syntax.Name] = nameProvider
      val ooParadigm: oo.type = oo
      val polymorphics: params.type = params
      val genericsParadigm: generics.type = generics
    }
}
