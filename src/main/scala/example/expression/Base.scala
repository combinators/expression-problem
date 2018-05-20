package example.expression

import org.combinators.cls.interpreter.ReflectedRepository
import expression.DomainModel
import expression.history.History
import expression.instances.UnitSuite

trait Base {

  /**
    * To be overridden by sub-typed traits that wish to add combinators programmatically to the repository
    *
    * @param gamma    The current repository
    * @param history  The domain model history
    * @tparam G       Typed parameter
    * @return
    */
  def init[G <: ExpressionDomain](gamma : ReflectedRepository[G], history:History) : ReflectedRepository[G] = gamma

  // Find way to maintain clean separation between Language (i.e., Java) and constraints (i.e., NextRank).

}

// This class exists so the 'def init' methods can be included in any trait that wishes
// to add dynamic traits to the repository. Otherwise nothing is included here (for now)
class ExpressionDomain(val history:History, val allTests:UnitSuite) {
  // assumed access to expression
  println ("Domain Size:" + history.size());

}

