package example.expression.visitor

import expression.{DomainModel, Exp}
import org.combinators.cls.types.Constructor

import scala.collection.JavaConverters._

/**
  * Determine targets based on different approaches.
  */
object Synthesizer extends SemanticTypes  {

  /**
    * Compute all targets to be synthesized for visitor.
    *
    * @param model     Domain Model which contains the information about the problem
    */
  def visitorTargets(model:DomainModel): Seq[Constructor] = {
    var seq:Seq[Constructor] = Seq.empty

    seq = seq :+ generated(generated.visitor)
    seq = seq :+ exp(exp.base, new Exp)

    // every sub-type gets a target
    model.data.asScala.foreach (
      e => seq = seq :+ exp(exp.visitor, e)
    )

    // every op gets a target
    model.ops.asScala.foreach (
      o => seq = seq :+ ops(ops.visitor, o)
    )

    // test cases
    seq = seq :+ driver

    seq
  }



}

