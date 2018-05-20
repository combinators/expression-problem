package example.expression.covariant

import expression.data.{Add, Eval, Lit}
import expression.history.History
import expression.{DomainModel, Exp, Operation}
import org.combinators.cls.types.Constructor

import scala.collection.JavaConverters._

/**
  * Determine targets based on different approaches.
  */
object Synthesizer extends SemanticTypes  {

  /**
    * Compute all targets to be synthesized for covariant solution.
    *
    * @param hist     Domain Model Evolution history
    * @param tag      Which domain model to synthesize
    */
  def covariantTargets(hist:History, tag:String): Seq[Constructor] = {
    var seq:Seq[Constructor] = Seq.empty

    val flat:DomainModel = hist.flatten(tag)

    seq = seq :+ ep(ep.interface, new Exp)      // baseline

    // strip out Eval from this list?
    val subsets:List[List[Operation]] = flat.ops.asScala.toSet[Operation].filterNot(p => p.getClass.getSimpleName.equals("Eval")).subsets.map(_.toList).toList.filter(_.nonEmpty)

    subsets.foreach {
      sub: List[Operation] => {
        println(">>> sub:" + sub.toString)
        if (sub.length == 1) {
          seq = seq :+ ep(ep.interface, sub.head)
        } else {
          // only call for > 1 subset length
          seq = seq :+ ep(ep.interface, sub)

          flat.data.asScala.foreach(
            e => {
              seq = seq :+ ep(ep.interface, e, sub)
              seq = seq :+ ep(ep.finalType, e, sub)
            }
          )
        }
      }
    }

    // every sub-type gets a target
    flat.data.asScala.foreach (
      e => seq = seq :+ ep(ep.finalType, e)
    )

    // default methods for Eval
    flat.data.asScala.foreach(
      e => seq = seq :+ ep(ep.defaultMethods, e, new Eval)
    )

    // every type x op gets a target
    flat.data.asScala.foreach (
      e => {
        // Skip all Eval...
        flat.ops.asScala.filterNot(p => p.getClass.getSimpleName.equals("Eval")).foreach (
          o => {
            seq = seq :+ ep(ep.finalType, e, List(o))
            seq = seq :+ ep(ep.interface, e, o)
          }
        )
      }
    )

    // not needed if above works.
    //seq = seq :+ ep(ep.interface, new Eval)

    // test cases
    seq = seq :+ driver

    seq
  }
}

