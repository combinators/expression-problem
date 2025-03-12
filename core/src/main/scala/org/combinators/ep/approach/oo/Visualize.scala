package org.combinators.ep.approach.oo     /*DI:LI:AD*/

import org.combinators.ep.domain.GenericModel
import org.combinators.ep.domain.GraphViz
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm._

/**
 * Process model by generating representations suitable for GraphViz
 *
 * https://dreampuf.github.io/GraphvizOnline
 *
 * Choose "graphviz" as the approach and (for either Java or Scala) generate the resulting
 * code. What it does, instead, is generate the OO solution and then creates files, either
 * "eip.viz" (structural evolution) or "evolution.viz" (which also includes dependency links)
 * that you can copy and paste into the above service.
 */
trait Visualize extends SharedOO {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]

  import paradigm._
  import syntax._

  // Here b/c of AIP though not being called.
  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      method <- getMember(message.to, names.mangle(names.instanceNameOf(message.request.op)))
      result <- apply(method, message.request.op.parameters.map(message.request.arguments))
    } yield result
  }

  // Here b/c of AIP though not being called.
  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      rt <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(rt)

      res <- instantiateObject(rt, args)
    } yield res
  }

  /** Find the last evolution that requires its own Exp definition. */
  def latestModelDefiningInterface(domain: GenericModel): GenericModel = {

    // Merge needs a new EXP only if there is a new operation in one of the branches going back to common ancestor.
    if (domain.isDomainBase || domain.ops.nonEmpty) {
      domain
    } else {
      // is there a single type that can represent the "least upper bound" of all prior branches. (TAKEN FROM COCO)
      val ancestorsWithTypeInterfaces = domain.former.map(parent => latestModelDefiningInterface(parent)).distinct

      // If there is one model that is the DESCENDANT of ALL of these ancestors, then that is
      // the one to choose, otherwise we must be a merge and we handle it
      val candidates = ancestorsWithTypeInterfaces.filter(m => ancestorsWithTypeInterfaces.forall(gm => gm.beforeOrEqual(m)))
      if (candidates.length == 1) {
        candidates.head
      } else {
        domain
      }
    }
  }

  def dataTypeCasesWithNewOperations(domain: GenericModel): Map[DataTypeCase, Set[Operation]] = {
    val flatDomain = domain.flatten

    val allDataTypeCases = flatDomain.typeCases.toSet  // idea from coco
    val allOperations = flatDomain.ops.toSet

    val initialMap = domain.optimizations.foldLeft(Map.empty[DataTypeCase, Set[Operation]]) { (resultMap, pair) =>
      resultMap.updated(pair._1, Set(pair._2) ++ resultMap.getOrElse(pair._1, Set.empty) )
    }

    val real_result = allOperations.foldLeft(initialMap) { (resultMap, op) =>
      allDataTypeCases.foldLeft(resultMap) { (nextMap, tpe) =>
        val mt = domain.findTypeCase(tpe).get
        val mo = domain.findOperation(op).get

        // find FIRST one that is descendant. ONE must exist, since Graph is whole
        val descendant = domain.inChronologicalOrder.find(m => !m.notComparableTo(mt) && !m.notComparableTo(mo) && mt.beforeOrEqual(m) && mo.beforeOrEqual(m)).get

        if (!descendant.before(domain)) {
          nextMap.updated(tpe, Set(op) ++ resultMap.getOrElse(tpe, Set.empty))
        } else { nextMap }}
    }

    real_result
//
//    // This is a map of (tpeCase, Set[Operations]) that are needed to be generated AT THIS domain
//
//    val full = allOperations.flatMap(op => allDataTypeCases.map(tpe => {
//      val mt = domain.findTypeCase(tpe).get
//      val mo = domain.findOperation(op).get
//
//      // find FIRST one that is descendant. ONE must exist, since Graph is whole
//      val descendant = domain.inChronologicalOrder.find(m => !m.notComparableTo(mt) && !m.notComparableTo(mo) && mt.beforeOrEqual(m) && mo.beforeOrEqual(m)).get
//      if (descendant.before(domain)) {
//        None
//      } else {
//        Some(tpe, op)
//      }
//    }))
//    println(domain.name, full)
//
//    // New Exp is created only with new operations
//    // it might be overkill to assign allOperations as the value
//    if (domain.former.length > 1) {
//      return allDataTypeCases.map(dt => (dt, allOperations)).toMap
//    }
//
//    // start with any optimized (DataTypeCase, Operation) since these have new implementations and so MUST have default interface implementation
//    val optimizedMap = domain.optimizations.foldLeft(Map.empty[DataTypeCase, Set[Operation]]) { (resultMap, pair) =>
//      resultMap.updated(pair._1, Set(pair._2) ++ resultMap.getOrElse(pair._1, Set.empty) )
//    }
//
//    allDataTypeCases.foldLeft(Map.empty[DataTypeCase, Set[Operation]]) { (resultMap, tpeCase) =>
//
//      // When tpeCase is being defined in this 'domain', will need all past operations.
//      val pastOperations = if (domain.typeCases.contains(tpeCase)) {
//        domain.pastOperations    // including this one
//      } else {
//        Seq.empty
//      }
//
//      // Any optimized tpe cases with operation must be included, as well as producer methods.
//      val optimized = optimizedMap.getOrElse(tpeCase, Set.empty)
//      // if all producers then I2 generates MORE than it should.
//      val producers = Seq.empty  // allOperations.filter(op => op.isProducer(domain))
//
//      val updatedOperations = (domain.ops ++ optimized ++ producers ++ pastOperations).toSet
//      resultMap.updated(tpeCase, updatedOperations)
//    }
  }

  def implement(gdomain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {

    gdomain.toSeq.foreach(m => println(m.name, latestModelDefiningInterface(m).name))
    // document evolutions
    GraphViz.outputGraphViz(gdomain)

    // Document EIPs
    GraphViz.outputGraphWithDependenciesViz(gdomain, domainSpecific)

    gdomain.toSeq.foreach(gm => println(gm.name, dataTypeCasesWithNewOperations(gm).filter(p => p._2.nonEmpty).map(pair => s"${pair._1.name}: ${pair._2.map(op => op.name)}")))

    // produce table
    val flat = gdomain.flatten

    // header
    print("OP,")
    flat.typeCases.foreach(tpe => {
      print(tpe.name + ",")
    })
    println()

    flat.ops.foreach(op => {
      print(op.name + ",")
      flat.typeCases.foreach(tpe => {
        val opt = latestModelDefiningOperatorClass(gdomain, tpe, op, domainSpecific)

        if (opt.isEmpty) {
          print("-,")
        } else {
          print(opt.get.name + ",")
        }
      })
      println()
    })

    val flatDomain = gdomain.linearize.flatten
    for {
      _ <- registerTypeMapping(flatDomain)
      _ <- domainSpecific.initialize(this)
    } yield ()
  }
}

object Visualize {
  type WithParadigm[P <: AnyParadigm] = Visualize { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   oo: ObjectOriented.WithBase[base.type]
  ): Visualize.WithParadigm[base.type] =
    new Visualize {
      override val paradigm: base.type = base
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val ooParadigm: ObjectOriented.WithBase[paradigm.type] = oo
    }
}
