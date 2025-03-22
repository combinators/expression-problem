package org.combinators.ep.approach.oo     /*DI:LI:AD*/

import org.combinators.ep.domain.{GenericModel, GraphViz}
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

    val allDataTypeCases = flatDomain.typeCases.toSet // idea from coco
    val allOperations = flatDomain.ops.toSet

    // be sure to grab ALL optimizations that have appeared List of (DataTypeCase, Operation)
    val overriddenMap = domain.toSeq.foldLeft(Map.empty[DataTypeCase, Set[Operation]]) {
      (resultMap, m) =>
        m.optimizations.foldLeft(resultMap) { (resultMap, pair) =>
          resultMap.updated(pair._1, Set(pair._2) ++ resultMap.getOrElse(pair._1, Set.empty))
        }}

    val lastExp = latestModelDefiningInterface(domain)
    val overriddenMap1 = domain.toSeq.filter(dm => lastExp.before(dm)).flatMap(m => m.optimizations).groupBy(_._1).map(f => (f._1, f._2.map(pair => pair._2).toSet))        //.groupBy{ case (tpe, op) => tpe. }

    println("OverrideMap: ", domain.name + "," + overriddenMap.map(pair => pair._1.name + " [" + pair._2.map(op => op.name)
      .foldLeft("") {case (g,str) => g + str + ","}
      + "], ")
      .foldLeft("") { case (group, str) => group + str })

    // Merging makes this more complicated BECAUSE there could be multiple Exp that are brought together,
    // and if so, then will need to BLEND together
    val pastWithExp = domain.former.filter(dm => dm == latestModelDefiningInterface(dm))
    val xyz = if (pastWithExp.length > 1) { pastWithExp.flatMap(m => dataTypeCasesWithNewOperations(m)) } else { Seq.empty }
    val mergeResults = xyz.groupBy(_._1).map(triple => triple._1 -> triple._2.flatMap(pm => pm._2)).filter(entry => entry._2.nonEmpty)
    //  .map(pair => (pair._1, pair._2.flatMap(p => p._2).toSet))
    if (domain.name.equals("oo3")) {
      println ("SDS")
    }
    val mergeResultsxxx = if (pastWithExp.length > 1) { pastWithExp.flatMap(m => dataTypeCasesWithNewOperations(m)).toMap } else { Map.empty[DataTypeCase, Set[Operation]] }

    val mergeMap = if (pastWithExp.length > 1) {
      pastWithExp.foldLeft(overriddenMap){ (updatedMap, m) =>
        dataTypeCasesWithNewOperations(m).foldLeft(updatedMap) { (nextMap, pair) => {
          nextMap.updated(pair._1, pair._2 ++ nextMap.getOrElse(pair._1, Set.empty))
        }}
      }
      // multiple Exp in former, so we have to join together
    } else { overriddenMap}

    val mergeMap0 = if (pastWithExp.length > 1) {
      pastWithExp.foldLeft(Map.empty[DataTypeCase, Set[Operation]]){ (updatedMap, m) =>
        dataTypeCasesWithNewOperations(m).foldLeft(updatedMap) { (nextMap, pair) => {
          nextMap.updated(pair._1, pair._2 ++ nextMap.getOrElse(pair._1, Set.empty))
        }}
      }
      // multiple Exp in former, so we have to join together
    } else { Map.empty[DataTypeCase, Set[Operation]]}


    println("MergeMap: ", domain.name + "," + mergeMap0.map(pair => pair._1.name + " [" + pair._2.map(op => op.name)
      .foldLeft("") {case (g,str) => g + str + ","}
      + "], ")
      .foldLeft("") { case (group, str) => group + str })

    // whenever a new Exp is defined, MUST duplicate logic for all producer methods; incorporate into logic below
    val addedExp = domain == latestModelDefiningInterface(domain)

    val resultss = allDataTypeCases.map(tpe => {
      val mt = domain.findTypeCase(tpe).get

      val affected = allOperations.filter(op => {
        val mo = domain.findOperation(op).get
        val descendant = domain.inChronologicalOrder.find(m => !m.notComparableTo(mt) && !m.notComparableTo(mo) && mt.beforeOrEqual(m) && mo.beforeOrEqual(m)).get
        !descendant.before(domain) || (addedExp && op.isProducer(domain))
      })

      (tpe, affected)
    }).filter(pair => pair._2.nonEmpty).toMap


    val affected = allOperations.map(op => {
        val dts = allDataTypeCases.filter(tpe => {
        val mt = domain.findTypeCase(tpe).get
        val mo = domain.findOperation(op).get

        // find FIRST one that is descendant. ONE must exist, since Graph is whole
        val descendant = domain.inChronologicalOrder.find(m => !m.notComparableTo(mt) && !m.notComparableTo(mo) && mt.beforeOrEqual(m) && mo.beforeOrEqual(m)).get
        !descendant.before(domain) || (addedExp && op.isProducer(domain))
      })
    }
    )

    val results0 = allOperations.foldLeft(Map.empty[DataTypeCase, Set[Operation]]) { (resultMap, op) =>
      allDataTypeCases.foldLeft(resultMap) { (nextMap, tpe) =>
        val mt = domain.findTypeCase(tpe).get
        val mo = domain.findOperation(op).get

        // find FIRST one that is descendant. ONE must exist, since Graph is whole
        val descendant = domain.inChronologicalOrder.find(m => !m.notComparableTo(mt) && !m.notComparableTo(mo) && mt.beforeOrEqual(m) && mo.beforeOrEqual(m)).get

        if (!descendant.before(domain) || (addedExp && op.isProducer(domain))) {
          nextMap.updated(tpe, Set(op) ++ resultMap.getOrElse(tpe, Set.empty))
        } else {
          nextMap
        }
      }
    }
    val results = allOperations.foldLeft(mergeMap) { (resultMap, op) =>
      allDataTypeCases.foldLeft(resultMap) { (nextMap, tpe) =>
        val mt = domain.findTypeCase(tpe).get
        val mo = domain.findOperation(op).get

        // find FIRST one that is descendant. ONE must exist, since Graph is whole
        val descendant = domain.inChronologicalOrder.find(m => !m.notComparableTo(mt) && !m.notComparableTo(mo) && mt.beforeOrEqual(m) && mo.beforeOrEqual(m)).get

        if (!descendant.before(domain) || (addedExp && op.isProducer(domain))) {
          nextMap.updated(tpe, Set(op) ++ resultMap.getOrElse(tpe, Set.empty))
        } else {
          nextMap
        }
      }
    }
    println("ResultsMap: ", domain.name + "," + results0.map(pair => pair._1.name + " [" + pair._2.map(op => op.name)
      .foldLeft("") {case (g,str) => g + str + ","}
      + "], ")
      .foldLeft("") { case (group, str) => group + str })

    val res =
      Seq(overriddenMap1, mergeResults, resultss)
        .flatten                        // List[(String, String)]
        .groupBy { case (k, _) => k }   // Map[String, List[(String, String)]]
        .map(pair => (pair._1, pair._2.flatMap(p => p._2).toSet))
    //.mapValues(_.map { case (_, v) => v })
    if (!res.equals(results)) {
      println("BAD")
    }

    results
  }

  def dataTypeCasesWithNewOperationsReduced(domain: GenericModel): Map[DataTypeCase, Set[Operation]] = {
    val flatDomain = domain.flatten

    val allDataTypeCases = flatDomain.typeCases.toSet
    val allOperations = flatDomain.ops.toSet

    val overridden = domain.toSeq.flatMap(m => m.optimizations).groupBy(_._1).map(entry => (entry._1, entry._2.map(pair => pair._2).toSet))

    // Merging makes this more complicated BECAUSE there could be multiple Exp that are brought together,
    // and if so, then will need to BLEND together
    val pastWithExp = domain.former.filter(dm => dm == latestModelDefiningInterface(dm))

    val merged = if (pastWithExp.length > 1) {
      pastWithExp.flatMap(m => dataTypeCasesWithNewOperations(m)).groupBy(_._1)
        .map(triple => triple._1 -> triple._2.flatMap(pm => pm._2))
        .filter(entry => entry._2.nonEmpty)
    } else { Seq.empty }
    //val merged1 = xyz.groupBy(_._1).map(triple => triple._1 -> triple._2.flatMap(pm => pm._2)).filter(entry => entry._2.nonEmpty)

    // whenever a new Exp is defined, MUST duplicate logic for all producer methods; incorporate into logic below
    val addedExp = domain == latestModelDefiningInterface(domain)

    val updated = allDataTypeCases.map(tpe => {
      val mt = domain.findTypeCase(tpe).get

      val affected = allOperations.filter(op => {
        val mo = domain.findOperation(op).get
        val descendant = domain.inChronologicalOrder.find(m => !m.notComparableTo(mt) && !m.notComparableTo(mo) && mt.beforeOrEqual(m) && mo.beforeOrEqual(m)).get
        !descendant.before(domain) || (addedExp && op.isProducer(domain))
      })

      (tpe, affected)
    }).filter(pair => pair._2.nonEmpty).toMap

    Seq(overridden, merged, updated)
        .flatten
        .groupBy { case (k, _) => k }
        .map(pair => (pair._1, pair._2.flatMap(p => p._2).toSet))
  }

  def implement(gdomain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    println()
    //gdomain.toSeq.foreach(m => println(m.name, latestModelDefiningInterface(m).name))

    //gdomain.toSeq.foreach(m => println(m.name, dataTypeCasesWithNewOperations(m).map(pair => pair._1.name ++ "," ++ pair._2.map(op => op.name))))
    gdomain.toSeq.reverse.foreach(m => println(m.name,
      dataTypeCasesWithNewOperations(m).map(pair => pair._1.name + " [" + pair._2.map(op => op.name)
        .foldLeft("") {case (g,str) => g + str + ","}
         + "], ")
        .foldLeft("") { case (group, str) => group + str }
    ))

    // document evolutions
    GraphViz.outputGraphViz(gdomain)

    // Document EIPs
    GraphViz.outputGraphWithDependenciesViz(gdomain, domainSpecific)

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
