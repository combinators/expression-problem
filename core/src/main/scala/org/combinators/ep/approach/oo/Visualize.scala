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

  def latestModelDefiningNewTypeInterface(domain: GenericModel): GenericModel = {
    if (domain.isDomainBase || domain.ops.nonEmpty) {
      domain
    } else {
      // is there a single type that can represent the "least upper bound" of all prior branches.
      val ancestorsWithTypeInterfaces = domain.former.map(ancestor => latestModelDefiningNewTypeInterface(ancestor)).distinct
      // To validate this works, need multiple branches where NEITHER defines operators
      if (ancestorsWithTypeInterfaces.size == 1 && !ancestorsWithTypeInterfaces.head.isDomainBase) { // take care to avoid falling below "floor"
        ancestorsWithTypeInterfaces.head
      } else {
        domain // we have to do merge
      }
    }
  }

  /**
   * Every stage needs a Factory
   */
  def latestModelDefiningNewFactoryType(domain: GenericModel): GenericModel = {
    if (domain.optimizations.nonEmpty || domain.isDomainBase || domain.typeCases.nonEmpty || domain == latestModelDefiningNewTypeInterface(domain)) {
      domain
    } else {
      // is there a single type that can represent the "least upper bound" of all prior branches.
      val ancestorsWithFactoryTypes = domain.former.map(ancestor => latestModelDefiningNewFactoryType(ancestor)).distinct
      // To validate this works, need multiple branches where NEITHER defines operators
      if (ancestorsWithFactoryTypes.size == 1 && !ancestorsWithFactoryTypes.head.isDomainBase) { // take care to avoid falling below "floor"
        ancestorsWithFactoryTypes.head
      } else {
        domain
      }
    }
  }

  def cocoNewDataTypeCasesWithNewOperations(domain: GenericModel): Map[DataTypeCase, Set[Operation]] = {
    val flatDomain = domain.flatten

    val allDataTypeCases = flatDomain.typeCases.toSet
    val allOperations = flatDomain.ops.toSet
    val lastExp = latestModelDefiningNewTypeInterface(domain)
    val overridden = domain.toSeq.filter(dm => lastExp.before(dm)).flatMap(m => m.optimizations).groupBy(_._1).map(entry => (entry._1, entry._2.map(pair => pair._2).toSet))

    // Merging makes this more complicated BECAUSE there could be multiple Exp that are brought together,
    // and if so, then will need to BLEND together. COCO DIFFERENT! IN OTHERS IT IS latestModelDefiningNewTypeInterface
    val pastWithExp = if (domain.former.length > 1) domain.former.filter(dm => dm == latestModelDefiningNewFactoryType(dm)) else Seq.empty

    // These are last with FACTORY. Now go back to last with Exp for each of these. Must grab
    val merged_exps = pastWithExp.flatMap(m => {
      val first = latestModelDefiningNewTypeInterface(m)
      domain.toSeq.filter(dm => first.beforeOrEqual(dm))
    }).distinct

    val allops = merged_exps.flatMap(m => m.ops).distinct
    val alltps = merged_exps.flatMap(m => m.typeCases).distinct
    val merged = alltps.map(tpe => (tpe, allops))

    // whenever a new Exp is defined,
    val addedExp = domain == lastExp

    val updated = allDataTypeCases.map(tpe => {
      val mt = domain.findTypeCase(tpe).get

      val affected = allOperations.filter(op => {
        val mo = domain.findOperation(op).get
        val descendant = domain.inChronologicalOrder.find(m => !m.notComparableTo(mt) && !m.notComparableTo(mo) && mt.beforeOrEqual(m) && mo.beforeOrEqual(m)).get
        !descendant.before(domain) || addedExp
      })

      (tpe, affected)
    }).filter(pair => pair._2.nonEmpty).toMap

    val output = Seq(overridden, merged, updated)
      .flatten
      .groupBy { case (k, _) => k }
      .map(entry => (entry._1, entry._2.flatMap(pair => pair._2).toSet))

    output
  }

  def objectAlgebrasDataTypeCasesWithNewOperations(evolutionImplementationProvider: EvolutionImplementationProvider[this.type], domain: GenericModel): Map[DataTypeCase, Set[Operation]] = {
    val flatDomain = domain.flatten
    val allDataTypeCases = flatDomain.typeCases.toSet
    val allOperations = flatDomain.ops.toSet

    allDataTypeCases.foldLeft(Map.empty[DataTypeCase, Set[Operation]]) { (resultMap, tpeCase) =>
      // Remembers all operations that are already supported
      val presentOperations = domain.operationsPresentEarlier(tpeCase)

      val overwrittenOperations = allOperations.filter { operation =>
        // Does our current domain contain an override implementation?
        evolutionImplementationProvider.evolutionSpecificDependencies(
          PotentialRequest(domain.baseDataType, tpeCase, operation)
        ).contains(domain)
        //        // Are we applicable based on EIP? Tells us in which domain EIP is applicable
        //        val lastOverwritingDomain =
        //          evolutionImplementationProvider.applicableIn(
        //            forApproach = this,
        //            potentialRequest = PotentialRequest(domain.baseDataType, tpeCase, operation),
        //            currentModel = domain
        //          )
        //        lastOverwritingDomain.contains(domain)
      }
      val updatedOperations = (allOperations -- presentOperations) ++ overwrittenOperations
      // If we have any updated operations, if we have a former one that doesn't support the current type case, or if we are in a merge.
      val output = if (updatedOperations.nonEmpty || domain.former.exists(ancestor => !ancestor.supports(tpeCase)) || domain.former.size > 1) {
        resultMap.updated(tpeCase, updatedOperations)
      } else {
        resultMap
      }
      output
    }
  }

  def triviallyDataTypeCasesWithNewOperations(domain: GenericModel): Map[DataTypeCase, Set[Operation]] = {
    val flatDomain = domain.flatten

    val allDataTypeCases = flatDomain.typeCases.toSet
    val allOperations = flatDomain.ops.toSet
    val lastExp = latestModelDefiningInterface(domain)
    val overridden = domain.toSeq.filter(dm => lastExp.before(dm)).flatMap(m => m.optimizations).groupBy(_._1).map(entry => (entry._1, entry._2.map(pair => pair._2).toSet))

    // Merging makes this more complicated BECAUSE there could be multiple Exp that are brought together,
    // and if so, then will need to BLEND together
    val pastWithExp = if (domain.former.length > 1) domain.former.filter(dm => dm == latestModelDefiningInterface(dm)) else Seq.empty

    val merged = pastWithExp.flatMap(m => triviallyDataTypeCasesWithNewOperations(m)).groupBy(_._1)
      .map(triple => triple._1 -> triple._2.flatMap(pm => pm._2))
      .filter(entry => entry._2.nonEmpty)

    // whenever a new Exp is defined, MUST duplicate logic for all producer methods; incorporate into logic below
    val addedExp = domain == lastExp

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
      .map(entry => (entry._1, entry._2.flatMap(pair => pair._2).toSet))
  }
  def ancestorsDefiningNewTypeInterfaces2(domain: GenericModel): Set[GenericModel] = {
    val ancestorsWithNewTypeInterfaces = domain.former.map(ancestor => latestModelDefiningNewTypeInterface2(ancestor))

    ancestorsWithNewTypeInterfaces.distinct.filterNot { ancestor =>
      ancestorsWithNewTypeInterfaces.exists(otherAncestor => ancestor.before(otherAncestor))
    }.toSet
  }
  def latestModelDefiningNewTypeInterface2(domain: GenericModel): GenericModel = {
    if (domain.isDomainBase || domain.ops.nonEmpty) {
      domain
    } else {
      // is there a single type that can represent the "least upper bound" of all prior branches.
      val ancestorsWithTypeInterfaces = ancestorsDefiningNewTypeInterfaces2(domain)  // INTERPRETER
      //val ancestorsWithTypeInterfaces = domain.former.map(ancestor => latestModelDefiningNewTypeInterface(ancestor)).distinct // COCO

      if (ancestorsWithTypeInterfaces.size == 1 && !ancestorsWithTypeInterfaces.head.isDomainBase) { // take care to avoid falling below "floor"
        ancestorsWithTypeInterfaces.head
      } else {
        domain // we have to do merge
      }
    }
  }

  def latestModelDefiningNewTypeInterface3(domain: GenericModel): GenericModel = {
    if (domain.isDomainBase || domain.ops.nonEmpty) {
      domain
    } else {
      // is there a single type that can represent the "least upper bound" of all prior branches.
      //val ancestorsWithTypeInterfaces = ancestorsDefiningNewTypeInterfaces2(domain)  // INTERPRETER
      val ancestorsWithTypeInterfaces = domain.former.map(ancestor => latestModelDefiningNewTypeInterface3(ancestor)).distinct // COCO

      if (ancestorsWithTypeInterfaces.size == 1 && !ancestorsWithTypeInterfaces.head.isDomainBase) { // take care to avoid falling below "floor"
        ancestorsWithTypeInterfaces.head
      } else {
        domain // we have to do merge
      }
    }
  }

  def implement(gdomain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    println()
    //gdomain.toSeq.foreach(m => println(m.name, latestModelDefiningInterface(m).name))

    //gdomain.toSeq.foreach(m => println(m.name, dataTypeCasesWithNewOperations(m).map(pair => pair._1.name ++ "," ++ pair._2.map(op => op.name))))
//    gdomain.toSeq.reverse.foreach(m => println(m.name,
//      dataTypeCasesWithNewOperations(m).map(pair => pair._1.name + " [" + pair._2.map(op => op.name)
//        .foldLeft("") {case (g,str) => g + str + ","}
//         + "], ")
//        .foldLeft("") { case (group, str) => group + str }
//    ))
    gdomain.toSeq.foreach(m => println(m.name, " has ", latestModelDefiningNewTypeInterface2(m).name))

    println()

    gdomain.toSeq.foreach(m =>
      println(m.name, " has ", latestModelDefiningNewTypeInterface3(m).name))

    print("CoCo:" + gdomain.name)
    val outputCoCO = new java.io.File(new java.io.File("target"), "coco-newDataTypeCases.txt")
    val fileWriterCoCo = new java.io.FileWriter (outputCoCO, true)
    gdomain.toSeq.distinct.foreach(m => {
      fileWriterCoCo.write(m.name + "," +
      cocoNewDataTypeCasesWithNewOperations(m).map(pair => pair._1.name + " [" + pair._2.map(op => op.name)
        .foldLeft("") { case (g, str) => g + str + "," }
        + "], ")
        .foldLeft("") { case (group, str) => group + str } + "\n")
    }
    )
    fileWriterCoCo.close()

    print("ObjectAlgebras:" + gdomain.name)
    val outputObjectAlgebras = new java.io.File(new java.io.File("target"), "algebra-newDataTypeCases.txt")
    val fileWriterObjAlg = new java.io.FileWriter (outputObjectAlgebras, true)
    gdomain.toSeq.distinct.foreach(m => {
      fileWriterObjAlg.write(m.name + "," +
      objectAlgebrasDataTypeCasesWithNewOperations(domainSpecific, m).map(pair => pair._1.name + " [" + pair._2.map(op => op.name)
        .foldLeft("") { case (g, str) => g + str + "," }
        + "], ")
        .foldLeft("") { case (group, str) => group + str } + "\n")
    }
    )
    fileWriterObjAlg.close()

    print("Trivially:" + gdomain.name)
    val outputObjectTrivially= new java.io.File(new java.io.File("target"), "trivially-newDataTypeCases.txt")
    val fileWriterTrivially= new java.io.FileWriter (outputObjectTrivially,  true)
    gdomain.toSeq.distinct.foreach(m => {
      fileWriterTrivially.write(m.name + "," +
      triviallyDataTypeCasesWithNewOperations(m).map(pair => pair._1.name + " [" + pair._2.map(op => op.name)
        .foldLeft("") { case (g, str) => g + str + "," }
        + "], ")
        .foldLeft("") { case (group, str) => group + str } + "\n")
    }
    )
    fileWriterTrivially.close()
//
//    // document evolutions
//    //GraphViz.outputGraphViz(gdomain)
//
//    // Document EIPs
//    //GraphViz.outputGraphWithDependenciesViz(gdomain, domainSpecific)
//
//    // produce table
//    val flat = gdomain.flatten
//
//    // header
//    print("OP,")
//    flat.typeCases.foreach(tpe => {
//      print(tpe.name + ",")
//    })
//    println()
//
//    flat.ops.foreach(op => {
//      print(op.name + ",")
//      flat.typeCases.foreach(tpe => {
//        val opt = latestModelDefiningOperatorClass(gdomain, tpe, op, domainSpecific)
//
//        if (opt.isEmpty) {
//          print("-,")
//        } else {
//          print(opt.get.name + ",")
//        }
//      })
//      println()
//    })

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
