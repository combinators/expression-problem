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

  def implement(gdomain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {

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

