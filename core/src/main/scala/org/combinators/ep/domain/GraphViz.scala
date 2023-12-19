package org.combinators.ep.domain.GraphViz

import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.domain.{Evolution, GenericModel}
import org.combinators.ep.generator.communication.PotentialRequest
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

object GraphViz {
  def outputNodes(model:GenericModel) : Unit = {
    model.inChronologicalOrder
      .map(m => {

        // nodes
        val entry = s"""${m.name.toUpperCase()} [shape=rect label=<

        <TABLE BORDER="0">
          <TR>
            <TD BGCOLOR="lightblue">${m.typeCases.map(tpe => tpe.name).mkString(", ")}</TD>
            <TD BGCOLOR="beige">${m.ops.map(op => op.name).mkString(", ")}</TD>
          </TR>
          <tr><td colspan="2" style="text-align:center">${m.name.toUpperCase()}</td></tr>
        </TABLE>
        >]"""

        println(entry)

        // edges
        m.former.map(pm => println(s"${m.name.toUpperCase()} -> ${pm.name.toUpperCase()}"))
      })
  }

  def outputGraphWithDependenciesViz[AIP <: ApproachImplementationProvider](model:GenericModel, domainSpecific: EvolutionImplementationProvider[AIP]): Unit = {
    //eip.dep
    println("==========================================================")
    println("digraph G {")
    println("label = \"Dependency Graph\"")

    // includes edges
    outputNodes(model)

    // special edges based on operation dependencies.

    model.inChronologicalOrder.reverse
      .map(m => {
        val added = scala.collection.mutable.Map[String,Int]()

        m.typeCases.map(tpe => {
          m.flatten.ops.map(op => {
            val result = domainSpecific.dependencies(PotentialRequest(model.baseDataType, tpe, op))
            if (result.isDefined) {
              result.get.map(depend_op => {
                val defining_model = m.findOperation(depend_op).get
                // now we know that for this m, there is a dashed line to the model that defined operation
                // and the label on that dashed arrow must be
                val arrow = f" ${m.name.toUpperCase()} -> ${defining_model.name.toUpperCase()} [style=dashed, color=grey label=<${op.name}-${depend_op.name}>]"
                added += arrow -> 1
              })
            }
          })
        })

        // built in operations. NOTE: IN this case, it MIGHT happen the m.findOperation cannot find
        // the dependent operation, so it must be skipped
        Set(Operation.asTree).map(op => {
          m.flatten.typeCases.map(tpe => {
            val result = domainSpecific.dependencies(PotentialRequest(model.baseDataType, tpe, op))
            if (result.isDefined) {
              result.get.map(depend_op => {
                val defining_model = m.findOperation(depend_op)
                // now we know that for this m, there is a dashed line to the model that defined operation
                // and the label on that dashed arrow must be
                if (defining_model.isDefined) {
                  val arrow = f" ${m.name.toUpperCase()} -> ${defining_model.get.name.toUpperCase()} [style=dashed, color=grey label=<${op.name}-${depend_op.name}>]"
                  added += arrow -> 1
                }
              })
            }
          })
        })

        m.ops.map(op => {
          m.flatten.typeCases.map(tpe => {
            val result = domainSpecific.dependencies(PotentialRequest(model.baseDataType, tpe, op))
            if (result.isDefined) {
              result.get.map(depend_op => {
                val defining_model = m.findOperation(depend_op).get
                // now we know that for this m, there is a dashed line to the model that defined operation
                // and the label on that dashed arrow must be
                val arrow = f" ${m.name.toUpperCase()} -> ${defining_model.name.toUpperCase()} [style=dashed, color=grey label=<${op.name}-${depend_op.name}>]"
                added += arrow -> 1
              })
            }
          })
        })

        // now add to the graph as an edge
        added.map(pair => println (pair._1))
      })

    println("}")
    println("==========================================================")
    println("OUTPUT DM")
  }

  def outputGraphViz(lastOne:Evolution): Unit = {

    val model = lastOne.getModel

    println("----------------------------------------------------------")
    println("digraph G {")
    println("label = \"Evolution Graph\"")

    outputNodes(model)

    println("}")
    println("----------------------------------------------------------")
  }
}
