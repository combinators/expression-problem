package org.combinators.ep.domain

import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.generator.communication.PotentialRequest
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

/**
 * Generate online at
 *
 * https://dreampuf.github.io/GraphvizOnline
 */
object GraphViz {
  def outputNodes(model:GenericModel) : String = {
    val added = scala.collection.mutable.Map[String,Int]()

    var result = new String("")
    model.inChronologicalOrder
      .map(m => {

        // nodes
        val entry = s"""${m.name.toUpperCase()} [shape=rect label=<

        <TABLE BORDER="0">
          <TR>
            <TD BGCOLOR="lightblue">${m.typeCases.map(tpe => tpe.name).mkString(", ")}</TD>
            <TD BGCOLOR="beige">${m.ops.map(op =>
          if (op.isBinary(model) && op.isProducer(model)) { f"${op.name}<sub>BP</sub>" }
          else if (op.isBinary(model)) { f"${op.name}<sub>B</sub>"}
          else if (op.isProducer(model)) { f"${op.name}<sub>P</sub>"}
          else { op.name } ).mkString(", ")}</TD>
          </TR>
          <tr><td colspan="2" style="text-align:center">${m.name.toUpperCase()}</td></tr>
        </TABLE>
        >]"""

        result += entry + "\n"

        // edges
        m.former.map(pm => {
          val arrow = s"${m.name.toUpperCase()} -> ${pm.name.toUpperCase()}"
          added += arrow -> 1
        })
      })

    added.foreach(pair => result += pair._1 + "\n")
    result
  }

  def outputGraphWithDependenciesViz[AIP <: ApproachImplementationProvider]
               (model:GenericModel,
                domainSpecific: EvolutionImplementationProvider[AIP],
                fileName:String = "eip.viz"): Unit = {

    val fileWriter = new java.io.FileWriter (new java.io.File(fileName))
    fileWriter.write("digraph G {\n")
    fileWriter.write("label = \"Dependency Graph\"\n")

    // includes edges
    fileWriter.write (outputNodes(model))

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
        added.foreach(pair => fileWriter.write (pair._1 + "\n"))
      })

    fileWriter.write("}\n")
    fileWriter.close()
    println("GraphViz Dependency written to " + fileName)
  }

  def outputGraphViz(model:GenericModel, fileName:String = "evolution.viz"): Unit = {

    val fileWriter = new java.io.FileWriter (new java.io.File(fileName))
    fileWriter.write("digraph G {\n")
    fileWriter.write("label = \"Evolution Graph\"\n")

    fileWriter.write(outputNodes(model))

    fileWriter.write("}\n")
    fileWriter.close()
    println("GraphViz EvolutionGraph written to " + fileName)
  }
}
