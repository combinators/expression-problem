package org.combinators.ep.domain    /*DI:LI:AI*/

import org.combinators.ep.domain.abstractions.Operation
import org.combinators.ep.generator.communication.PotentialRequest
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider}

/**
 * Generate online at
 *
 * https://dreampuf.github.io/GraphvizOnline
 */
object GraphViz {

  val collapse_isOps:Boolean = true

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

    val output = new java.io.File(new java.io.File("target"), fileName)
    val fileWriter = new java.io.FileWriter (output)
    fileWriter.write("digraph G {\n")
    fileWriter.write("label = \"Dependency Graph\"\n")

    // includes edges
    fileWriter.write (outputNodes(model))

    // special edges based on operation dependencies.

    val addedNodes = scala.collection.mutable.Map[String,Int]()
    val addedArrows = scala.collection.mutable.Map[String,Int]()

    model.inChronologicalOrder.reverse
      .map(m => {
        m.typeCases.map(tpe => {
          m.flatten.ops.map(op => {
            val result = domainSpecific.dependencies(PotentialRequest(model.baseDataType, tpe, op))
            if (result.isDefined) {
              result.get.map(depend_op => {
                val defining_model = m.findOperation(depend_op).get
                // now we know that for this m, there is a dashed line to the model that defined operation
                // and the label on that dashed arrow must be

                val fromName = if (collapse_isOps && op.name.startsWith("is")) {
                  "isOP"
                } else { op.name }
                val toName = if (collapse_isOps && depend_op.name.startsWith("is")) {
                  "isOP"
                } else { depend_op.name }

                val arrow = f" ${m.name.toUpperCase()} -> ${defining_model.name.toUpperCase()} [style=dashed, color=grey label=<${fromName}-${toName}>]"
                addedArrows += arrow -> 1
              })
            }
          })
        })

        // built in operations. NOTE: IN this case, it MIGHT happen the m.findOperation cannot find
        // the dependent operation, so it must be skipped
        Set(Operation.asTree).map(op => {
          m.typeCases.map(tpe => {
            val result = domainSpecific.dependencies(PotentialRequest(model.baseDataType, tpe, op))
            if (result.isDefined) {
              result.get.map(depend_op => {
                val defining_model = m.findOperation(depend_op)
                // now we know that for this m, there is a dashed line to the model that defined operation
                // and the label on that dashed arrow must be
                if (defining_model.isDefined) {

                  val fromName = if (collapse_isOps && op.name.startsWith("is")) {
                    "isOP"
                  } else { op.name }
                  val toName = if (collapse_isOps && depend_op.name.startsWith("is")) {
                    "isOP"
                  } else { depend_op.name }

                  val arrow = f" ${m.name.toUpperCase()} -> ${defining_model.get.name.toUpperCase()} [style=dashed, color=grey label=<${fromName}-${toName}>]"
                  addedArrows += arrow -> 1
                }
              })
            }
          })
        })

        // check dependencies on operations defined anew ONLY
        m.ops.map(op => {
          m.flatten.typeCases.map(tpe => {
            val result = domainSpecific.dependencies(PotentialRequest(model.baseDataType, tpe, op))
            if (result.isDefined) {
              result.get.map(depend_op => {
                val defining_model = m.findOperation(depend_op).get

                val fromName = if (collapse_isOps && op.name.startsWith("is")) {
                  "isOP"
                } else { op.name }
                val toName = if (collapse_isOps && depend_op.name.startsWith("is")) {
                  "isOP"
                } else { depend_op.name }

                // now we know that for this m, there is a dashed line to the model that defined operation
                // and the label on that dashed arrow must be
                val arrow = f" ${m.name.toUpperCase()} -> ${defining_model.name.toUpperCase()} [style=dashed, color=grey label=<${fromName}-${toName}>]"
                addedArrows += arrow -> 1
              })



              // MIGHT be too much detail but is accurate
//              if (result.get.isEmpty) {
//                // at least want to record that this EIP is responsible for the op/eip
//
//                val arrow = f"""${m.name.toUpperCase()} -> ${op.name} [shape=dot dir=none]"""
//                addedArrows += arrow -> 1
//                val node =f"""${op.name} [shape=circle fixedsize=true width="0.25" label=<${op.name}>]"""
//                addedNodes += node -> 1
//              }
            }
          })
        })

        // even if neither types or ops (especially then, since a merge) must find dependencies.
      })

    addedNodes.foreach(pair => fileWriter.write (pair._1 + "\n"))
    addedArrows.foreach(pair => fileWriter.write (pair._1 + "\n"))

    fileWriter.write("}\n")
    fileWriter.close()
    println("GraphViz Dependency written to " + output)
  }

  def outputGraphViz(model:GenericModel, fileName:String = "evolution.viz"): Unit = {

    val output = new java.io.File(new java.io.File("target"), fileName)
    val fileWriter = new java.io.FileWriter (output)
    fileWriter.write("digraph G {\n")
    fileWriter.write("label = \"Evolution Graph\"\n")

    fileWriter.write(outputNodes(model))

    fileWriter.write("}\n")
    fileWriter.close()
    println("GraphViz EvolutionGraph written to " + output)
  }
}
