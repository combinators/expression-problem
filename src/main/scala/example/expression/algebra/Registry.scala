package example.expression.algebra

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import example.expression.j.Operators
import expression.{Exp, Operation}
import org.combinators.templating.twirl.Java

object registry extends Operators {
  /** Implementations for an operation. Map(op, Map(exp,MethodDecls)). */
  var implementations:Map[Class[_ <: Operation],Map[Class[_ <: Exp],MethodDeclaration]] = Map()

  /**
    * Return desired map of expressions by operation.
    *
    * @param op    Operation under consideration
    */
  def getImplementation(op:Operation):Map[Class[_ <: Exp],MethodDeclaration] = implementations(op.getClass)

  /**
    * Return whether operation exists.
    *
    * @param op    Operation under consideration
    */
  def hasImplementation(op:Operation):Boolean = implementations.contains(op.getClass)

  /**
    * Given an operation and a map of (Exp x String) call addImpl on each.
    *
    * Placed here for easy global access.
    *
    * @param op
    * @param map
    */
  def registerImpl (op:Operation, map:Map[Exp,String]): Unit = {
    map.keys.foreach {
      key =>
        addImpl(op, key, Java(map(key)).statements())
    }
  }


  /**
    * For the given operation, add the sequence of statements to implement for given expression subtype.
    * This dynamically maintains a map which can be inspected for the code synthesis.
    *
    * @param op      Operation under consideration
    * @param exp     Expression context
    * @param stmts   Sequence of statements that represents implementation of operation in given context.
    */
  def addImpl(op:Operation, exp:Exp, stmts:Seq[Statement]): Unit = {
    val name = exp.getClass.getSimpleName

    print ("::::: addImpl:" + implementations.size)
    var map:Map[Class[_ <: Exp],MethodDeclaration] = if (implementations.contains(op.getClass)) {
      implementations(op.getClass) - exp.getClass
    } else {
      Map()
    }

    val tpe:String = Type_toString(op.`type`)
    map += (exp.getClass -> Java(
      s"""
         |public $tpe visit($name e) {
         |   ${stmts.mkString("\n")}
         |}
        """.stripMargin).methodDeclarations().head)

    implementations -= op.getClass
    implementations += (op.getClass -> map)
  }

}
