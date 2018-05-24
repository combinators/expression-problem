package example.expression.covariant

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import example.expression.j.Operators
import expression.{Attribute, Exp, FunctionMethod, Operation}
import expression.types.Types
import org.combinators.templating.twirl.Java

import scala.collection.JavaConverters._

trait Registry extends Operators {
  /**
    * Given an interface for a type, adds a default implementation of given operation
    *
    * @param fm    Domain Model Function Method that models this operation
    * @param sub   The subType associated with....
    * @param stmts ...the statements containing an implementation of Operation for SubType.
    */
  def AddDefaultImpl(fm: FunctionMethod, sub: Exp, stmts: Seq[Statement], unit:CompilationUnit): CompilationUnit = {

      val tpe = Type_toString(fm.returnType)
      val name = fm.name

      // methods are marked as default later
      val methods = Java(s"$tpe $name() { ${stmts.mkString} }").methodDeclarations()

      // these are default methods
      methods.foreach { m =>
        m.setDefault(true)
        unit.getTypes.get(0).getMembers.add(m)
      }
      unit
    }

  /**
    * Given an extension to Exp and a given operation (and its stmts implementation) produce an
    * interface with default method. Overide methods that are of class Exp. Thus: AddExpOperation (Add, PrettyP, ...)
    *
    * interface AddPrettyP extends Add, PrettyP {
    * PrettyP left();
    * PrettyP right();
    * default String print() {
    * return "(" + left().print() + " + " + right().print() + ")";
    * }
    * }
    *
    * @param exp   SubType (i.e., Add) for which an operation is to be defined.
    * @param op    Operation to be defined.
    * @param stmts Default set of statements for implementation
    */
  def AddExpOperation(exp: Exp, op: Operation, stmts: Seq[Statement]): CompilationUnit = {
      val opName = op.getClass.getSimpleName
      val expName = exp.getClass.getSimpleName

      val unit: CompilationUnit = Java(
        s"""
           |package ep;
           |interface $expName$opName extends $expName, $opName { }
           |""".stripMargin).compilationUnit()

      val tpe = Type_toString(op.`type`)

      // methods are marked as default later
      val methods: Seq[MethodDeclaration] = Java(
        s"""
           |$tpe ${op.name}() {
           |   ${stmts.mkString("\n")}
           |}
         """.stripMargin).methodDeclarations()

      // reclassify an field of type Exp with the more precise $expName
      // PrettyP left();
      // PrettyP right();
      exp.ops.asScala.foreach {
        case att: Attribute =>
          // only redefine if originally the Exp field.
          if (att.attType == Types.Exp) {
            val fields: Seq[MethodDeclaration] = Java(s"""$opName ${att.attName}();""").methodDeclarations()

            fields.foreach { x => unit.getTypes.get(0).getMembers.add(x) }
          }

        case _: FunctionMethod =>
      }

      // these are default methods
      methods.foreach { m =>
        m.setDefault(true)
        unit.getTypes.get(0).getMembers.add(m)
      }

      unit
    }
}
