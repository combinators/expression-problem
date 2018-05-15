package example.expression.visitor

import com.github.javaparser.ast.stmt.Statement
import example.expression.j.Operators
import expression.data._
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

/**
  * Collect implementation using standard data type
  *
  * Not yet extensible to new data types that fall outside of the Lit/BinaryExp/UnaryExp hierarchy.
  */
trait CollectCodeGenerators extends Operators {

  /**
    * Code generator for reproducing the structure of the visitor pattern invocation for collect.
    */
  val collectGenerators:CodeGeneratorRegistry[Seq[Statement]] = CodeGeneratorRegistry.merge[Seq[Statement]](
    CodeGeneratorRegistry[Seq[Statement], Lit] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Lit) =>
        Java(s"""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                 |list.add(e.getValue());
                 |return list;
                 |""".stripMargin).statements()
    },

    CodeGeneratorRegistry[Seq[Statement], BinaryExp] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], binary:BinaryExp) =>
        Java(s"""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                 |list.addAll(e.getLeft().accept(this));
                 |list.addAll(e.getRight().accept(this));
                 |return list;
                 |""".stripMargin).statements()
    },

    CodeGeneratorRegistry[Seq[Statement], UnaryExp] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], unary:UnaryExp) => {
        Java(s"""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                 |list.addAll(e.getExp().accept(this));
                 |return list;""".stripMargin).statements()
      }
    },
  )
}
