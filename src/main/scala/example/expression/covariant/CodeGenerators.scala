package example.expression.covariant

import com.github.javaparser.ast.stmt.Statement
import example.expression.j.MethodMapper
import expression.{DomainModel, Exp}
import expression.data._
import expression.extensions.{Divd, Mult, Neg, Sub}
import org.combinators.cls.types.Type
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry
import org.combinators.cls.types.syntax._

/**
  * Common code generators for the covariant solution.
  *
  * Customized to know about the mode recent model domain
  */
class CodeGenerators(model:DomainModel)  {

  /**
    * Return the operator to use for a given binary exp type in Java.
    *
    * @param exp Exp subclass that corresponds to a binary operator.
    * @return
    */
  def getBinaryOperator( exp:Exp) : com.github.javaparser.ast.expr.BinaryExpr.Operator = {
    exp match {

      case _:Add => com.github.javaparser.ast.expr.BinaryExpr.Operator.PLUS
      case _:Sub => com.github.javaparser.ast.expr.BinaryExpr.Operator.MINUS
      case _:Mult => com.github.javaparser.ast.expr.BinaryExpr.Operator.MULTIPLY
      case _:Divd => com.github.javaparser.ast.expr.BinaryExpr.Operator.DIVIDE

      case _ => null
    }
  }

  /**
    * Return the operator to use for a given unary exp type in Java.
    *
    * @param exp  Exp subclass that corresponds to a unary operator.
    * @return
    */
  def getUnaryOperator( exp:Exp) : com.github.javaparser.ast.expr.UnaryExpr.Operator = {
    exp match {

      case _:Neg => com.github.javaparser.ast.expr.UnaryExpr.Operator.MINUS

      case _ => null
    }
  }

  /**
    * Code generator for reproducing the structure of the covariant invocation for eval.
    */
  val evalGenerators:CodeGeneratorRegistry[Seq[Statement]] = CodeGeneratorRegistry.merge[Seq[Statement]](
    CodeGeneratorRegistry[Seq[Statement], Lit] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Lit) =>
        Java(s"""return value();""").statements()
    },

    CodeGeneratorRegistry[Seq[Statement], BinaryExp] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], binary:BinaryExp) =>
        Java(s"""return left().eval() ${getBinaryOperator(binary).asString} right().eval();""").statements()
    },

    CodeGeneratorRegistry[Seq[Statement], UnaryExp] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], unary:UnaryExp) =>
        Java(s"""return ${getUnaryOperator(unary).asString} exp().eval();""").statements()
    },
  )

  /**
    * Code generator for reproducing the structure of the visitor pattern invocation for prettyP.
    */
  val prettypGenerators:CodeGeneratorRegistry[Seq[Statement]] = CodeGeneratorRegistry.merge[Seq[Statement]](
    CodeGeneratorRegistry[Seq[Statement], Lit] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Lit) =>
        Java(s"""return "" + value();""").statements()
    },

    CodeGeneratorRegistry[Seq[Statement], BinaryExp] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], binary:BinaryExp) =>
        Java(s"""return "(" + left().print() + "${getBinaryOperator(binary).asString}" + right().print() + ")";""").statements()
    },

    CodeGeneratorRegistry[Seq[Statement], UnaryExp] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], unary:UnaryExp) =>
        Java(s"""return "${getUnaryOperator(unary).asString}" + exp().print();""").statements()
    },
  )

  /**
    * Code generator for reproducing the structure of the visitor pattern invocation for prettyP.
    */
  val collectLitGenerators:CodeGeneratorRegistry[Seq[Statement]] = CodeGeneratorRegistry.merge[Seq[Statement]](
    CodeGeneratorRegistry[Seq[Statement], Lit] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Lit) =>
        Java("""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                |list.add(value());
                |return list;
                |""".stripMargin).statements()
    },

    CodeGeneratorRegistry[Seq[Statement], BinaryExp] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:BinaryExp) =>
        Java("""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                |list.addAll(left().collectList());
                |list.addAll(right().collectList());
                |return list;
                |""".stripMargin).statements()
    },

    CodeGeneratorRegistry[Seq[Statement], UnaryExp] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:UnaryExp) =>
        Java("""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                |list.addAll(exp().collectList());
                |return list;
                |""".stripMargin).statements()
    },
  )

  /**
    * Code generator for reproducing the structure of the visitor pattern invocation for simplify
    */
  val collectSimplifyGenerators:CodeGeneratorRegistry[Seq[Statement]] = CodeGeneratorRegistry.merge[Seq[Statement]](
    CodeGeneratorRegistry[Seq[Statement], Lit] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Lit) =>
        Java("""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                |list.add(value());
                |return list;
                |""".stripMargin).statements()
    },

    CodeGeneratorRegistry[Seq[Statement], BinaryExp] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:BinaryExp) =>
        Java("""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                |list.addAll(left().collectList());
                |list.addAll(right().collectList());
                |return list;
                |""".stripMargin).statements()
    },

    CodeGeneratorRegistry[Seq[Statement], UnaryExp] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:UnaryExp) =>
        Java("""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                |list.addAll(exp().collectList());
                |return list;
                |""".stripMargin).statements()
    },
  )

/**
  * When used, it isn't important what semantic Type is, which is why we omit it.
  *
  * Extend MethodMapper to have access to representation
  */
class ExpressionCombinator(e:Exp) extends MethodMapper {

  def apply(generators: CodeGeneratorRegistry[Expression]): Expression = {
    val cc3: Option[Expression] = generators(e)
    if (cc3.isEmpty) {
      Java("false").expression()
    } else {
      cc3.get
    }
  }

  var semanticType: Type = representation(representation.eval) =>: representation(representation.expr)
}

}
