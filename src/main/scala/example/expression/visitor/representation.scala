package example.expression.visitor

import com.github.javaparser.ast.stmt.Statement
import example.expression.j.MethodMapper
import expression.{Exp, Operation}
import expression.data.{Add, BinaryExp, Lit, UnaryExp}
import expression.extensions.{Divd, Mult, Neg, Sub}
import expression.types.{FrameworkTypes, GenericType, TypeInformation, Types}
import org.combinators.cls.types.Type
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry
import org.combinators.cls.types.syntax._

/**
  * Here is the default code registry for the existing Constraints in Solitaire domain.
  *
  * Over time, variations may introduce their own constraints, which are merged locally
  * within the variation. Should a constraint be used by multiple variations, then promote
  * to the end of this object.
  *
  * All logging through constraintCodeGenerators.logger
  */
object representationCodeGenerators  {

  // HACK: DUPLICATE FROM METHOD MAPPER SINCE CAN"'T BRING IN...
  /** Convert a type into its Java String equivalent. */
  def Type_toString (ty:TypeInformation): String =
    ty match {
      case Types.Exp=> "Exp"           // base class of everything

      case Types.Void => "void"
      case Types.Int => "Integer"      // allow boxing/unboxing for generics
      case Types.String => "String"
      case g:GenericType => Type_toString(g.base) + "<" + Type_toString(g.generic) + ">"
      case FrameworkTypes.List => "java.util.List"
      case _ => "None"
    }

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
    * Code generator for reproducing the structure of the visitor pattern invocation for eval.
    */
  val evalGenerators:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression] = CodeGeneratorRegistry.merge[com.github.javaparser.ast.expr.Expression](
    CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, Lit] {
      case (_:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], _:Lit) =>
        Java(s"""e.getValue()""").expression[com.github.javaparser.ast.expr.Expression]
    },

    CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, BinaryExp] {
      case (_:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], binary:BinaryExp) =>
        Java(s"""e.getLeft().accept(this) ${getBinaryOperator(binary).asString} e.getRight().accept(this)""").expression[com.github.javaparser.ast.expr.Expression]
    },

    CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, UnaryExp] {
      case (_:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], unary:UnaryExp) =>
        Java(s"""${getUnaryOperator(unary).asString} e.getExp().accept(this)""").expression[com.github.javaparser.ast.expr.Expression]()
    },
  )

  /**
    * Code generator for building up the structure of the expression using classes
    *
    * new BinaryExp(new Add, new Lit(new Lit, 1), new Lit(new Lit, 2))  -->
    *
    *   Add add = new Add(new Lit(1), new Lit(2));
    *
    *    // ( ( 5 * 7 ) + ( 8 / 9 ) )
    *
    * new BinaryExp(new Add(),
            new expression.instances.BinaryExp(new Mult(),
                new expression.instances.Lit(new Lit(),5),
                new expression.instances.Lit(new Lit(),7))
            new expression.instances.BinaryExp(new Divd(),
                new expression.instances.Lit(new Lit(),8),
                new expression.instances.Lit(new Lit(),9));
    */
  val instanceGenerators:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression] = CodeGeneratorRegistry.merge[com.github.javaparser.ast.expr.Expression](
    CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, expression.instances.Lit] {
      case (_:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], lit:expression.instances.Lit) =>
        Java(s"""new Lit(Double.valueOf(${lit.value}))""").expression[com.github.javaparser.ast.expr.Expression]
    },

    CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, expression.instances.BinaryExp] {
      case (registry:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], binary:expression.instances.BinaryExp) => {
        val Type:String = binary.self.getClass.getSimpleName
        val left:Option[com.github.javaparser.ast.expr.Expression] = registry(binary.left)
        val right:Option[com.github.javaparser.ast.expr.Expression] = registry(binary.right)
        if (left.isDefined && right.isDefined) {
          Java(s"""new $Type(${left.get}, ${right.get})""").expression[com.github.javaparser.ast.expr.Expression]
        } else {
          Java("""false""").expression[com.github.javaparser.ast.expr.Expression]
        }
      }
    },

    CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, expression.instances.UnaryExp] {
      case (registry:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], unary:expression.instances.UnaryExp) => {
        val Type:String = unary.self.getClass.getSimpleName
        val inner:Option[com.github.javaparser.ast.expr.Expression] = registry(unary.exp)
        if (inner.isDefined) {
          Java(s"""new $Type(${inner.get})""").expression[com.github.javaparser.ast.expr.Expression]
        } else {
          Java("false").expression[com.github.javaparser.ast.expr.Expression]
        }
      }
    },
  )

  /**
    * Code generator for reproducing the structure of the visitor pattern invocation for prettyP.
    */
  val prettypGenerators:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression] = CodeGeneratorRegistry.merge[com.github.javaparser.ast.expr.Expression](
    CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, Lit] {
      case (_:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], _:Lit) =>
        Java(s"""|"" + e.getValue() + ""
                 |""".stripMargin).expression[com.github.javaparser.ast.expr.Expression]
    },

    CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, BinaryExp] {
      case (_:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], binary:BinaryExp) =>
        Java(
          s"""|"(" + e.getLeft().accept(this) + "${getBinaryOperator(binary).asString}" + e.getRight().accept(this) + ")"
              |""".stripMargin).expression[com.github.javaparser.ast.expr.Expression]
    },

    CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, UnaryExp] {
      case (_:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], unary:UnaryExp) => {
        Java(
          s"""|"${getUnaryOperator(unary).asString}" + e.getExp().accept(this)
              |""".stripMargin).expression[com.github.javaparser.ast.expr.Expression]
      }
    },
  )

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
