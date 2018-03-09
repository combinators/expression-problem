package example.expression.algebra

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.MethodDeclaration
import example.expression.ExpressionDomain
import expression._
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types._
import org.combinators.cls.types.syntax._
import org.combinators.templating.twirl.Java
import expression.data.Eval
import expression.types.Types

import scala.collection.JavaConverters._

/** Future work to sanitize combinators to be independent of Exp. */
class ExpressionSynthesis(override val domain:DomainModel) extends ExpressionDomain(domain) with SemanticTypes {

  /** Generate from domain. USER NEEDS TO SPECIFY THESE EITHER AUTOMATICALLY OR MANUALLY */
  class BaseInterface(op:Eval) {
    def apply() : CompilationUnit = {
      val name = op.name
      val iName = name.capitalize

      Java(s"""
         |package algebra;
         |
         |// The evaluation interface
         |interface $iName {
         |	int $name();
         |}
         |""".stripMargin).compilationUnit()
    }
    val semanticType:Type = ops(ops.base, new Eval)
  }
  @combinator object BaseExpClass {
    // have a domain object


    def apply(): CompilationUnit = {

      val signatures = domain.data.asScala
        .map(sub => {
          var params: Seq[String] = Seq.empty
          sub.ops.asScala.foreach {
            case att: Attribute =>
              val tpe = Type_toString(att.attType)
              params = params :+ s"$tpe ${att.attName}"
            case _ =>
          }
          s""" E ${sub.getClass.getSimpleName}(${params.mkString(",")});"""
        }).mkString("\n")

      Java(s"""package algebra;
              |
              |interface ExpAlg<E> {
              | $signatures
              |}
              |
              |""".

          stripMargin).compilationUnit()
}
    val semanticType:Type = exp(exp.base, new Exp)
  }

  class BaseClass(expr: Exp) {
    def apply(): CompilationUnit = {

      // build up a sequence of parameters right from the Expr definition
      var params: Seq[String] = Seq.empty
      expr.ops.asScala.foreach {
        case att: Attribute =>
          val tpe = Type_toString(att.attType)
          params = params :+ s"$tpe ${att.attName}"
        case _ =>
      }

      // comma-separate these to be proper syntax within method
      val paramList = params.mkString(",")

      val name = expr.getClass.getSimpleName
      val iName= expr.getClass.getSimpleName.capitalize
      Java(s"""package algebra; interface ${iName}ExpAlg extends ExpAlg<E>
           |{
           |        E $name($paramList);
           |}""".stripMargin).compilationUnit()
    }

    // semantic type is based on the subclass (i.e., it will be exp('Base, 'Lit) or exp('Base, 'Add)
    val semanticType:Type = exp(exp.algebra, expr)
  }
//  / The object algebra
//  // hint: .addJob[CompilationUnit](alg(alg.base))
//  class EvalExpAlg implements ExpAlg<Eval> {
//    public Eval lit(final int x) {
//      return new Eval() {
//        public int eval() {
//          return x;
//        }
//      };
//    }
//
//    public Eval add(final Eval e1, final Eval e2) {
//      return new Eval() {
//        public int eval() {
//          return e1.eval() + e2.eval();
//        }
//      };
//    }
//  }


  class OperationBaseClass(op:Operation) {
    def apply(unit:CompilationUnit): CompilationUnit= {
      // this gets "eval" and we want the name of the Interface.
      //val name = op.name
      val name = op.name.capitalize
      val returnType = op.`type`     // allows direct access to java field with reserved token name

      val methods = domain.data.asScala
        .map(sub => {  // sub is either 'lit' or 'add'
          val subName = sub.getClass.getSimpleName
          // build up sequence
          var params:Seq[String] = Seq.empty
          sub.ops.asScala.foreach {
            case att: Attribute =>
              val tpe = if (att.attType == Types.Exp) {
                  name
              } else {
                Type_toString(att.attType)
              }

              params = params :+ s"final $tpe ${att.attName}"
            case _ =>
          }

          // creates method body
          val paramList = params.mkString(",")
          s"""
             |public ${name} ${subName}(${paramList}) {
             |        return new ${name}() {
             |            public ${returnType} eval() {
             |                return this; // HACK must fix
             |            }
             |        };
             |    }
           """.stripMargin

        }).mkString("\n")


      Java(s"""package algebra;
            |
            |class ${name}ExpAlg implements ExpAlg<${name}> {
            |    $methods
            |}
            |""".stripMargin).compilationUnit()
  }

    val semanticType:Type =  ops(ops.base, new Eval) =>: ops (ops.algebra, new Eval)
  }

 // interface SubExpAlg<E> extends ExpAlg<E> {
  //  E sub(E e1, E e2);
  //}

  // Updating evaluation:
  //class EvalSubExpAlg extends EvalExpAlg implements SubExpAlg<Eval> {
   //public Eval sub(final Eval e1, final Eval e2) {
      //return new Eval() {
        //public int eval() {
          //return e1.eval() - e2.eval();
       // }
     // };
    //}
  //}











  class OpImpl(op:Operation) {
    def apply: CompilationUnit = {

      val name = op.getClass.getSimpleName
      val tpe = Type_toString(op.`type`)

      //implementations
      Java(s"""|package algebra;
               |interface $name {
               |  $tpe $name();
               |}""".stripMargin).compilationUnit()
    }

    val semanticType:Type = ops (ops.algebra,op)
  }
}


