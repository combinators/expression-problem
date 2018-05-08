package example.expression.covariant

import com.github.javaparser.ast.stmt.Statement
import expression.data._
import expression.{DomainModel, Operation}
import expression.extensions.{Collect, PrettyP, SimplifyBase}
import expression.instances.UnitTest
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

import scala.collection.JavaConverters._
/**
  * Common code generators for the covariant solution.
  *
  * Customized to know about the mode recent model domain BUT ALSO the expression to be used
  * when synthesizing the code. The reason is that the test cases need to know which objects to
  * instantiate, based upon the desired operations. Here is sample code from the covariant repository.
  * Note how the instantiated classes change based upon the desired data types AND operations.

    System.out.println("======Add======");
    Add add = new AddFinal(new LitFinal(7), new LitFinal(4));
    System.out.println(add.eval());
    System.out.println("======Sub======");
    Sub sub = new SubFinal(new LitFinal(7), new LitFinal(4));
    System.out.println(sub.eval());
    System.out.println("======Print======");

    /* the line below causes compile-time error, if now commented out. */
    //AddPrettyPFinal exp = new AddPrettyPFinal(new LitFinal(7)), new LitFinal(4));
    AddPrettyPFinal prt = new AddPrettyPFinal(new LitPrettyPFinal(7), new LitPrettyPFinal(4));
    System.out.println(prt.print() + " = " + prt.eval());
    System.out.println("======CollectLiterals======");
    AddCollectFinal addc = new AddCollectFinal(new LitCollectFinal(3), new LitCollectFinal(4));
    System.out.println(addc.collectList().toString());
    System.out.println("======Composition: Independent Extensibility======");
    AddPrettyPCollectFinal addpc = new AddPrettyPCollectFinal(new LitPrettyPCollectFinal(3), new LitPrettyPCollectFinal(4));
    System.out.println(addpc.print() + " = " + addpc.eval() + " Literals: " + addpc.collectList().toString());
  }

  Have to find the most recent class in the evolution hierarchy.
  */
class TestCaseCodeGenerators(model:DomainModel, expr:expression.instances.Expression)  {

  var resultNumber:Integer = 0

  var subTypes:String = null

  /**
    * When an expression requires operations o1, o2, then the subtypes must be instantiated from the
    * most specific subclasses available, derived from the operations to be required of the expr
    *
    * @return
    */
  def computeSubTypes() : String = {
    if (subTypes == null) {
      var ops: List[String] = List.empty
      for (ut: UnitTest <- expr.asScala) {

        // ignore Eval, which is assumed to always be there
        if (!ut.op.getClass.getSimpleName.equals("Eval")) {
          ops = ops :+ ut.op.getClass.getSimpleName
        }
      }

      subTypes = ops.sortWith(_ < _).mkString("")
    }

    subTypes
  }


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
      case (_:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], lit:expression.instances.Lit) => {
        // must choose the most specific sub-types available, depending upon the operations
        val subTypes:String = computeSubTypes()
        Java(s"""new Lit${subTypes}Final(${lit.value})""").expression[com.github.javaparser.ast.expr.Expression]
      }
    },

    CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, expression.instances.BinaryExp] {
      case (registry:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], binary:expression.instances.BinaryExp) => {
        val Type:String = binary.self.getClass.getSimpleName
        val left:Option[com.github.javaparser.ast.expr.Expression] = registry(binary.left)
        val right:Option[com.github.javaparser.ast.expr.Expression] = registry(binary.right)
        val subTypes:String = computeSubTypes()
        if (left.isDefined && right.isDefined) {
          Java(s"""new ${Type}${subTypes}Final(${left.get}, ${right.get})""").expression[com.github.javaparser.ast.expr.Expression]
        } else {
          Java("""false""").expression[com.github.javaparser.ast.expr.Expression]
        }
      }
    },

    CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, expression.instances.UnaryExp] {
      case (registry:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], unary:expression.instances.UnaryExp) => {
        val Type:String = unary.self.getClass.getSimpleName
        val inner:Option[com.github.javaparser.ast.expr.Expression] = registry(unary.exp)
        val subTypes:String = computeSubTypes()
        if (inner.isDefined) {
          Java(s"""new ${Type}${subTypes}Final(${inner.get})""").expression[com.github.javaparser.ast.expr.Expression]
        } else {
          Java("false").expression[com.github.javaparser.ast.expr.Expression]
        }
      }
    },
  )

  /**
    * Code generator for generating test cases from Operations.
    *
    * Note this needs to be made extensible (ala solitaire extensions of base constraints)
    *
    * Not easy to get this working since we need to pattern match on the Eval, but we need to also pass
    * in parameters. Must think some more...
    */
    val testCaseGenerator:CodeGeneratorRegistry[Seq[Statement]] = CodeGeneratorRegistry.merge[Seq[Statement]](
      CodeGeneratorRegistry[Seq[Statement], Eval] {
        case (_:CodeGeneratorRegistry[Seq[Statement]], _:Eval) => {
          val blocks:Seq[UnitTest] = expr.iterator().asScala.toSeq
          val expected:String = "null"

          // grab all UnitTests to get the operations tha are needed
          resultNumber = resultNumber + 1

          Java(s"""|  Double result$resultNumber = (Double) 7.4);
                   |  assertEquals(${expected.toString}, result$resultNumber.doubleValue());
                   |""".stripMargin).statements()
        }
      },

      CodeGeneratorRegistry[Seq[Statement], PrettyP] {
        case (_:CodeGeneratorRegistry[Seq[Statement]], _:PrettyP) =>
          Java(s"""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                   |list.addAll(e.getLeft().accept(this));
                   |list.addAll(e.getRight().accept(this));
                   |return list;
                   |""".stripMargin).statements()
      },

      CodeGeneratorRegistry[Seq[Statement], SimplifyBase] {
        case (_:CodeGeneratorRegistry[Seq[Statement]], _:SimplifyBase) => {
          Java(s"""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                   |list.addAll(e.getExp().accept(this));
                   |return list;""".stripMargin).statements()
        }
      },

      CodeGeneratorRegistry[Seq[Statement], Collect] {
        case (_:CodeGeneratorRegistry[Seq[Statement]], _:Collect) => {
          Java(s"""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                   |list.addAll(e.getExp().accept(this));
                   |return list;""".stripMargin).statements()
        }
      },
    )

}
