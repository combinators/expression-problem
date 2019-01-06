package example.expression.j  /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import example.expression.domain.{Evolution, M0, M5, MathDomain, OperationDependency}
import org.combinators.templating.twirl.Java

/**
  * BinaryMethod capability
  *
  * Still Java-based, naturally and JUnit
  */
trait e5 extends Evolution with JavaGenerator with JUnitTestGenerator with OperationDependency with M0 with  M5 {
  self: e0 with e1 with e2 with e3 with e4 =>
  val domain:MathDomain
  import domain._


//  /**
//    * List can be accommodated (in Java) by populating ArrayList with values drawn from test case.
//    */
//  override def preAssert(test:domain.EqualsTestCase, id:String): Seq[Statement] = {
//    val tpe = test.op.returnType.get
//    tpe match {
//      case tree:Tree =>
//        val seq:Seq[Any] = test.expect._2.asInstanceOf[Seq[Any]]
//        val jtype = Java(typeConverter(list)).tpe
//        val inner:Type = jtype.asClassOrInterfaceType().getTypeArguments.get.get(0)
//
//        Java(s"$jtype list$id = ${dispatch(convert(test.inst), test.op)};").statements ++
//          Java(s"$jtype result$id = new java.util.ArrayList<$inner>();").statements ++
//          seq.map(elt => Java(s"result$id.add($elt);").statement) ++ Java(s"assertEquals (list$id, result$id);").statements
//
//      case _ => super.preAssert(test, id)
//    }
//  }
//
//  /** For developing test cases with lists, must convert expected value into a list using preAssert, and then just return result$id. */
//  override def expected(test:domain.EqualsTestCase, id:String): Expression = {
//    val tpe = test.op.returnType.get
//    tpe match {
//      case list:List =>
//        Java(s"result$id").expression[Expression]
//
//      case _ => super.expected(test, id)
//    }
//  }

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case domain.AsTree => scala.List[domain.Operation](Identifier)
      case _ => super.dependency(op)
    }
  }

  abstract override def typeConverter(tpe:domain.TypeRep) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.Tree => Java(s"tree.Tree").tpe()      // package class goes here.
      case _ => super.typeConverter(tpe)
    }
  }

  /** helper method for testing when goal is to check equality between tree objects . */
  def treesAreSame(treeExp1:Expression, treeExp2:Expression) : Expression = {
    Java(s"${treeExp1.toString}.same(${treeExp2.toString})").expression[Expression]()
  }

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      // Simplify only works for solutions that instantiate expression instances. As a binary
      case domain.AsTree => {
        val atts = subExpressions(exp)

        // TODO: replace hard-coded DefinedSubTypes with dependent operation getSubTypeIdentifier and dispatch accordingtly.

        // different strategies have different means of accessing attributes, either directly or via
        // getXXX methods. This logic method must defer that knowledge to later.
        // "this" is only valid expression when datatype as class
        exp match {   // was $litValue     ;
          case Lit =>   // ${exp.hashCode()}

            val attParams = atts.map(att => att._2.toString).mkString(",")
            Java(s"""return new tree.Node(java.util.Arrays.asList(new tree.Leaf($attParams)), ${delegate(exp, Identifier)}); """).statements

          case Add|Sub|Mult|Divd|Neg =>
            val params = atts.map(att => att._2.toString + ".astree()").mkString(",")
            Java(s""" return new tree.Node(java.util.Arrays.asList($params), ${delegate(exp, Identifier)} ); """).statements
          }
      }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testMethod(tests:Seq[domain.TestCase]) : MethodDeclaration = {

    // EXTRACT all SameTestCase ones and handle here
    var pass:Seq[domain.TestCase] = Seq.empty
    val local:Seq[domain.TestCase] = tests.filter(p => p match {
      case _:SameTestCase => true
      case _ => false
    })

    val stmts:Seq[Statement] = tests.zipWithIndex.flatMap(pair => {
      val test = pair._1
      val idx = pair._2

      val id:String = s"c$idx"

      test match {
        case ctc: SameTestCase =>
          // assertFalse(${dispatch(convert(m5_s1), domain.AsTree)}.same(${dispatch(convert(m5_s2), domain.AsTree)}));

          val tree1 = dispatch(convert(ctc.inst1), AsTree)
          val tree2 = dispatch(convert(ctc.inst2), AsTree)

          val same = Java(s"$tree1.same($tree2)").expression[Expression]()

          if (ctc.result) {
            Java(s"assertTrue($same);").statements
          } else {
            Java(s"assertFalse($same);").statements
          }
        case _ =>
          pass = pass :+ test
          Seq.empty
      }
    })

    // add these all in to what super produces
    addStatements(super.testMethod(pass), stmts)
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator :+ testMethod(M5_tests)
  }
}
