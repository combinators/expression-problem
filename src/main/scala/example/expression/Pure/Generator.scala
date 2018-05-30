package example.expression.Pure

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.stmt.Statement
import org.combinators.templating.twirl.Java


/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait AbstractGenerator {
  val pure:Pure
  import pure._

  /** Return designated Java type associated with type, or void if all else fails. */
  def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case types.Exp => Java("Straight").tpe()
      case _ => Java ("void").tpe()  // reasonable stop
    }
  }

  def methodGenerator(exp:expressions.Exp)(op:Operation): MethodDeclaration = {
    val name = exp.toString

    val retType = op.returnType match {
      case Some(tpe) => typeGenerator(tpe)
      case _ => Java("void").tpe
    }

    val str:String =
      s"""
         |public $retType ${op.name}() {
         |  ${methodBodyGenerator(exp)(op).mkString("\n")}
         |}""".stripMargin
    println(str)

    Java(str).methodDeclarations().head
  }

  def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = ???

  /** Return sample JUnit test cases. */
  def testGenerator(num:Int): Seq[MethodDeclaration] = Seq.empty

  def generateSuite(): CompilationUnit = {
    val methods:Seq[MethodDeclaration] = testGenerator(1)

    Java(s"""|import junit.framework.TestCase;
             |public class TestSuite extends TestCase {
             |    ${methods.mkString("\n")}
             |}""".stripMargin).compilationUnit()
  }

  def generateExp(domain:Model, e:expressions.Exp) : CompilationUnit = {
    val name = e.toString

    val methods:Seq[MethodDeclaration] = domain.ops.map(methodGenerator(e))
    val atts:Seq[FieldDeclaration] = e.attributes.flatMap(att => Java(s"private ${typeGenerator(att.tpe)} ${att.name};").fieldDeclarations())

    // Builds up the attribute fields and set/get methods. Also prepares for one-line constructor.
    var params:Seq[String] = Seq.empty
    var cons:Seq[String] = Seq.empty

    val fields:Seq[FieldDeclaration] = e.attributes.map(att => {
      val capAtt = att.name.capitalize
      val tpe = typeGenerator(att.tpe)

      params = params :+ s"$tpe ${att.name}"
      cons   = cons   :+ s"  this.${att.name} = ${att.name};"

      Java(s"private $tpe ${att.name};").fieldDeclarations().head
    })

    val constructor = Java(s"""
                              |public $name (${params.mkString(",")}) {
                              |   ${cons.mkString("\n")}
                              |}""".stripMargin).constructors().head

    val str:String = s"""
                        |public class $name extends Straight {
                        |
                        |  ${constructor.toString}
                        |
                        |  ${atts.mkString("\n")}
                        |
                        |  ${methods.mkString("\n")}
                        |}""".stripMargin
    println(">>>>>" + str)
    Java(str).compilationUnit()
  }

  def generateBase(domain:Model): CompilationUnit = {

    // only grab those operations which have a return type defined
    val signatures:Seq[MethodDeclaration] = domain.ops.map(op => {

      val ret = if (op.returnType.isDefined) {
        typeGenerator(op.returnType.get)
      } else {
        "void"
      }

      Java(s"public abstract $ret ${op.name}();").methodDeclarations().head
    })

    // same every time
    Java(s"""|public abstract class Straight {
             |  ${signatures.mkString("\n")}
             |}""".stripMargin).compilationUnit()
  }
}

trait E0_Generator extends AbstractGenerator {
  import pure._

  abstract override def testGenerator(num:Int): Seq[MethodDeclaration] = {
    super.testGenerator(num+1) ++ Java(
      s"""
         |public void test$num() {
         |   Straight exp1 = new Add(new Lit(1.0), new Lit(2.0));
         |   assertEquals(3.0, exp1.eval());
         |
         |   Lit lit1 = new Lit(3);
         |   assertEquals(3.0, lit1.eval());
         |}""".stripMargin).methodDeclarations()
  }

  abstract override def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case Double => Java("double").tpe()
      case _ => super.typeGenerator(tpe)
    }
  }

  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      case Eval => {
        exp match {
          case Lit => Java(s"return value;").statements
          case Add => Java(s"return left.eval() + right.eval();").statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }
      }

      case _ => super.methodBodyGenerator(exp)(op)
    }
  }
}


trait E1_Generator extends AbstractGenerator {
  import pure._

  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      case Eval => {
        exp match {
          case Sub => Java(s"return left.eval() - right.eval();").statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }
      }

      case _ => super.methodBodyGenerator(exp)(op)
    }
  }

  abstract override def testGenerator(num:Int): Seq[MethodDeclaration] = {
    super.testGenerator(num+1) ++ Java(
      s"""
         |public void test$num() {
         |   Straight exp1 = new Sub(new Lit(1.0), new Lit(2.0));
         |   assertEquals(-1.0, exp1.eval());
         |}""".stripMargin).methodDeclarations()
  }
}

trait E2_Generator extends AbstractGenerator {
  import pure._

  abstract override def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case String => Java("String").tpe()
      case _ => super.typeGenerator(tpe)
    }
  }

  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      case PrettyP => {
        exp match {
          case Lit => Java(s"""return "" + value + ""; """).statements()
          case Add => Java(s"""return "(" + left.print() + "+" + right.print() + ")";""").statements()
          case Sub => Java(s"""return "(" + left.print() + "-" + right.print() + ")";""").statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }
      }

      case _ => super.methodBodyGenerator(exp)(op)
    }
  }

  abstract override def testGenerator(num:Int): Seq[MethodDeclaration] = {
    super.testGenerator(num+1) ++ Java(
      s"""
         |public void test$num() {
         |   Straight exp1 = new Sub(new Lit(1.0), new Lit(2.0));
         |   assertEquals("(1.0-2.0)", exp1.print());
         |
         |   Straight exp2 = new Add(new Sub(new Lit(1.0), new Lit(2.0)), new Add(new Lit(5.0), new Lit(6.0)));
         |   assertEquals("((1.0-2.0)+(5.0+6.0))", exp2.print());
         |}""".stripMargin).methodDeclarations()
  }
}

trait E3_Generator extends AbstractGenerator {
  import pure._

  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      case PrettyP => {
        exp match {
          case Neg => Java(s"""return "-" + exp.print(); """).statements()
          case Mult => Java(s"""return "(" + left.print() + "*" + right.print() + ")";""").statements()
          case Divd => Java(s"""return "(" + left.print() + "/" + right.print() + ")";""").statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }
      }

      case Eval => {
        exp match {
          case Neg => Java(s"""return - exp.eval(); """).statements()
          case Mult => Java(s"""return left.eval() * right.eval();""").statements()
          case Divd => Java(s"""return left.eval() / right.eval();""").statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }
      }
      case _ => super.methodBodyGenerator(exp)(op)
    }
  }

  abstract override def testGenerator(num:Int): Seq[MethodDeclaration] = {
    super.testGenerator(num+1) ++ Java(
      s"""
         |public void test$num() {
         |   Straight exp1 = new Neg(new Lit(1.0));
         |   assertEquals("-1.0", exp1.print());
         |   assertEquals(-1.0, exp1.eval());
         |
         |   Straight exp2 = new Mult(new Divd(new Lit(5.0), new Lit(2.0)), new Lit(4.0));
         |   assertEquals("((5.0/2.0)*4.0)", exp2.print());
         |}""".stripMargin).methodDeclarations()
  }
}