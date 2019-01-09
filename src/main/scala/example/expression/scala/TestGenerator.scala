package example.expression.scala    /*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}

import scala.meta.{Stat, Term}

trait TestGenerator extends ScalaGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  /** Takes extra parameters and expands them. */
  def expand(tpe:TypeRep, value:Any) : Term = {
    tpe match {
      case domain.baseTypeRep => convert(value.asInstanceOf[AtomicInst])
      case _ => Scala(value.toString).term()
    }
  }

  /**
    * Return properly formatted expected value as a string.
    *
    * The goal of this method is to return a code fragment that effectively contains the
    * expected value of an operation on a specific instance. Note this is not as simple
    * as being a string value (from a toString invocation). For example, with the AsTree
    * operation, we want to ensure that the result of calling AsTree (a tree.Tree) is the
    * same as the expected value, which is a code fragment that effectively calls asTree
    * on another instance.
    *
    */
  def expected(test:TestCaseExpectedValue, id:String) : (Term => Stat) => Stat = continue => {
    val expectedType:Type = typeConverter(test.expect._1)
    val baseType:Type = Scala("AtomicInst").tpe()
    val baseName:Type = Scala(domain.baseTypeRep.name).tpe()

    if (expectedType.toString().equals(baseName.toString())) {   // Type doesn't seem to support .equals check
      val converted:Expression = convert(test.expect._2.asInstanceOf[AtomicInst])
      continue(converted)
    } else {

      val converted:Expression = test.expect._2 match {
        case ai:AtomicInst => convert(ai)
        case _ => Scala(test.expect._2.toString).term()
      }

      continue(converted)
    }
  }

  /** Actual value in a test case. */
  def actual(op:Operation, inst:AtomicInst, terms:Term*):Expression = dispatch(convert(inst), op, terms : _*)

  /** Convert a test instance into a Java Expression for instantiating that instance. */
  def convert(inst: AtomicInst): Expression = {
    val name = inst.e.name
    inst match {
      case ui: UnaryInst =>
        Scala(s"new $name(${convert(ui.inner)})").expression()
      case bi: BinaryInst =>
        val left = convert(bi.left)
        val right = convert(bi.right)
        Scala(s"new $name($left, $right)").expression()
      case exp: AtomicInst => Scala(s"new $name(${exp.i.get})").expression()

      case _ => Scala(s""" "unknown $name" """).expression()
    }
  }

  /** Return sample test cases as methods. */
  def testGenerator: Seq[Stat] = Seq.empty

  /** Return MethodDeclaration associated with given test cases. */
  def testMethod(tests:Seq[TestCase]) : Stat = {

    val stmts:Seq[scala.meta.Stat] = tests.zipWithIndex.map(pair => {
      val test = pair._1
      val idx = pair._2

      val id:String = s"v$idx"

      // The expected method takes in a function that will be called by the expected method. Now, the expected
      // method will pass in the expression (which is expected) into this function, and it is the job of that
      // function to return the variable.
      test match {
        case eq:EqualsTestCase => {
          val params = eq.params.map(pair => expand(pair._1, pair._2))
          expected(eq, id)(expectedExpr => Scala(s"assert ($expectedExpr == ${actual(eq.op, eq.inst, params: _*)})").statement())
        }

        case comp:EqualsCompositeTestCase => {
          val params = comp.params.map(pair => expand(pair._1, pair._2))

          val x :Expression = actual(comp.ops.head, comp.inst, params: _*)   // HACK: Only works for two-deep
          val y :Expression = dispatch(x, comp.ops.tail.head)

          expected(comp, id)(expectedExpr => Scala(s"assert ($expectedExpr == $y)").statement())
        }

        case ne:NotEqualsTestCase =>
          val params = ne.params.map(pair => expand(pair._1, pair._2))
          expected(ne, id)(expectedExpr => Scala(s"assert ($expectedExpr != ${actual(ne.op, ne.inst, params: _*)})").statement())
      }
    })

    Scala(s"""
            |def test() : Unit = {
            |   ${stmts.mkString("\n")}
            |}""".stripMargin).declaration()
  }
}
