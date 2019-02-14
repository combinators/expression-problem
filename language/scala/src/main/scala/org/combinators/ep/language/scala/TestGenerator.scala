package org.combinators.ep.language.scala     /*DI:LD:AI*/

import org.combinators.ep.domain.{BaseDomain, ModelDomain}

import scala.meta.{Stat, Term}

trait TestGenerator extends ScalaGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  /** Type to use when referring to specific instance. */
  def exprDefine(exp:AtomicInst) : Type = {
    scala.meta.Type.Name(exp.e.name)
  }

  /** Used when one already has code fragments bound to variables, which are to be used for left and right. */
  def convertRecursive(inst: Binary, left:String, right:String): Expression = {
    val name = inst.name
    Scala(s"new $name($left, $right)").expression
  }

   /** Takes extra parameters and expands them. */
  def expand(tpe:TypeRep, value:Any) : Term = {
    tpe match {
      case domain.baseTypeRep => convert(value.asInstanceOf[AtomicInst])
      case _ => Scala(value.toString).term
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
  def expected(test:TestCaseExpectedValue, id:String) : (Term => Seq[Stat]) => Seq[Stat] = continue => {
    val expectedType:Type = typeConverter(test.expect.tpe)
    val baseName:Type = Scala(domain.baseTypeRep.name).tpe

    if (expectedType.toString().equals(baseName.toString())) {   // Type doesn't seem to support .equals check
      val converted:Expression = convert(test.expect.inst.asInstanceOf[AtomicInst])
      continue(converted)
    } else {

      val converted:Expression = test.expect.inst match {
        case ai:AtomicInst => convert(ai)
        case _ => Scala(test.expect.inst.toString).term
      }

      continue(converted)
    }
  }

  /** Actual value in a test case. */
  def actual(op:Operation, inst:Inst, terms:Term*):Expression = dispatch(convert(inst), op, terms : _*)

  /** Convert a test instance into a Java Expression for instantiating that instance. */
  def convert(inst: Inst): Expression = {
    val name = inst.name
    inst match {
      case ui: UnaryInst =>
        Scala(s"new $name(${toTargetLanguage(ui.inner)})").expression
      case bi: BinaryInst =>
        val left = toTargetLanguage(bi.left)
        val right = toTargetLanguage(bi.right)
        Scala(s"new $name($left, $right)").expression
      case exp: AtomicInst => Scala(s"new $name(${exp.ei.inst})").expression

      case _ => Scala(s""" "unknown $name" """).expression
    }
  }

  /** Return sample test cases as methods. */
  def testGenerator: Seq[Seq[Stat]] = Seq.empty

  /** Performance tests. */
  def performanceMethod: Seq[Seq[Stat]] = Seq.empty

  /**
    * Traits can override this method to add their test cases to the mix.
    */
  def testMethod(tests:Seq[TestCase]) : Seq[Seq[Stat]] = {
    tests.zipWithIndex.map{ case (test, idx) => scalaTestMethod(test, idx) }
  }

  /** Return Sequence of statements associated with given test cases. */
  def scalaTestMethod(test:TestCase, idx:Int) : Seq[Stat] = {
      val id:String = s"v$idx"

      // The expected method takes in a function that will be called by the expected method. Now, the expected
      // method will pass in the expression (which is expected) into this function, and it is the job of that
      // function to return the variable.
      test match {
        case eq:EqualsTestCase =>
          val params = eq.params.map(pair => expand(pair.tpe, pair.inst))
          expected(eq, id)(expectedExpr => Seq(Scala(s"assert ($expectedExpr == ${actual(eq.op, eq.inst, params: _*)})").statement))

        case comp:EqualsCompositeTestCase =>
          val params = comp.params.map(pair => expand(pair._1, pair._2))

          // TODO: replicate in other languages
//          val x2 :Expression = actual(comp.ops.head, comp.inst, params: _*)
//          val y2 :Expression = dispatch(x2, comp.ops.tail.head)

          val start:Expression = actual(comp.ops.head, comp.inst, params: _*)
          val result:Expression = comp.ops.tail.foldLeft(start){case (state, next) => dispatch(state, next)}

          expected(comp, id)(expectedExpr => Seq(Scala(s"assert ($expectedExpr == $result)").statement))

        case ne:NotEqualsTestCase =>
          val params = ne.params.map(pair => expand(pair._1, pair._2))
          expected(ne, id)(expectedExpr => Seq(Scala(s"assert ($expectedExpr != ${actual(ne.op, ne.inst, params: _*)})").statement))
      }
    }
}
