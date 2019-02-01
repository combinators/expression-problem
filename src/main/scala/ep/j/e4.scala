package ep.j  /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import ep.domain._
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e4 extends Evolution with JavaGenerator with JUnitTestGenerator with OperationDependency with M0 with M1 with M2 with M3 with M4 {
  self:e0 with e1 with e2 with e3 =>
  val domain:MathDomain

  /**
    * List can be accommodated (in Java) by populating ArrayList with values drawn from test case.
    *
    * Calls 'continue' with an expression (the result of the prior new statements) and just concatenates all statements
    */
   override def expected(test:domain.TestCaseExpectedValue, id:String) : (Expression => Seq[Statement]) => Seq[Statement] = continue => {
     test.expect._1 match {   // HACK: NOT sure if this works.
      case list:List =>
        val seq: Seq[Any] = test.expect._2.asInstanceOf[Seq[Any]]
        val jtype = Java(typeConverter(list)).tpe
        val inner: Type = jtype.asClassOrInterfaceType().getTypeArguments.get.get(0)

        val map = seq.map(elt => s"result$id.add($elt);")
        val str = s"""
                     |$jtype result$id = new java.util.ArrayList<$inner>();
                     |${map.mkString("\n")}
                     |${continue(Java(s"result$id").expression[Expression]).mkString("\n")}
             """.stripMargin
        Java(str).statements

      case _ => super.expected(test,id)(continue)
    }
  }

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case Simplify => scala.List[domain.Operation](PrettyP, Eval)
      case _ => super.dependency(op)
    }
  }

  abstract override def typeConverter(tpe:domain.TypeRep) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case el:List => Java(s"java.util.List<${typeConverter(el.generic)}>").tpe()
      case _ => super.typeConverter(tpe)
    }
  }

  abstract override def logic(exp:domain.Atomic, op:domain.Operation): Seq[Statement] = {
    val zero = Java("0.0").expression[Expression]()
    val one = Java("1.0").expression[Expression]()
    val negOne = Java("-1.0").expression[Expression]()

    // generate the actual body
    val source = Source(exp,op)
    op match {
        // Simplify only works for solutions that instantiate expression instances
      case Simplify =>

        exp match {
          case Lit => result(Java(s" ${inst(Lit, expression(exp, litValue))}").expression[Expression]())
          case Add =>
            val deltaLeft = deltaChildOp(source, domain.base.left, Eval)
            val deltaRight = deltaChildOp(source, domain.base.right, Eval)
            Java(s"""|double leftVal = ${contextDispatch(source, deltaLeft)};
                     |double rightVal = ${contextDispatch(source, deltaRight)};
                     |if ((leftVal == 0 && rightVal == 0) || (leftVal + rightVal == 0)) {
                     |   ${result(inst(Lit, zero)).mkString("\n")}
                     |} else if (leftVal == 0) {
                     |   ${result(dispatch(expression(exp, domain.base.right), Simplify)).mkString("\n")}
                     |} else if (rightVal == 0) {
                     |   ${result(dispatch(expression(exp, domain.base.left), Simplify)).mkString("\n")}
                     |} else {
                     |   ${result(inst(Add, dispatch(expression(exp, domain.base.left), Simplify),dispatch(expression(exp, domain.base.right), Simplify))).mkString("\n")}
                     |}""".stripMargin).statements()
          case Sub =>
            val deltaLeft = deltaChildOp(source, domain.base.left, Eval)
            val deltaRight = deltaChildOp(source, domain.base.right, Eval)
            Java(s"""|if (${contextDispatch(source, deltaLeft)} == ${contextDispatch(source, deltaRight)}) {
                     |   ${result(inst(Lit, zero)).mkString("\n")}
                     |} else {
                     |   ${result(inst(Sub, dispatch(expression(exp, domain.base.left), Simplify),dispatch(expression(exp, domain.base.right), Simplify))).mkString("\n")}
                     |}""".stripMargin).statements()
          case Mult =>
            val deltaLeft = deltaChildOp(source, domain.base.left, Eval)
            val deltaRight = deltaChildOp(source, domain.base.right, Eval)
            Java(s"""|double leftVal = ${contextDispatch(source, deltaLeft)};
                     |double rightVal = ${contextDispatch(source, deltaRight)};
                     |if (leftVal == 0 || rightVal == 0) {
                     |   ${result(inst(Lit, zero)).mkString("\n")}
                     |} else if (leftVal == 1) {
                     |   ${result(dispatch(expression(exp, domain.base.right), Simplify)).mkString("\n")}
                     |} else if (rightVal == 1) {
                     |   ${result(dispatch(expression(exp, domain.base.left), Simplify)).mkString("\n")}
                     |} else {
                     |   ${result(inst(Mult, dispatch(expression(exp, domain.base.left), Simplify),dispatch(expression(exp, domain.base.right), Simplify))).mkString("\n")}
                     |}
                     |""".stripMargin).statements()
          case Divd =>
            val deltaLeft = deltaChildOp(source, domain.base.left, Eval)
            val deltaRight = deltaChildOp(source, domain.base.right, Eval)
            Java(s"""|double leftVal = ${contextDispatch(source, deltaLeft)};
                     |double rightVal = ${contextDispatch(source, deltaRight)};
                     |if (leftVal == 0) {
                     |   ${result(inst(Lit, zero)).mkString("\n")}
                     |} else if (rightVal == 1) {
                     |   ${result(dispatch(expression(exp, domain.base.left), Simplify)).mkString("\n")}
                     |} else if (leftVal == rightVal) {
                     |   ${result(inst(Lit, one)).mkString("\n")}
                     |} else if (leftVal == -rightVal) {
                     |   ${result(inst(Lit, negOne)).mkString("\n")}
                     |} else {
                     |   ${result(inst(Divd, dispatch(expression(exp, domain.base.left), Simplify),dispatch(expression(exp, domain.base.right), Simplify))).mkString("\n")}
                     |}
                     |""".stripMargin).statements()
            // TODO: Would love to have ability to simplify neg(neg(x)) to just be x. This requires a form
            // of inspection that might not be generalizable...
          case Neg =>
            val deltaInner = deltaChildOp(source, domain.base.inner, Eval)
            Java(s"""
                    |if (${contextDispatch(source, deltaInner)} == 0) {
                    |   ${result(inst(Lit, zero)).mkString("\n")}
                    |} else {
                    |   ${result(inst(Neg, dispatch(expression(exp, domain.base.inner), Simplify))).mkString("\n")}
                    |}""".stripMargin).statements()
          case _ => super.logic(exp, op)
        }

      case Collect =>
        val returnList = result(Java("list").expression[Expression]()).mkString("\n")
        exp match {
          case _:domain.Binary => Java(
            s"""|${typeConverter(List(Double))} list = ${dispatch(expression(exp, domain.base.left), Collect)};
                |list.addAll(${dispatch(expression(exp, domain.base.right), Collect)});
                |$returnList
                |""".stripMargin).statements()

          case _:domain.Unary  => Java(
            s"""|${typeConverter(List(Double))} list = new java.util.ArrayList<Double>();
                |list.addAll(${dispatch(expression(exp, domain.base.inner), Collect)});
                |$returnList
                |""".stripMargin).statements()

          case _:domain.Atomic => Java(
            s"""|${typeConverter(List(Double))} list = new java.util.ArrayList<Double>();
                |list.add(${expression(exp, litValue)});
                |$returnList
                |""".stripMargin).statements()

          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
     if (getModel.supports(Simplify)) {
       super.testGenerator ++ testMethod(M4_tests) ++ testMethod(M4_simplify_tests)
    } else {
      super.testGenerator ++ testMethod(M4_tests)
    }
  }
}