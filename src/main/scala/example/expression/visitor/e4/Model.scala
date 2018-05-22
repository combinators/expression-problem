package example.expression.visitor.e4

import com.github.javaparser.ast.stmt.Statement
import expression.{Exp, Operation}
import expression.data.{Add, Lit}
import expression.extensions._
import expression.operations.SimplifyExpr
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

trait Model {

  var codeGenerator:CodeGeneratorRegistry[Seq[Statement]]

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:Collect, _:Lit)) =>
      Java(s"""|java.util.List<Double> list = new java.util.ArrayList<Double>();
               |list.add(e.getValue());
               |return list;
               |""".stripMargin).statements()
  })

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:Collect, _:Lit)) =>
      Java(s""" "" + e.getValue() + "" """).statements()
  })

  val collectStatements:Seq[Statement] = Java(s"""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                                   |list.addAll(e.getLeft().accept(this));
                                   |list.addAll(e.getRight().accept(this));
                                   |return list;
                                   |""".stripMargin).statements()

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:Collect, _:Add)) => collectStatements })

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:Collect, _:Sub)) => collectStatements })

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:Collect, _:Mult)) => collectStatements })

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:Collect, _:Divd)) => collectStatements })

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:Collect, _:Neg)) =>
      Java(s"""|java.util.List<Double> list = new java.util.ArrayList<Double>();
               |list.addAll(e.getExp().accept(this));
               |return list;""".stripMargin).statements()
  })


  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:SimplifyExpr, _:Lit)) =>
      Java(s"""return e;""").statements()
  })

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:SimplifyExpr, _:Add)) =>
      Java(s"""
              |double leftVal = e.getLeft().accept(new Eval());
              |double rightVal = e.getRight().accept(new Eval());
              |if ((leftVal == 0 && rightVal == 0) || (leftVal + rightVal == 0)) {
              |  return new Lit(0.0);
              |} else if (leftVal == 0) {
              |  return e.getRight().accept(this);
              |} else if (rightVal == 0) {
              |  return e.getLeft().accept(this);
              |} else {
              |  return new Add(e.getLeft().accept(this), e.getRight().accept(this));
              |}
              |""".stripMargin).statements()
  })

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:SimplifyExpr, _:Sub)) =>
      Java(s"""
              |if (e.getLeft().accept(new Eval()).equals(e.getRight().accept(new Eval()))) {
              |  return new Lit(0.0);
              |} else {
              |  return new Sub(e.getLeft().accept(this), e.getRight().accept(this));
              |}
              |""".stripMargin).statements()
  })

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:SimplifyExpr, _:Neg)) =>
      Java(s"""
              |if (e.getExp().accept(new Eval()) == 0) {
              |  return new Lit(0.0);
              |} else {
              |  return e;
              |}""".stripMargin).statements()
  })

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:SimplifyExpr, _:Mult)) =>
      Java(s"""
              |double leftVal = e.getLeft().accept(new Eval());
              |double rightVal = e.getRight().accept(new Eval());
              |if (leftVal == 0 || rightVal == 0) {
              |  return new Lit(0.0);
              |} else if (leftVal == 1) {
              |  return e.getRight().accept(this);
              |} else if (rightVal == 1) {
              |  return e.getLeft().accept(this);
              |} else {
              |  return new Mult(e.getLeft().accept(this), e.getRight().accept(this));
              |}
              |""".stripMargin).statements()
  })

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:SimplifyExpr, _:Divd)) =>
      Java(s"""
              |double leftVal = e.getLeft().accept(new Eval());
              |double rightVal = e.getRight().accept(new Eval());
              |if (leftVal == 0) {
              |  return new Lit(0.0);
              |} else if (rightVal == 1) {
              |  return e.getLeft().accept(this);
              |} else if (leftVal == rightVal) {
              |  return new Lit(1.0);
              |} else if (leftVal == -rightVal) {
              |  return new Lit(-1.0);
              |} else {
              |  return new Divd(e.getLeft().accept(this), e.getRight().accept(this));
              |}
              |""".stripMargin).statements()
  })

}
