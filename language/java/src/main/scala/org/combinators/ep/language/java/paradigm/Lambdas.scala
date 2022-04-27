package org.combinators.ep.language.java.paradigm

import com.github.javaparser.ast.stmt.ReturnStmt
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.control.{Lambdas => Lams, _}
import org.combinators.ep.language.java.{CtorCtxt, MethodBodyCtxt}

trait Lambdas[Ctxt, AP <: AnyParadigm] extends Lams[Ctxt] {

  val base: AP
  import base.syntax._
  val manip: Imperative.BlockContextManipulator[Ctxt]

  object lambdaCapabilities extends LambdaCapabilities {
    implicit val canLambda: Understands[Ctxt, Lambda[Name, Type, Ctxt, Expression]] =
      new Understands[Ctxt, Lambda[Name, Type, Ctxt, Expression]] {
        def perform(context: Ctxt, command: Lambda[Name, Type, Ctxt, Expression]): (Ctxt, Expression) = {
          val params = command.variables
          val lambdaExpr = new com.github.javaparser.ast.expr.LambdaExpr()
          
          params.foreach { case (paramName, paramTpe) =>
            lambdaExpr.addParameter(paramTpe.clone(), paramName.mangled)
          }
          val paramMap = params.map {
            case(name, tpe) => (name, new com.github.javaparser.ast.expr.NameExpr(name.toAST))
          }.toMap
          val (lambdaCtxt, bodyExp) = Command.runGenerator(command.body(paramMap), manip.nextBlockContext(context))
          val body = manip.getBlock(lambdaCtxt)
          body.addStatement(new ReturnStmt(bodyExp))
          lambdaExpr.setBody(body)
          (manip.copyWithBlock(lambdaCtxt, manip.getBlock(context)), lambdaExpr)
        }
      }
  }
}

object Lambdas {
  def inMethodContext[AP <: AnyParadigm](base: AP): Lambdas[MethodBodyCtxt, base.type] = {
    val b: base.type = base
    new Lambdas[MethodBodyCtxt, b.type] {
      val base: b.type = b
      val manip: Imperative.MethodBlockContextManipulator.type = Imperative.MethodBlockContextManipulator
    }
  }

  def inConstructorContext[AP <: AnyParadigm](base: AP): Lambdas[CtorCtxt, base.type] = {
    val b: base.type = base
    new Lambdas[CtorCtxt, b.type] {
      val base: b.type = b
      val manip: Imperative.CtorBlockContextManipulator.type = Imperative.CtorBlockContextManipulator
    }
  }
}