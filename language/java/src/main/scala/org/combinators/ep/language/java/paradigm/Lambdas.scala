package org.combinators.ep.language.java.paradigm    /*DI:LD:AI*/

import org.combinators.cogen.paradigm.control.Lambdas as Lams
import org.combinators.cogen.{Command, Understands}
import org.combinators.cogen.paradigm.control.Lambda

trait Lambdas[Ctxt, AP <: AnyParadigm] extends Lams[Ctxt] {

  val base: AP
  val imperative: Imperative[Ctxt, AP]
  import base.syntax._

  object lambdaCapabilities extends LambdaCapabilities {
    implicit val canLambda: Understands[Ctxt, Lambda[Name, Type, Ctxt, Expression]] =
      new Understands[Ctxt, Lambda[Name, Type, Ctxt, Expression]] {
        def perform(context: Ctxt, command: Lambda[Name, Type, Ctxt, Expression]): (Ctxt, Expression) = {
          import imperative.imperativeCapabilities.returnStmt
          val params = command.variables

          val lambdaExpr = new com.github.javaparser.ast.expr.LambdaExpr()
          params.foreach { case (paramName, paramTpe) =>
            lambdaExpr.addParameter(paramTpe.clone(), paramName.mangled)
          }
          val paramMap = params.map{ case(paramName, tpe) => (paramName, new com.github.javaparser.ast.expr.NameExpr(paramName.mangled))}.toMap
          val lambdaGen = for {
            resultExp <- command.body(paramMap)
            resultStmt <- returnStmt(resultExp)
          } yield resultStmt
          val (lambdaCtxt, lambdaResult) = Command.runGenerator(lambdaGen, imperative.manip.nextBlockContext(context))
          val lambdaStmt = imperative.manip.getBlock(lambdaCtxt).addStatement(lambdaResult)
          lambdaExpr.setBody(lambdaStmt)

          (imperative.manip.copyWithBlock(lambdaCtxt, imperative.manip.getBlock(context)), lambdaExpr)
        }
      }
  }
}