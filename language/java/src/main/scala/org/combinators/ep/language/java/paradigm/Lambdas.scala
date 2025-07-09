package org.combinators.ep.language.java.paradigm    /*DI:LD:AI*/

import org.combinators.cogen.paradigm.control.Lambdas as Lams
import org.combinators.cogen.{Command, Understands}

trait Lambdas[Ctxt, AP <: AnyParadigm] extends Lams[Ctxt] {

  val base: AP
  import base.syntax._

//  object lambdaCapabilities extends LambdaCapabilities {
//    implicit val canLambda: Understands[Ctxt, Lambda[Name, Type, Ctxt, Expression]] =
//      new Understands[Ctxt, Lambda[Name, Type, Ctxt, Expression]] {
//        def perform(context: Ctxt, command: Lambda[Name, Type, Ctxt, Expression]): (Ctxt, Statement) = {
//          val params = command.variables
//          val lambdaExpr = new com.github.javaparser.ast.expr.LambdaExpr()
//
//          params.foreach { case (paramName, paramTpe) =>
//            lambdaExpr.addParameter(paramTpe.clone(), paramName.mangled)
//          }
//          val paramMap = params.map{ case(name, tpe) => (name, new com.github.javaparser.ast.expr.NameExpr(name.toAST))}.toMap
//          val (lambdaCtxt, _) = Command.runGenerator(command.body(paramMap), manip.nextBlockContext(context))
//
//          lambdaExpr.setBody(command.body)
//          whileStmt.setBody(manip.getBlock(whileCtxt))
//          (manip.copyWithBlock(whileCtxt, manip.getBlock(context)), lambdExpr)
//        }
//      }
//  }
}