package org.combinators.ep.language.scala.paradigm.control   /*DI:LD:AI*/

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.control.{Lambda, PatternMatch, Functional => Func}
import org.combinators.ep.generator.paradigm.{DeclareVariable, IfThenElse}
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.language.scala.{MethodBodyCtxt, Syntax}
import org.combinators.ep.language.scala.paradigm.{AnyParadigm, BlockContextManipulator}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._

import scala.meta.{Case, Defn, Pat, Term}

trait Functional[Ctxt, AP <: AnyParadigm] extends Func[Ctxt] {

  val base: AP
  import base.syntax._

  val blockContextManipulator: BlockContextManipulator[Ctxt]

  object functionalCapabilities extends FunctionalCapabilities {
    implicit val canDeclareVar: Understands[Ctxt, DeclareVariable[Syntax.MangledName, Type, Generator[Ctxt, Term], Term]] =
      new Understands[Ctxt, DeclareVariable[Syntax.MangledName, Type, Generator[Ctxt, Term], Term]] {
        def perform(
          context: Ctxt,
          command: DeclareVariable[Syntax.MangledName, Type, Generator[Ctxt, Term], Term]): (Ctxt, Term) = {
          val (resultingCtxt, rhs) =
            Command.runGenerator(command.initialization, context)
          val lhs = Term.Name(command.name.toAST.value)
          val assignment = Defn.Val(
            mods = List.empty,
            pats = List(Pat.Var(lhs)),
            decltpe = Some(command.tpe),
            rhs = rhs
          )
          val nextBlock =
            Term.Block(blockContextManipulator.getBlock(resultingCtxt).stats :+ assignment)
          (blockContextManipulator.copyWithBlock(resultingCtxt, nextBlock), lhs)
        }
      }

    implicit val canIfThenElse: Understands[Ctxt, IfThenElse[Term, Generator[Ctxt, Term], Generator[Ctxt, Term], Term]] =
      new Understands[Ctxt, IfThenElse[Term, Generator[Ctxt, Term], Generator[Ctxt, Term], Term]] {
        def perform(
          context: Ctxt,
          command: IfThenElse[Term, Generator[Ctxt, Term], Generator[Ctxt, Term], Term]
        ): (Ctxt, Term) = {
          val genUpToLastElse =
            command.elseIfBranches.foldLeft[Generator[Ctxt, Term] => Generator[Ctxt, Term]](elseGen =>
              for {
                ifBranch <- command.ifBranch
                elseBranch <- elseGen
              } yield Term.If(command.condition, ifBranch, elseBranch)
            ) { case (ifElseGen, (elseIfCond, elseIfGen)) =>
              (elseGen) =>
                ifElseGen(
                  for {
                    elseIfBranch <- elseIfGen
                    elseBranch <- elseGen
                  } yield Term.If(elseIfCond, elseIfBranch, elseBranch)
                )
            }
          val gen = genUpToLastElse(command.elseBranch)
          Command.runGenerator(gen, context)
        }
      }
    implicit val canPatternMatch: Understands[Ctxt, PatternMatch[Ctxt, Syntax.MangledName, Term]] =
      new Understands[Ctxt, PatternMatch[Ctxt, Syntax.MangledName, Term]] {
        def perform(
          context: Ctxt,
          command: PatternMatch[Ctxt, Syntax.MangledName, Term]
        ): (Ctxt, Term) = {
          val matchExp =
            for {
              cases <- forEach(command.options.toList) { case ((name, attNames), caseGen) =>
                val variables = attNames.map(attName => Term.fresh(attName.toAST.value))
                caseGen(variables).map[Case](caseCode =>
                  Case(
                    Pat.Extract(Term.Name(name.toAST.value), variables.toList.map(variable => Pat.Var(variable))),
                    None,
                    caseCode
                  ))
              }
            } yield Term.Match(command.onValue, cases)

          Command.runGenerator(matchExp, context)
        }
      }
    implicit val canLambda: Understands[Ctxt, Lambda[Syntax.MangledName, Type, Ctxt, Term]] =
      new Understands[Ctxt, Lambda[Syntax.MangledName, Type, Ctxt, Term]] {
        def perform(
          context: Ctxt, 
          command: Lambda[Syntax.MangledName, Type, Ctxt, Term]
        ): (Ctxt, Term) = {
          val variable = Term.fresh(command.variable.toAST.value)
          val lambdaGen = 
            command.body(variable).map(body =>
              Term.Function(List(Term.Param(List.empty, variable, Some(command.tpe), None)), body)
            )
          Command.runGenerator(lambdaGen, context)
        }
      }
  }


}


object Functional {
  def inMethodContext[AP <: AnyParadigm](base: AP): Functional[MethodBodyCtxt, base.type] = {
    val b: base.type = base
    new Functional[MethodBodyCtxt, b.type] {
      val base: b.type = b
      val blockContextManipulator: BlockContextManipulator[MethodBodyCtxt] = BlockContextManipulator.inMethodContext(base)
    }
  }
}