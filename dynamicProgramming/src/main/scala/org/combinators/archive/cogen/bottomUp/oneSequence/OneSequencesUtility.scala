package org.combinators.archive.cogen.bottomUp.oneSequence

import org.combinators.cogen.TypeRep
import org.combinators.cogen.Command.Generator
import org.combinators.dp.original.Utility

trait OneSequencesUtility extends Utility {
  import paradigm._
  import syntax._

  def format_if_else(iterator: Expression, input: (Expression, Statement)): (Generator[MethodBodyContext,Expression], Generator[MethodBodyContext,Unit]) = {
    import paradigm.methodBodyCapabilities._

    val cond = arithmetic.arithmeticCapabilities.le(iterator, input._1)
    val body = for {
      _ <- addBlockDefinitions(Seq(input._2))
    } yield ()
    (cond, body)
  }

  def one_sequence_bottom_up(iterator: Expression, length: Expression, baseCases: Seq[(Expression, Statement)],relation: Statement): Generator[MethodBodyContext, Seq[Statement]] ={
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      condExpr <- arithmetic.arithmeticCapabilities.lt(iterator,length)
      while_loop <- impParadigm.imperativeCapabilities.whileLoop(condExpr, for {

        ifCond1 <- arithmetic.arithmeticCapabilities.le(iterator, baseCases.head._1)
        ifStmt <- impParadigm.imperativeCapabilities.ifThenElse(ifCond1, for {
          _ <- addBlockDefinitions(Seq(baseCases.head._2)) //First Base Case
        } yield (),

          Seq.empty,
          //baseCases.tail.map(format_if_else),   //Other Base Cases

          Some(
            for {
              _ <- addBlockDefinitions(Seq(relation))  //General Case
            } yield ())
        )

        incrStmt <- plus_equals(iterator, one)
        _ <- addBlockDefinitions(Seq(ifStmt,incrStmt))
      } yield ())
    }yield(Seq(while_loop))
  }
}
