package org.combinators.bottomUp.oneSequence

import org.combinators.dp.Utility
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator

trait OneSequencesUtility extends Utility {
  import paradigm._
  import syntax._

  def format_if_else(iterator: Expression, input: (Expression, Statement)): (Generator[MethodBodyContext,Expression], Generator[MethodBodyContext,Unit]) = {
    import paradigm.methodBodyCapabilities._

    var cond = arithmetic.arithmeticCapabilities.le(iterator, input._1)
    var body = for {
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
