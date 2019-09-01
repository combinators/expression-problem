package org.combinators.ep.generator

import cats.free.Free
import cats.free.Free.liftF


trait Command {
  type Result
}

case class AddImport[Import](imp: Import) extends Command {
  type Result = Unit
}

case class AddBlockDefinitions[Statement](definitions: Seq[Statement]) extends Command {
  type Result = Unit
}

case class Return[Expression, Statement](result: Expression) extends Command {
  type Result = Seq[Statement]
}

object Command {
  type WithResult[R] = Command { type Result = R }
  type PerformCommand[C, X] = Perform[C, Command.WithResult[X]]
  type Generator[C, A] = Free[PerformCommand[C, *], A]

  case class Perform[Context, +Cmd <: Command](cmd: Cmd)(implicit interpreter: Understands[Context, Cmd])


  def addImport[Import, C](imp: Import)
    (implicit interp: Understands[C, AddImport[Import]]): Generator[C, Unit] =
    liftF[PerformCommand[C, *], Unit](Perform[C, AddImport[Import]](AddImport(imp)))

  def addBlockDefinitions[Statement, C](stmts: Seq[Statement])
    (implicit interp: Understands[C, AddBlockDefinitions[Statement]]): Generator[C, Unit] =
    liftF[PerformCommand[C, *], Unit](Perform(AddBlockDefinitions(stmts)))

  def doReturn[Expression, Statement, C](result: Expression)
    (implicit interp: Understands[C, Return[Expression, Statement]]): Generator[C, Seq[Statement]] =
    liftF[PerformCommand[C, *], Seq[Statement]](Perform(Return(result)))
}