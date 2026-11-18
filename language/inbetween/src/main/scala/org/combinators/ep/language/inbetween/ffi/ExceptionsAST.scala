package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

import org.combinators.ep.language.inbetween.any.AnyAST

trait ExceptionsAST extends AnyAST {
  object exceptionsOps {
    trait FinalTypes {
      type Raise <: exceptionsOps.Raise
      type Exception <: exceptionsOps.Exception
    }

    trait Raise extends any.Statement {
      def getSelfRaise: exceptionsOpsFinalTypes.Raise
      def exception: Exception

      def copy(exception: Exception = exception): Raise =
        exceptionsOpsFactory.raise(exception)
    }

    trait Exception extends any.Expression {
      def getSelfException: exceptionsOpsFinalTypes.Exception
      def cause: any.Expression

      def copy(cause: any.Expression = cause): Exception =
        exceptionsOpsFactory.exception(cause)
    }

    trait Factory {
      def raise(exception: Exception): Raise
      def exception(cause: any.Expression): Exception

      implicit def convert(other: Raise): exceptionsOpsFinalTypes.Raise = other.getSelfRaise
      implicit def convert(other: Exception): exceptionsOpsFinalTypes.Exception = other.getSelfException
    }
  }

  val exceptionsOpsFinalTypes: exceptionsOps.FinalTypes
  val exceptionsOpsFactory: exceptionsOps.Factory
}
