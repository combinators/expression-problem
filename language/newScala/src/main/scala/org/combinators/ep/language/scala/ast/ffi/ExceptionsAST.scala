package org.combinators.ep.language.scala.ast.ffi     /*DI:LD:AI*/

import org.combinators.ep.language.inbetween.ffi.ExceptionsAST as InbetweenExceptionsAST
import org.combinators.ep.language.scala.ast.{BaseAST, FinalBaseAST}
import org.combinators.ep.language.scala.ast.ffi.OperatorExpressionsAST

trait ExceptionsAST extends InbetweenExceptionsAST { self: OperatorExpressionsAST & BaseAST =>
  object scalaExceptions {
    object exceptionsOpsOverride {
      trait FinalTypes extends exceptionsOps.FinalTypes {
        type Exception <: exceptionsOpsOverride.Exception
        type Raise <: exceptionsOpsOverride.Raise
      }

      trait Exception extends exceptionsOps.Exception with scalaBase.anyOverrides.Expression {
        import factory.*

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): exceptionsOps.Exception =
          exceptionsOpsFactory.convert(copy(cause = cause.prefixRootPackage(rootPackageName, excludedTypeNames)))

        override def toScala: String = s"new RuntimeException(${cause.toScala})"
      }

      trait Raise extends exceptionsOps.Raise with scalaBase.anyOverrides.Statement {
        import exceptionsOpsFactory.*

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): exceptionsOps.Raise = {
          exceptionsOpsFactory.convert(copy(exception = exception.prefixRootPackage(rootPackageName, excludedTypeNames)))
        }

        override def toScala: String = s"throw ${exception.toScala}"
      }

      trait Factory extends exceptionsOps.Factory {}
    }
  }

  val exceptionsOpsFinalTypes : scalaExceptions.exceptionsOpsOverride.FinalTypes
  val exceptionsOpsFactory: scalaExceptions.exceptionsOpsOverride.Factory
}

trait FinalExceptionsAST extends ExceptionsAST { self: FinalOperatorExpressionsAST & FinalBaseAST =>

  object finalExceptionsFinalTypes {
    trait ExceptionsFinalTypes extends scalaExceptions.exceptionsOpsOverride.FinalTypes {
      // To complete this design, have to fix the actual types for the convert() to work properly
      type Raise = scalaExceptions.exceptionsOpsOverride.Raise
      type Exception = scalaExceptions.exceptionsOpsOverride.Exception
    }
  }

  object finalExceptionsFactoryTypes {
    trait ExceptionsFactory extends scalaExceptions.exceptionsOpsOverride.Factory {
      override def exception(cause:any.Expression): scalaExceptions.exceptionsOpsOverride.Exception = {
        case class Exception(override val cause:any.Expression) extends scalaExceptions.exceptionsOpsOverride.Exception
          with finalBaseAST.anyOverrides.FinalExpression {
          override def getSelfException: exceptionsOpsFinalTypes.Exception = this
        }
        Exception(cause)
      }

      override def raise(exception:exceptionsOps.Exception): scalaExceptions.exceptionsOpsOverride.Raise = {
        case class Raise(exception:exceptionsOps.Exception) extends scalaExceptions.exceptionsOpsOverride.Raise
          with finalBaseAST.anyOverrides.FinalStatement {
          override def getSelfRaise: exceptionsOpsFinalTypes.Raise = this
        }
        Raise(exception)
      }
    }
  }

  override val exceptionsOpsFinalTypes: finalExceptionsFinalTypes.ExceptionsFinalTypes = new finalExceptionsFinalTypes.ExceptionsFinalTypes {}
  val exceptionsOpsFactory: finalExceptionsFactoryTypes.ExceptionsFactory = new finalExceptionsFactoryTypes.ExceptionsFactory {}
}