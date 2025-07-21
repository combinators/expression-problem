package org.combinators.cogen.paradigm.ffi

import org.combinators.cogen.paradigm.{AnyParadigm, Apply, Reify}
import org.combinators.cogen.{Command, Understands, TypeRep}
import Command.Generator

import scala.annotation.tailrec

case class StringAppend()
case class GetStringLength()
case class ToString[Type](sourceType: Type)

trait Strings[Context] extends FFI {
  import base.syntax._

  trait StringCapabilities {
    implicit val canGetStringLength: Understands[Context, Apply[GetStringLength, Expression, Expression]]
    def getStringLength(expression: Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[GetStringLength, Expression, Expression](GetStringLength(), Seq(expression)))

    implicit val canAppend: Understands[Context, Apply[StringAppend, Expression, Expression]]
    def stringAppend(xs: Expression*): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[StringAppend, Expression, Expression](StringAppend(), xs))

    implicit val canToStringInCtxt: Understands[Context, Apply[ToString[Type], Expression, Expression]]
    def asString(expression: Expression, ofType: Type): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[ToString[Type], Expression, Expression](ToString(ofType), Seq(expression)))

    def makeString
        (exprs: Seq[Expression], start: String, sep: String, end: String)
        (implicit canReifyString: Understands[Context, Reify[String, Expression]]): Generator[Context, Expression] = {
      @tailrec def make(sepExp: Expression, endExp: Expression, s: Seq[Expression] => Seq[Expression], xs: Seq[Expression]): Seq[Expression] =
        xs match {
          case Seq(x) => s(Seq(x, endExp))
          case x +: xs => make(sepExp, endExp, (tl: Seq[Expression]) => s(x +: sepExp +: tl), xs)
          case Seq() => Seq(endExp)
        }
      for {
        startExp <- Reify[String, Expression](TypeRep.String, start).interpret
        sepExp <- Reify[String, Expression](TypeRep.String, sep).interpret
        endExp <- Reify[String, Expression](TypeRep.String, end).interpret
        inters = startExp +: make(sepExp, endExp, tl => tl, exprs)
        res <- stringAppend(inters*)
      } yield res
    }

    def makeString
        (exprs: Seq[Expression], sep: String)
        (implicit canReifyString: Understands[Context, Reify[String, Expression]]): Generator[Context, Expression] =
      makeString(exprs, "", sep, "")
  }
  val stringCapabilities: StringCapabilities
}

object Strings {
  type WithBase[Ctxt, B <: AnyParadigm] = Strings[Ctxt] { val base: B }
}