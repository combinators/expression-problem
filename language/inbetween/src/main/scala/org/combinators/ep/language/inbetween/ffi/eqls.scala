package org.combinators.ep.language.inbetween.ffi

import org.combinators.ep.language.inbetween.any

object eqls {
  trait Equals[FT <: any.FinalTypes] extends any.Expression[FT] with Factory[FT] {
    def tpe: any.Type[FT]
    def left: any.Expression[FT]
    def right: any.Expression[FT]

    def copy(
      tpe: any.Type[FT],
      left: any.Expression[FT] = left,
      right: any.Expression[FT] = right
    ): Equals[FT] = equals(tpe, left, right)
  }

  trait Factory[FT <: any.FinalTypes] extends any.Factory[FT] {
    def equals(tpe: any.Type[FT], left: any.Expression[FT], right: any.Expression[FT]): Equals[FT]
  }
}