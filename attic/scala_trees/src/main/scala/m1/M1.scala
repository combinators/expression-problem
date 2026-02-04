package m1

import m0.{ExpM0, M0ExtFam, evalExp_M0}

trait M1ExtFam[F]:
  type M1ExtT
  given M0ExtFam[F] = compiletime.deferred

enum ExpM1[F]:
  case Sub[F](left: ExpM0[F], right: ExpM0[F]) extends ExpM1[F]
  case M1Ext[F](m1ext: M1ExtFam[F])(ext: m1ext.M1ExtT) extends ExpM1[F]

trait M1

given m0m1Ext: M0ExtFam[M1]:
  type M0ExtT = ExpM1[M1]

given m1m1Ext: M1ExtFam[M1]:
  type M1ExtT = Void

def evalExp_M1[F](helpWith: (e1: M1ExtFam[F]) => e1.M1ExtT => Double)(exp: ExpM1[F]): Double =
  exp match {
    case ExpM1.Sub(left, right) =>
      val help = evalExp_M0[F](e0 => evalExp_M1(helpWith))
      help(left) - help(right)
    case ext@ExpM0.M0Ext(_) =>
      helpWith(ext.e)(ext.ext)
  }

//def evalExpM1(exp: ExpM0[M0]): Double =
//  evalExp_M0(helpWithEvalM0)(exp)
//def helpWithEvalM0(e: M0ExtFam[M0])(exp: e.M0ExtT): Double = ???