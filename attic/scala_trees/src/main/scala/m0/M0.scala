package m0

trait M0ExtFam[F]:
  type M0ExtT

trait ExpM0[F]:
  given M0E

enum ExpM0[F]:
  case Lit[F](value: Double) extends ExpM0[F]
  case Add[F](left: ExpM0[F], right: ExpM0[F]) extends ExpM0[F]
  case M0Ext[F](e: M0ExtFam[F])(val ext: e.M0ExtT) extends ExpM0[F]


trait M0

given m0m0Ext: M0ExtFam[M0]:
  type M0ExtT = Void

def evalExp_M0[F](helpWith: (e: M0ExtFam[F]) => e.M0ExtT => Double)(exp: ExpM0[F]): Double =
  exp match {
    case ExpM0.Lit(d) => d
    case ExpM0.Add(left, right) =>
      val help = evalExp_M0(helpWith)
      help(left) + help(right)
    case ext@ExpM0.M0Ext(_) =>
      helpWith(ext.e)(ext.ext)
  }

def evalExpM0(exp: ExpM0[M0]): Double =
  evalExp_M0(helpWithEvalM0)(exp)
def helpWithEvalM0(e: M0ExtFam[M0])(exp: e.M0ExtT): Double = ???
