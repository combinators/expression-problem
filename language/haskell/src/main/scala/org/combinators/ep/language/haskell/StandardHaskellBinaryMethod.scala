package org.combinators.ep.language.haskell      /*DI:LD:AI*/

import java.nio.file.Paths

import org.combinators.ep.domain.{BaseDomain, OperationDependency}

/**
* Standard uses no context.
*/
trait StandardHaskellBinaryMethod extends OperationDependency {
  val domain: BaseDomain with ModelDomain

  def addedImports(op:domain.Operation):Seq[Haskell]

  /**
    * Haskell-specific optimizations for binary operations, in particular, equals
    *
    * Any BinaryMethod takes Astree representation and does its thing
    *
    * @param m
    * @param op
    * @return
    */
  def generateBinaryOp(m: domain.Model, op: domain.Operation): HaskellWithPath = {
    val imports = m.types.map(tpe => Haskell(s"import ${tpe.concept}")).mkString("\n")

    val dependentOps = dependency(op).map(op => new Haskell(s"import ${op.concept}"))
    val extraImports = addedImports(op) ++ dependentOps
    val code = Haskell(s"""|module ${op.concept} where
                           |import Base
                           |
                           |${extraImports.mkString("\n")}
                           |$imports
                           |equals :: Astree f => Expr f -> Expr f -> Bool
                           |equals expr1 expr2 = (astree expr1) == (astree expr2)
                           |""".stripMargin)
    HaskellWithPath(code, Paths.get(s"${op.concept}.hs"))
  }
}