package example.expression.haskell     /*DI:LD:AI*/

import java.nio.file.Paths

import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.generator.BinaryMethod

/**
* Standard uses no context.
*/
trait StandardHaskellBinaryMethod extends BinaryMethod  {
  val domain: BaseDomain with ModelDomain

  // only approach-dependent code knows how to handle binary methods
  override def binaryContext: String = ""

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
    val name = op.name.capitalize
    val imports = m.types.map(tpe => Haskell(s"import ${tpe.name}")).mkString("\n")

    val code = Haskell(s"""|module $name where
                           |import Base
                           |import GeneralExpr    -- only needed for Producer operations
                           |${addedImports(op).mkString("\n")}
                           |$imports
                           |equals :: Astree f => Expr f -> Expr f -> Bool
                           |equals expr1 expr2 = (astree expr1) == (astree expr2)
                           |""".stripMargin)
    HaskellWithPath(code, Paths.get(s"$name.hs"))
  }
}