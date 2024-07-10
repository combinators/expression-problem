package org.combinators.ep.language.inbetween.functional

import org.combinators.ep.language.inbetween.any


package object control {
  trait FinalTypes extends any.FinalTypes {
    type Lambda <: Expression
    type DeclareVariable <: Expression
    type IfThenElse <: Expression
    type PatternMatch <: Expression
    type PatternExpression
    type TypePatternExpression <: PatternExpression
    type Name <: PatternExpression
  }

  trait Lambda[FT <: FinalTypes] extends any.Expression[FT] with Factory[FT] {
    def variables: Seq[(any.Name[FT], any.Type[FT])]
    def body: any.Expression[FT]

    def copy(
      variables: Seq[(any.Name[FT], any.Type[FT])] = this.variables,
      body: any.Expression[FT] = this.body
    ): Lambda[FT] = lambda(variables, body)
  }

  trait DeclareVariable[FT <: FinalTypes] extends any.Expression[FT] with Factory[FT] {
    def getSelfDeclareVariable: finalTypes.DeclareVariable
    def name: any.Name[FT]
    def tpe: any.Type[FT]
    def initializer: Option[any.Expression[FT]]
    def inExp: any.Expression[FT]

    def copy(
      name: any.Name[FT] = name,
      tpe: any.Type[FT] = tpe,
      initializer: Option[any.Expression[FT]] = this.initializer,
      inExp: any.Expression[FT] = this.inExp
    ): DeclareVariable[FT] = declareVariable(name, tpe, initializer, inExp)
  }

  trait IfThenElse[FT <: FinalTypes] extends any.Expression[FT] with Factory[FT] {
    def getSelfIfThenElse: finalTypes.IfThenElse

    def condition: any.Expression[FT]
    def ifBranch: any.Expression[FT]
    def elseIfBranches: Seq[(any.Expression[FT], any.Expression[FT])]
    def elseBranch: any.Expression[FT]

    def copy(
      condition: any.Expression[FT] = this.condition,
      ifBranch: any.Expression[FT] = this.ifBranch,
      elseIfBranches: Seq[(any.Expression[FT], any.Expression[FT])] = this.elseIfBranches,
      elseBranch: any.Expression[FT] = this.elseBranch
    ): IfThenElse[FT] =
      ifThenElse(condition, ifBranch, elseIfBranches, elseBranch)
  }

  trait PatternExpression[FT <: FinalTypes] extends Factory[FT] {
    def getSelfPatternExpression: finalTypes.PatternExpression

    def getVariables: Seq[any.Name[FT]]
  }

  trait Name[FT <: FinalTypes] extends any.Name[FT] with PatternExpression[FT] {
    override def getVariables: Seq[any.Name[FT]] = Seq(this)
  }

  trait TypePatternExpression[FT <: FinalTypes] extends PatternExpression[FT] {
    def getSelfTypePatternExpression: finalTypes.TypePatternExpression    

    def tpe: any.Type[FT]
    def constructorName: Seq[any.Name[FT]]
    def constructorArguments: Seq[PatternExpression[FT]]

    override def getVariables: Seq[any.Name[FT]] = constructorArguments.flatMap(_.getVariables)

    def copy(
      tpe: any.Type[FT] = this.tpe,
      constructorName: Seq[any.Name[FT]] = this.constructorName,
      constructorArguments: Seq[PatternExpression[FT]] = this.constructorArguments
    ): TypePatternExpression[FT] = typePatternExpression(tpe, constructorName, constructorArguments)
  }

  trait PatternMatch[FT <: FinalTypes] extends any.Expression[FT] with Factory[FT] {
    def getSelfPatternMatch: finalTypes.PatternMatch

    def onValue: any.Expression[FT]
    def cases: Seq[(PatternExpression[FT], any.Expression[FT])]

    def copy(
      onValue: any.Expression[FT] = this.onValue,
      cases: Seq[(PatternExpression[FT], any.Expression[FT])] = this.cases
    ): PatternMatch[FT] =
      patternMatch(onValue, cases)
  }

  trait Factory[FT <: FinalTypes] extends any.Factory[FT] {
    def lambda(
      variables: Seq[(any.Name[FT], any.Type[FT])],
      body: any.Expression[FT]
    ): Lambda[FT]

    def declareVariable(
      name: any.Name[FT],
      tpe: any.Type[FT],
      initializer: Option[any.Expression[FT]],
      inExp: any.Expression[FT]
    ): DeclareVariable[FT] = declareVariable(name, tpe, initializer, inExp)

    def ifThenElse(
      condition: any.Expression[FT],
      ifBranch: any.Expression[FT],
      elseIfBranches: Seq[(any.Expression[FT], any.Expression[FT])],
      elseBranch: any.Expression[FT]
    ): IfThenElse[FT]

    def typePatternExpression(
      tpe: any.Type[FT],
      constructorName: Seq[any.Name[FT]],
      constructorArguments: Seq[PatternExpression[FT]] = Seq.empty
    ): TypePatternExpression[FT]

    def patternMatch(
      onValue: any.Expression[FT],
      cases: Seq[(PatternExpression[FT], any.Expression[FT])] = Seq.empty
    ): PatternMatch[FT]

    implicit def convert(other: Lambda[FT]): Lambda[FT]
    implicit def convert(other: DeclareVariable[FT]): DeclareVariable[FT]
    implicit def convert(ifThenElse: IfThenElse[FT]): IfThenElse[FT]
    implicit def convert(patternExp: PatternExpression[FT]): PatternExpression[FT]
    implicit def convert(typePatternExp: TypePatternExpression[FT]): TypePatternExpression[FT]
    implicit def convert(patternMatch: PatternMatch[FT]): PatternMatch[FT]

    implicit override def convert(other: any.Name[FT]): Name[FT]
  }
}
