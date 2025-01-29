package org.combinators.ep.language.inbetween.functional

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.language.inbetween.any

package object control {
  trait FinalTypes extends any.FinalTypes {
    type Lambda <: Expression
    type DeclareFunVariable <: Expression
    type FunIfThenElse <: Expression
    type PatternMatch <: Expression
    type PatternContext
    type PatternVariable <: Expression
    type ConstructorPattern <: Expression
  }

  trait Lambda[FT <: FinalTypes] extends any.Expression[FT] with Factory[FT] {
    def getSelfLambda: finalTypes.Lambda
    def variables: Seq[(any.Name[FT], any.Type[FT])]
    def body: any.Expression[FT]

    def copy(
      variables: Seq[(any.Name[FT], any.Type[FT])] = this.variables,
      body: any.Expression[FT] = this.body
    ): Lambda[FT] = lambda(variables, body)
  }

  trait DeclareFunVariable[FT <: FinalTypes] extends any.Expression[FT] with Factory[FT] {
    def getSelfDeclareFunVariable: finalTypes.DeclareFunVariable
    def name: any.Name[FT]
    def tpe: any.Type[FT]
    def isRecursive: Boolean
    def initializer: any.Expression[FT]
    def inExp: any.Expression[FT]

    def copy(
      name: any.Name[FT] = name,
      tpe: any.Type[FT] = tpe,
      isRecursive: Boolean = isRecursive,
      initializer:  any.Expression[FT] = this.initializer,
      inExp: any.Expression[FT] = this.inExp
    ): DeclareFunVariable[FT] = declareFunVariable(name, tpe, isRecursive = isRecursive, initializer, inExp)
  }

  trait IfThenElse[FT <: FinalTypes] extends any.Expression[FT] with Factory[FT] {
    def getSelfFunIfThenElse: finalTypes.FunIfThenElse

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
      funIfThenElse(condition, ifBranch, elseIfBranches, elseBranch)
  }

  trait PatternContext[FT <: FinalTypes] extends Factory[FT] {
    def getSelfPatternContext: finalTypes.PatternContext

    def variables: Seq[any.Name[FT]]
    def reify[T](tpe: TypeRep.OfHostType[T], value: T): any.Expression[FT]

    def copy(variables: Seq[any.Name[FT]] = this.variables): PatternContext[FT] = patternContext(variables)
  }

  trait PatternVariable[FT <: FinalTypes] extends Factory[FT] with any.Expression[FT] {
    def getSelfPatternVariable: finalTypes.PatternVariable

    def name: any.Name[FT]

    def copy(name: any.Name[FT] = this.name): PatternVariable[FT] = patternVariable(name)
  }

  trait ConstructorPattern[FT <: FinalTypes] extends Factory[FT] with any.Expression[FT] {
    def getSelfConstructorPattern: finalTypes.ConstructorPattern

    def tpe: any.Type[FT]
    def constructor: any.Name[FT]
    def arguments: Seq[any.Expression[FT]]

    def copy(
      tpe: any.Type[FT] = this.tpe,
      constructor: any.Name[FT] = this.constructor,
      arguments: Seq[any.Expression[FT]] = this.arguments
    ): ConstructorPattern[FT] = constructorPattern(tpe, constructor, arguments)
  }

  trait PatternMatch[FT <: FinalTypes] extends any.Expression[FT] with Factory[FT] {
    def getSelfPatternMatch: finalTypes.PatternMatch

    def onValue: any.Expression[FT]
    def cases: Seq[(any.Expression[FT], any.Expression[FT])]

    def copy(
      onValue: any.Expression[FT] = this.onValue,
      cases: Seq[(any.Expression[FT], any.Expression[FT])] = this.cases
    ): PatternMatch[FT] =
      patternMatch(onValue, cases)
  }
  
  trait Method[FT <: FinalTypes] extends any.Method[FT] with Factory[FT] {
    def emptyPatternCtxt: PatternContext[FT]
  }

  trait Factory[FT <: FinalTypes] extends any.Factory[FT] {
    def lambda(
      variables: Seq[(any.Name[FT], any.Type[FT])],
      body: any.Expression[FT]
    ): Lambda[FT]

    def declareFunVariable(
      name: any.Name[FT],
      tpe: any.Type[FT],
      isRecursive: Boolean,
      initializer: any.Expression[FT],
      inExp: any.Expression[FT]
    ): DeclareFunVariable[FT]

    def funIfThenElse(
      condition: any.Expression[FT],
      ifBranch: any.Expression[FT],
      elseIfBranches: Seq[(any.Expression[FT], any.Expression[FT])],
      elseBranch: any.Expression[FT]
    ): IfThenElse[FT]

    def patternContext(variables: Seq[any.Name[FT]] = Seq.empty): PatternContext[FT]

    def patternVariable(name: any.Name[FT]): PatternVariable[FT]

    def constructorPattern(
      tpe: any.Type[FT],
      constructor: any.Name[FT],
      arguments: Seq[any.Expression[FT]]
    ): ConstructorPattern[FT]

    def patternMatch(
      onValue: any.Expression[FT],
      cases: Seq[(any.Expression[FT], any.Expression[FT])] = Seq.empty
    ): PatternMatch[FT]

    implicit def convert(other: Lambda[FT]): Lambda[FT]
    implicit def convert(other: DeclareFunVariable[FT]): DeclareFunVariable[FT]
    implicit def convert(other: IfThenElse[FT]): IfThenElse[FT]
    implicit def convert(other: PatternContext[FT]): PatternContext[FT]
    implicit def convert(other: PatternVariable[FT]): PatternVariable[FT]
    implicit def convert(other: ConstructorPattern[FT]): ConstructorPattern[FT]
    implicit def convert(other: PatternMatch[FT]): PatternMatch[FT]
    implicit def convert(other: any.Method[FT]): Method[FT]

  }
}
