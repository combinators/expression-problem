package org.combinators.ep.language.inbetween.functional.control

import org.combinators.cogen.TypeRep
import org.combinators.ep.language.inbetween.any.AnyAST

trait FunctionalControlAST extends AnyAST {
  object funcontrol {
    object anyOverrides {
      trait FinalTypes extends any.FinalTypes {
        type Method <: anyOverrides.Method
      }
      
      trait Method extends any.Method { 
        def emptyPatternCtxt: PatternContext
      }
    }

    trait FinalTypes {
      type Lambda <: funcontrol.Lambda
      type DeclareFunVariable <: funcontrol.DeclareFunVariable
      type FunIfThenElse <: funcontrol.IfThenElse
      type PatternMatch <: funcontrol.PatternMatch
      type PatternContext <: funcontrol.PatternContext
      type PatternVariable <: funcontrol.PatternVariable
      type ConstructorPattern <: funcontrol.ConstructorPattern
    }

    trait Lambda extends any.Expression { 
      def getSelfLambda: functionalControlFinalTypes.Lambda
      def variables: Seq[(any.Name, any.Type)]
      def body: any.Expression

      def copy(
        variables: Seq[(any.Name, any.Type)] = this.variables,
        body: any.Expression = this.body
      ): Lambda = functionalControlFactory.lambda(variables, body)
    }

    trait DeclareFunVariable extends any.Expression {
      def getSelfDeclareFunVariable: functionalControlFinalTypes.DeclareFunVariable
      def name: any.Name
      def tpe: any.Type
      def isRecursive: Boolean
      def initializer: any.Expression
      def inExp: any.Expression

      def copy(
        name: any.Name = name,
        tpe: any.Type = tpe,
        isRecursive: Boolean = isRecursive,
        initializer: any.Expression = this.initializer,
        inExp: any.Expression = this.inExp
      ): DeclareFunVariable = functionalControlFactory.declareFunVariable(name, tpe, isRecursive = isRecursive, initializer, inExp)
    }

    trait IfThenElse extends any.Expression {
      def getSelfFunIfThenElse: functionalControlFinalTypes.FunIfThenElse

      def condition: any.Expression
      def ifBranch: any.Expression
      def elseIfBranches: Seq[(any.Expression, any.Expression)]
      def elseBranch: any.Expression

      def copy(
        condition: any.Expression = this.condition,
        ifBranch: any.Expression = this.ifBranch,
        elseIfBranches: Seq[(any.Expression, any.Expression)] = this.elseIfBranches,
        elseBranch: any.Expression = this.elseBranch
      ): IfThenElse =
        functionalControlFactory.funIfThenElse(condition, ifBranch, elseIfBranches, elseBranch)
    }

    trait PatternContext {
      def getSelfPatternContext: functionalControlFinalTypes.PatternContext

      def variables: Seq[any.Name]
      def reify[T](tpe: TypeRep.OfHostType[T], value: T): any.Expression

      def copy(variables: Seq[any.Name] = this.variables): PatternContext = functionalControlFactory.patternContext(variables)
    }

    trait PatternVariable extends any.Expression {
      def getSelfPatternVariable: functionalControlFinalTypes.PatternVariable

      def name: any.Name

      def copy(name: any.Name = this.name): PatternVariable = functionalControlFactory.patternVariable(name)
    }

    trait ConstructorPattern extends any.Expression {
      def getSelfConstructorPattern: functionalControlFinalTypes.ConstructorPattern

      def tpe: any.Type
      def constructor: any.Name
      def arguments: Seq[any.Expression]

      def copy(
        tpe: any.Type = this.tpe,
        constructor: any.Name = this.constructor,
        arguments: Seq[any.Expression] = this.arguments
      ): ConstructorPattern = functionalControlFactory.constructorPattern(tpe, constructor, arguments)
    }

    trait PatternMatch extends any.Expression {
      def getSelfPatternMatch: functionalControlFinalTypes.PatternMatch

      def onValue: any.Expression
      def cases: Seq[(any.Expression, any.Expression)]

      def copy(
        onValue: any.Expression = this.onValue,
        cases: Seq[(any.Expression, any.Expression)] = this.cases
      ): PatternMatch =
        functionalControlFactory.patternMatch(onValue, cases)
    }

    

    trait Factory {
      def lambda(
        variables: Seq[(any.Name, any.Type)],
        body: any.Expression
      ): Lambda

      def declareFunVariable(
        name: any.Name,
        tpe: any.Type,
        isRecursive: Boolean,
        initializer: any.Expression,
        inExp: any.Expression
      ): DeclareFunVariable

      def funIfThenElse(
        condition: any.Expression,
        ifBranch: any.Expression,
        elseIfBranches: Seq[(any.Expression, any.Expression)],
        elseBranch: any.Expression
      ): IfThenElse

      def patternContext(variables: Seq[any.Name] = Seq.empty): PatternContext

      def patternVariable(name: any.Name): PatternVariable

      def constructorPattern(
        tpe: any.Type,
        constructor: any.Name,
        arguments: Seq[any.Expression]
      ): ConstructorPattern

      def patternMatch(
        onValue: any.Expression,
        cases: Seq[(any.Expression, any.Expression)] = Seq.empty
      ): PatternMatch

      implicit def convert(other: Lambda): functionalControlFinalTypes.Lambda = other.getSelfLambda
      implicit def convert(other: DeclareFunVariable): functionalControlFinalTypes.DeclareFunVariable = other.getSelfDeclareFunVariable
      implicit def convert(other: IfThenElse): functionalControlFinalTypes.FunIfThenElse = other.getSelfFunIfThenElse
      implicit def convert(other: PatternContext): functionalControlFinalTypes.PatternContext = other.getSelfPatternContext
      implicit def convert(other: PatternVariable): functionalControlFinalTypes.PatternVariable = other.getSelfPatternVariable
      implicit def convert(other: ConstructorPattern): functionalControlFinalTypes.ConstructorPattern = other.getSelfConstructorPattern
      implicit def convert(other: PatternMatch): functionalControlFinalTypes.PatternMatch = other.getSelfPatternMatch
    }
  }

  val finalTypes: funcontrol.anyOverrides.FinalTypes
  val functionalControlFinalTypes: funcontrol.FinalTypes
  val functionalControlFactory: funcontrol.Factory
}
