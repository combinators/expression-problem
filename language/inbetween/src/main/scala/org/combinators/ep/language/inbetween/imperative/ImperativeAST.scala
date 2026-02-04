package org.combinators.ep.language.inbetween.imperative

import org.combinators.ep.language.inbetween.any.AnyAST

trait ImperativeAST extends AnyAST {
  object imperative {    
    trait FinalTypes {
      type DeclareVariable <: imperative.DeclareVariable
      type AssignVariable <: imperative.AssignVariable
      type IfThenElse <: imperative.IfThenElse
      type While <: imperative.While
      type VariableReferenceExpression <: imperative.VariableReferenceExpression
    }

    trait DeclareVariable extends any.Statement {
      def getSelfDeclareVariable: imperativeFinalTypes.DeclareVariable
      def name: any.Name
      def tpe: any.Type
      def initializer: Option[any.Expression]

      def copy(name: any.Name = name, tpe: any.Type = tpe, initializer: Option[any.Expression] = initializer): DeclareVariable =
        imperativeFactory.declareVariable(name, tpe, initializer)
    }

    trait VariableReferenceExpression extends any.Expression {
      def getSelfVariableReferenceExpression: imperativeFinalTypes.VariableReferenceExpression
      def name: any.Name

      def copy(name: any.Name = name): VariableReferenceExpression =
        imperativeFactory.variableReferenceExpression(name)
    }

    trait AssignVariable extends any.Statement {
      def getSelfAssignVariable: imperativeFinalTypes.AssignVariable
      def variable: any.Expression
      def assignmentExpression: any.Expression

      def copy(variable: any.Expression = variable, assignmentExpression: any.Expression = assignmentExpression): AssignVariable =
        imperativeFactory.assignVariable(variable, assignmentExpression)
    }

    trait LiftExpression extends any.Statement {
      def expression: any.Expression

      def copy(expression: any.Expression = expression): LiftExpression = imperativeFactory.liftExpression(expression)
    }

    trait IfThenElse extends any.Statement {
      def getSelfIfThenElse: imperativeFinalTypes.IfThenElse

      def condition: any.Expression
      def ifBranch: Seq[any.Statement]
      def elseIfBranches: Seq[(any.Expression, Seq[any.Statement])]
      def elseBranch: Seq[any.Statement]

      def copy(
        condition: any.Expression = condition,
        ifBranch: Seq[any.Statement] = ifBranch,
        elseIfBranches: Seq[(any.Expression, Seq[any.Statement])] = elseIfBranches,
        elseBranch: Seq[any.Statement] = elseBranch
      ): IfThenElse =
        imperativeFactory.ifThenElse(condition, ifBranch, elseIfBranches, elseBranch)
    }

    trait While extends any.Statement {
      def getSelfWhile: imperativeFinalTypes.While

      def condition: any.Expression
      def body: Seq[any.Statement]

      def copy(
        condition: any.Expression = condition,
        body: Seq[any.Statement] = body
      ): While = imperativeFactory.whileLoop(condition, body)
    }


    trait Factory {
      def declareVariable(
        name: any.Name,
        tpe: any.Type,
        initializer: Option[any.Expression]
      ): DeclareVariable

      def variableReferenceExpression(name: any.Name): VariableReferenceExpression
      def assignVariable(variable: any.Expression, expression: any.Expression): AssignVariable
      def liftExpression(expression: any.Expression): LiftExpression

      def ifThenElse(
        condition: any.Expression,
        ifBranch: Seq[any.Statement],
        elseIfBranches: Seq[(any.Expression, Seq[any.Statement])],
        elseBranch: Seq[any.Statement]
      ): IfThenElse
      def whileLoop(condition: any.Expression, body: Seq[any.Statement]): While


      implicit def convert(other: DeclareVariable): imperativeFinalTypes.DeclareVariable  = other.getSelfDeclareVariable
      implicit def convert(other: AssignVariable): imperativeFinalTypes.AssignVariable  = other.getSelfAssignVariable
      implicit def convert(other: IfThenElse): imperativeFinalTypes.IfThenElse  = other.getSelfIfThenElse
      implicit def convert(other: While): imperativeFinalTypes.While  = other.getSelfWhile
      implicit def convert(other: VariableReferenceExpression): imperativeFinalTypes.VariableReferenceExpression  = other.getSelfVariableReferenceExpression
    }
  }

  val imperativeFinalTypes: imperative.FinalTypes
  val imperativeFactory: imperative.Factory
}
