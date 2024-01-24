package org.combinators.ep.language.inbetween    /*DI:LI:AI*/

import scala.meta.Term.If

package object imperative {
  trait FinalTypes extends any.FinalTypes {
    type DeclareVariable <: Statement
    type AssignVariable <: Statement
    type IfThenElse <: Statement
    type While <: Statement
    type VariableReferenceExpression <: Expression
  }

  trait DeclareVariable[FT <: FinalTypes] extends any.Statement[FT] with Factory[FT] {
    def getSelfDeclareVariable: finalTypes.DeclareVariable
    def name: any.Name[FT]
    def tpe: any.Type[FT]
    def initializer: Option[any.Expression[FT]]
    
    def copy(name: any.Name[FT] = name, tpe: any.Type[FT] = tpe, initializer: Option[any.Expression[FT]] = initializer): DeclareVariable[FT] =
      declareVariable(name, tpe, initializer)
  }

  trait VariableReferenceExpression[FT <: FinalTypes] extends any.Expression[FT] with Factory[FT] {
    def getSelfVariableReferenceExpression: finalTypes.VariableReferenceExpression
    def name: any.Name[FT]

    def copy(name: any.Name[FT] = name): VariableReferenceExpression[FT] =
      variableReferenceExpression(name)
  }

  trait AssignVariable[FT <: FinalTypes] extends any.Statement[FT] with Factory[FT] {
    def getSelfAssignVariable: finalTypes.AssignVariable
    def variable: any.Expression[FT]
    def assignmentExpression: any.Expression[FT]

    def copy(variable: any.Expression[FT] = variable, assignmentExpression: any.Expression[FT] = assignmentExpression): AssignVariable[FT] =
      assignVariable(variable, assignmentExpression)
  }

  trait LiftExpression[FT <: FinalTypes] extends any.Statement[FT] with Factory[FT] {
    def expression: any.Expression[FT]
    
    def copy(expression: any.Expression[FT] = expression): LiftExpression[FT] = liftExpression(expression)
  }

  trait IfThenElse[FT <: FinalTypes] extends any.Statement[FT] with Factory[FT] {
    def getSelfIfThenElse: finalTypes.IfThenElse

    def condition: any.Expression[FT]
    def ifBranch: Seq[any.Statement[FT]]
    def elseIfBranches: Seq[(any.Expression[FT], Seq[any.Statement[FT]])]
    def elseBranch: Seq[any.Statement[FT]]

    def copy(
      condition: any.Expression[FT] = condition,
      ifBranch: Seq[any.Statement[FT]] = ifBranch,
      elseIfBranches: Seq[(any.Expression[FT], Seq[any.Statement[FT]])] = elseIfBranches,
      elseBranch: Seq[any.Statement[FT]] = elseBranch
    ): IfThenElse[FT] =
      ifThenElse(condition, ifBranch, elseIfBranches, elseBranch)
  }

  trait While[FT <: FinalTypes] extends any.Statement[FT] with Factory[FT] {
    def getSelfWhile: finalTypes.While

    def condition: any.Expression[FT]
    def body: Seq[any.Statement[FT]]

    def copy(
      condition: any.Expression[FT] = condition,
      body: Seq[any.Statement[FT]] = body
    ): While[FT] = whileLoop(condition, body)
  }


  trait Factory[FT <: FinalTypes] extends any.Factory[FT] {
    def declareVariable(
      name: any.Name[FT],
      tpe: any.Type[FT],
      initializer: Option[any.Expression[FT]]): DeclareVariable[FT]

    def variableReferenceExpression(name: any.Name[FT]): VariableReferenceExpression[FT]
    def assignVariable(variable: any.Expression[FT], expression: any.Expression[FT]): AssignVariable[FT]
    def liftExpression(expression: any.Expression[FT]): LiftExpression[FT]
    
    def ifThenElse(
      condition: any.Expression[FT],
      ifBranch: Seq[any.Statement[FT]],
      elseIfBranches: Seq[(any.Expression[FT], Seq[any.Statement[FT]])],
      elseBranch: Seq[any.Statement[FT]]): IfThenElse[FT]
    def whileLoop(condition: any.Expression[FT], body: Seq[any.Statement[FT]]): While[FT]


    implicit def convert(decl: DeclareVariable[FT]): DeclareVariable[FT]
    implicit def convert(assignVariable: AssignVariable[FT]): AssignVariable[FT]
    implicit def convert(ifThenElse: IfThenElse[FT]): IfThenElse[FT]
    implicit def convert(whileLoop: While[FT]): While[FT]
    implicit def convert(varRef: VariableReferenceExpression[FT]): VariableReferenceExpression[FT]
  }
}
