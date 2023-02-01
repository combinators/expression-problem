package org.combinators.ep.language.inbetween

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
  }

  trait VariableReferenceExpression[FT <: FinalTypes] extends any.Expression[FT] with Factory[FT] {
    def name: any.Name[FT]
  }

  trait AssignVariable[FT <: FinalTypes] extends any.Statement[FT] with Factory[FT] {
    def getSelfAssignVariable: finalTypes.AssignVariable
    def name: any.Name[FT]
    def assignmentExpression: any.Expression[FT]
  }

  trait LiftExpression[FT <: FinalTypes] extends any.Statement[FT] with Factory[FT] {
    def expression: any.Expression[FT]
  }

  trait IfThenElse[FT <: FinalTypes] extends any.Statement[FT] with Factory[FT] {
    def getSelfIfThenElse: finalTypes.IfThenElse

    def condition: any.Expression[FT]
    def ifBranch: Seq[any.Statement[FT]]
    def elseIfBranches: Seq[(any.Expression[FT], Seq[any.Statement[FT]])]
    def elseBranch: Seq[any.Statement[FT]]
  }

  trait While[FT <: FinalTypes] extends any.Statement[FT] with Factory[FT] {
    def getSelfWhile: finalTypes.While

    def condition: any.Expression[FT]
    def body: Seq[any.Statement[FT]]
  }


  trait Factory[FT <: FinalTypes] extends any.Factory[FT] {
    def declareVariable(
      name: any.Name[FT],
      tpe: any.Type[FT],
      initializer: Option[any.Expression[FT]]): DeclareVariable[FT]

    def variableReferenceExpression(name: any.Name[FT]): VariableReferenceExpression[FT]
    def assignVariable(name: any.Name[FT], expression: any.Expression[FT]): AssignVariable[FT]
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
