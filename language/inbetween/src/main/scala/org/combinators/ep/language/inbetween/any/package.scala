package org.combinators.ep.language.inbetween /*DI:LI:AI*/

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator;

package object any {
  trait FinalTypes {
    type Method
    type Import
    type Statement
    type Type
    type Name
    type Expression
    type ApplyExpression <: Expression
    type ArgumentExpression <: Expression
    type CompilationUnit
    type Project
  }

  trait Import[FT <: FinalTypes] extends Factory[FT] {
    def getSelfImport: finalTypes.Import
  }

  trait Statement[FT <: FinalTypes] extends Factory[FT] {
    def getSelfStatement: finalTypes.Statement
  }

  trait Type[FT <: FinalTypes] extends Factory[FT] {
    def getSelfType: finalTypes.Type
  }

  trait Method[FT <: FinalTypes] extends Factory[FT] {
    def getSelfMethod: finalTypes.Method

    def name: Name[FT]
    def imports: Set[Import[FT]] = Set.empty
    def statements: Seq[Statement[FT]] = Seq.empty
    def returnType: Option[Type[FT]] = Option.empty
    def parameters: Seq[(Name[FT], Type[FT])] = Seq.empty
    def typeLookupMap: TypeRep => Generator[Method[FT], Type[FT]] = Map.empty

    def addTypeLookups(lookups: TypeRep => Option[Generator[Method[FT], Type[FT]]]): Method[FT] = {
      copy(typeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.typeLookupMap(tpeRep)))
    }
    def getArguments(): Seq[(Name[FT], Type[FT], Expression[FT])] = {
      parameters.map(param => (param._1, param._2, argumentExpression(param._1)))
    }
    def toTargetLanguageType(tpe: TypeRep): Generator[Method[FT], Type[FT]] = typeLookupMap(tpe)

    def reify[T](tpe: TypeRep.OfHostType[T], value: T): Expression[FT]
    def resolveImport(tpe: Type[FT]): Seq[Import[FT]]
    def getFreshName(basedOn: Name[FT]): Name[FT]

    def copy(
      name: Name[FT] = this.name,
      imports: Set[Import[FT]] = this.imports,
      statements: Seq[Statement[FT]] = this.statements,
      returnType: Option[Type[FT]] = this.returnType,
      parameters: Seq[(Name[FT], Type[FT])] = this.parameters,
      typeLookupMap: TypeRep => Generator[Method[FT], Type[FT]] = this.typeLookupMap,
    ): Method[FT] = method(name, imports, statements, returnType, parameters)
  }

  trait Name[FT <: FinalTypes] extends Factory[FT] {
    def getSelfName: finalTypes.Name
  }

  trait Return[FT <: FinalTypes] extends Statement[FT] with Factory[FT] {
    def expression: Expression[FT]
    
    def copy(expression: Expression[FT]): Return[FT] = returnExpression(expression)
  }

  trait Expression[FT <: FinalTypes] extends Factory[FT] {
    def getSelfExpression: finalTypes.Expression

    def apply(other: Expression[FT]): Expression[FT] = applyExpression(this, arguments=Seq(other))
  }

  trait ApplyExpression[FT <: FinalTypes] extends Expression[FT] with Factory[FT] {
    def getSelfApplyExpression: finalTypes.ApplyExpression

    def function: Expression[FT]
    def arguments: Seq[Expression[FT]]

    def copy(
      function: Expression[FT] = this.function,
      arguments: Seq[Expression[FT]] = this.arguments
    ): ApplyExpression[FT] = applyExpression(function, arguments)
  }

  trait ArgumentExpression[FT <: FinalTypes] extends Expression[FT] {
    def getSelfArgumentExpression: finalTypes.ArgumentExpression

    def parameterName: any.Name[FT]

    def copy(
      parameterName: any.Name[FT] = this.parameterName
    ): ArgumentExpression[FT] = argumentExpression(parameterName)
  }

  trait CompilationUnit[FT <: FinalTypes] extends Factory[FT] {
    def getSelfCompilationUnit: finalTypes.CompilationUnit

    def name: Seq[Name[FT]] = Seq.empty
    def imports: Seq[Import[FT]] = Seq.empty

    def getFreshName(basedOn: Name[FT]): Name[FT]

    def initializeInProject(project: Project[FT]): CompilationUnit[FT]

    // TODO: Tests?

    def copy(
      name: Seq[Name[FT]] = this.name,
      imports: Seq[Import[FT]] = this.imports,
    ): CompilationUnit[FT] = compilationUnit(name, imports)
  }

  trait Project[FT <: FinalTypes] extends Factory[FT] {
    def getSelfProject: finalTypes.Project

    def compilationUnits: Set[CompilationUnit[FT]] = Set.empty

    def addTypeLookupsForMethods(lookups: TypeRep => Option[Generator[Method[FT], Type[FT]]]): Project[FT]

    def copy(
      compilationUnits: Set[CompilationUnit[FT]] = this.compilationUnits
    ): Project[FT] = project(compilationUnits)
  }

  trait Factory[FT <: FinalTypes] {
    val finalTypes: FT

    def project(compilationUnits: Set[CompilationUnit[FT]] = Set.empty): Project[FT]

    def compilationUnit(name: Seq[Name[FT]], imports: Seq[Import[FT]]): CompilationUnit[FT]
    def method(
      name: Name[FT],
      imports: Set[Import[FT]] = Set.empty,
      statements: Seq[Statement[FT]] = Seq.empty,
      returnType: Option[Type[FT]] = Option.empty,
      parameters: Seq[(Name[FT], Type[FT])] = Seq.empty,
      typeLookupMap: TypeRep => Generator[Method[FT], Type[FT]] = Map.empty,
    ): Method[FT]
    def returnExpression(expression: Expression[FT]): Return[FT]
    def applyExpression(function: Expression[FT], arguments: Seq[Expression[FT]]): ApplyExpression[FT]
    def argumentExpression(parameterName: Name[FT]): ArgumentExpression[FT]

    implicit def convert(other: Import[FT]): Import[FT]
    implicit def convert(other: Statement[FT]): Statement[FT]
    implicit def convert(other: Type[FT]): Type[FT]
    implicit def convert(other: Method[FT]): Method[FT]
    implicit def convert(other: Name[FT]): Name[FT]
    implicit def convert(other: Expression[FT]): Expression[FT]
    implicit def convert(other: ApplyExpression[FT]): ApplyExpression[FT]
    implicit def convert(other: ArgumentExpression[FT]): ArgumentExpression[FT]
    implicit def convert(other: CompilationUnit[FT]): CompilationUnit[FT]
    implicit def convert(other: Project[FT]): Project[FT]
  }
}