package org.combinators.ep.language.inbetween.any

import org.combinators.cogen.TypeRep
import org.combinators.cogen.Command.Generator

trait AnyAST {
  object any {
    trait FinalTypes {
      type Method <: any.Method
      type Import <: any.Import
      type Statement <: any.Statement
      type Type <: any.Type
      type Name <: any.Name
      type Expression <: any.Expression
      type ApplyExpression <: any.ApplyExpression
      type ArgumentExpression <: any.ArgumentExpression
      type TestSuite <: any.TestSuite
      type CompilationUnit <: any.CompilationUnit
      type Project <: any.Project
    }

    trait Import  {
      def getSelfImport: finalTypes.Import
    }

    trait Statement  {
      def getSelfStatement: finalTypes.Statement
    }

    trait Type  {
      def getSelfType: finalTypes.Type
    }

    trait Method  {
      def getSelfMethod: finalTypes.Method

      def name: Name
      def imports: Set[Import] = Set.empty
      def statements: Seq[Statement] = Seq.empty
      def returnType: Option[Type] = Option.empty
      def parameters: Seq[(Name, Type)] = Seq.empty
      def typeLookupMap: TypeRep => Generator[Method, Type] = Map.empty

      def addTypeLookups(lookups: TypeRep => Option[Generator[Method, Type]]): Method = {
        copy(typeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.typeLookupMap(tpeRep)))
      }
      def getArguments: Seq[(Name, Type, Expression)] = {
        parameters.map(param => (param._1, param._2, factory.argumentExpression(param._1)))
      }
      def addTestExpressions(exprs: Seq[Expression]): Method

      def toTargetLanguageType(tpe: TypeRep): Generator[Method, Type] = typeLookupMap(tpe)

      def reify[T](tpe: TypeRep.OfHostType[T], value: T): Expression
      def resolveImport(tpe: Type): Seq[Import]
      def getFreshName(basedOn: Name): Name

      def copy(
        name: Name = this.name,
        imports: Set[Import] = this.imports,
        statements: Seq[Statement] = this.statements,
        returnType: Option[Type] = this.returnType,
        parameters: Seq[(Name, Type)] = this.parameters,
        typeLookupMap: TypeRep => Generator[Method, Type] = this.typeLookupMap,
      ): Method = factory.method(name, imports, statements, returnType, parameters)
    }

    trait Name  {
      def getSelfName: finalTypes.Name
    }

    trait Return extends Statement {
      def expression: Expression

      def copy(expression: Expression): Return = factory.returnExpression(expression)
    }

    trait Expression  {
      def getSelfExpression: finalTypes.Expression

      def apply(other: Expression): Expression = factory.applyExpression(this, arguments = Seq(other))
    }

    trait ApplyExpression extends Expression {
      import factory.*
      def getSelfApplyExpression: finalTypes.ApplyExpression

      def function: Expression
      def arguments: Seq[Expression]

      def copy(
        function: Expression = this.function,
        arguments: Seq[Expression] = this.arguments
      ): ApplyExpression = applyExpression(function, arguments)
    }

    trait ArgumentExpression extends Expression {
      def getSelfArgumentExpression: finalTypes.ArgumentExpression

      def parameterName: any.Name

      def copy(
        parameterName: any.Name = this.parameterName
      ): ArgumentExpression = factory.argumentExpression(parameterName)
    }

    trait TestSuite  {
      def getSelfTestSuite: finalTypes.TestSuite

      def name: Name

      def tests: Seq[Method]

      def methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty

      def initializeInCompilationUnit(compilationUnit: any.CompilationUnit): TestSuite

      def copy(
        name: Name = this.name,
        tests: Seq[Method] = this.tests,
        methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.methodTypeLookupMap
      ): TestSuite = factory.testSuite(name, tests, methodTypeLookupMap)
    }

    trait CompilationUnit  {
      def getSelfCompilationUnit: finalTypes.CompilationUnit

      def name: Seq[Name] = Seq.empty
      def imports: Seq[Import] = Seq.empty

      def tests: Seq[TestSuite] = Seq.empty

      def getFreshName(basedOn: Name): Name

      def initializeInProject(project: Project): CompilationUnit

      def copy(
        name: Seq[Name] = this.name,
        imports: Seq[Import] = this.imports,
        tests: Seq[TestSuite] = this.tests,
      ): CompilationUnit = factory.compilationUnit(name, imports, tests)
    }

    trait Project  {
      def getSelfProject: finalTypes.Project

      def compilationUnits: Set[CompilationUnit] = Set.empty

      def addTypeLookupsForMethods(lookups: TypeRep => Option[Generator[Method, Type]]): Project

      def copy(
        compilationUnits: Set[CompilationUnit] = this.compilationUnits
      ): Project = factory.project(compilationUnits)
    }

    trait Factory {
      def project(compilationUnits: Set[CompilationUnit] = Set.empty): Project

      def compilationUnit(name: Seq[Name], imports: Seq[Import], tests: Seq[TestSuite]): CompilationUnit

      def testSuite(name: Name, tests: Seq[Method], methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty): TestSuite
      def method(
        name: Name,
        imports: Set[Import] = Set.empty,
        statements: Seq[Statement] = Seq.empty,
        returnType: Option[Type] = Option.empty,
        parameters: Seq[(Name, Type)] = Seq.empty,
        typeLookupMap: TypeRep => Generator[Method, Type] = Map.empty,
      ): Method
      def returnExpression(expression: Expression): Return
      def applyExpression(function: Expression, arguments: Seq[Expression]): ApplyExpression
      def argumentExpression(parameterName: Name): ArgumentExpression

      implicit def convert(other: Import): finalTypes.Import = other.getSelfImport
      implicit def convert(other: Statement): finalTypes.Statement = other.getSelfStatement
      implicit def convert(other: Type): finalTypes.Type = other.getSelfType
      implicit def convert(other: Method): finalTypes.Method = other.getSelfMethod
      implicit def convert(other: Name): finalTypes.Name = other.getSelfName
      implicit def convert(other: Expression): finalTypes.Expression = other.getSelfExpression
      implicit def convert(other: ApplyExpression): finalTypes.ApplyExpression = other.getSelfApplyExpression
      implicit def convert(other: ArgumentExpression): finalTypes.ArgumentExpression = other.getSelfArgumentExpression
      implicit def convert(other: TestSuite): finalTypes.TestSuite = other.getSelfTestSuite
      implicit def convert(other: CompilationUnit): finalTypes.CompilationUnit = other.getSelfCompilationUnit
      implicit def convert(other: Project): finalTypes.Project = other.getSelfProject
    }
  }

  val finalTypes: any.FinalTypes
  val factory: any.Factory
}
