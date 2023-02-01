package org.combinators.ep.language

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.abstractions.TypeRep.OfHostType
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.oo
import org.combinators.ep.language.inbetween.imperative
import org.combinators.ep.language.inbetween.ffi.arithmetic
import org.combinators.ep.language.inbetween.ffi.assertions
import org.combinators.ep.language.inbetween.ffi.boolean
import org.combinators.ep.language.inbetween.ffi.equals
import org.combinators.ep.language.inbetween.ffi.operatorExpression
import org.combinators.ep.language.inbetween.ffi.strings
import shapeless.Fin

import java.util.UUID

package object scala {
  trait FinalTypes
    extends any.FinalTypes
      with oo.FinalTypes
      with imperative.FinalTypes
      with operatorExpression.FinalTypes

  trait Project[FT <: FinalTypes] extends oo.Project[FT] with Factory[FT] {}

  trait Import[FT <: FinalTypes] extends any.Import[FT] with Factory[FT] {
    def components: Seq[any.Name[FT]]

    def toScala: String =
      s"""import ${components.map(_.toScala).mkString(".")}"""
  }

  trait Statement[FT <: FinalTypes] extends any.Statement[FT] with Factory[FT] {
    def toScala: String
  }

  trait DeclareVariable[FT <: FinalTypes] extends imperative.DeclareVariable[FT] with Statement[FT] with Factory[FT] {
    def toScala: String
  }

  trait AssignVariable[FT <: FinalTypes] extends imperative.AssignVariable[FT] with Statement[FT] with Factory[FT] {
    def toScala: String
  }

  trait IfThenElse[FT <: FinalTypes] extends imperative.IfThenElse[FT] with Statement[FT] with Factory[FT] {
    def toScala: String = {
      val elseIfs = elseIfBranches.map{ case (condition, body) =>
        s"""
           | else if (${condition.toScala}) {
           |  ${body.map(_.toScala).mkString("\n  ")}
           |}""".stripMargin
      }

      s"""
          |if (${condition.toScala}) {
          |  ${this.ifBranch.map(_.toScala).mkString("\n  ")}"
          |}${elseIfs.mkString("")} {
          |  ${elseBranch.mkString("\n  ")}
          |}
          """
    }
  }

  trait While[FT <: FinalTypes] extends imperative.While[FT] with Statement[FT] with Factory[FT] {
    def toScala: String
  }

  trait Expression[FT <: FinalTypes] extends oo.Expression[FT] with Factory[FT] {
    def toScala: String
  }

  trait ArgumentExpression[FT <: FinalTypes] extends Expression[FT] with any.ArgumentExpression[FT] with Factory[FT] {
    def toScala: String
  }

  trait MemberAccessExpression[FT <: FinalTypes] extends Expression[FT] with oo.MemberAccessExpression[FT] with Factory[FT] {
    def toScala: String
  }

  trait SelfReferenceExpression[FT <: FinalTypes] extends Expression[FT] with oo.SelfReferenceExpression[FT] with Factory[FT] {
    def toScala: String
  }

  trait ObjectInstantiationExpression[FT <: FinalTypes] extends Expression[FT] with oo.ObjectInstantiationExpression[FT] with Factory[FT] {
    def toScala: String
  }

  trait CastExpression[FT <: FinalTypes] extends Expression[FT] with oo.CastExpression[FT] with Factory[FT] {
    def toScala: String
  }

  trait InstanceOfExpression[FT <: FinalTypes] extends Expression[FT] with oo.InstanceOfExpression[FT] with Factory[FT] {
    def toScala: String
  }

  trait SuperReferenceExpression[FT <: FinalTypes] extends Expression[FT] with oo.SuperReferenceExpression[FT] with Factory[FT] {
    def toScala: String
  }

  trait BinaryExpression[FT <: FinalTypes] extends Expression[FT] with operatorExpression.BinaryExpression[FT] with Factory[FT] {
    def toScala: String = s"(${left.toScala} ${operator.toScala} ${right.toScala})"
  }

  trait UnaryExpression[FT <: FinalTypes] extends Expression[FT] with operatorExpression.UnaryExpression[FT] with Factory[FT] {
    def toScala: String = s"(${operator} ${operand})"
  }

  trait Operator[FT <: FinalTypes] extends operatorExpression.Operator[FT] with Factory[FT] {
    def toScala: String
  }



  trait Type[FT <: FinalTypes] extends any.Type[FT] with Factory[FT] {
    def toScala: String
    def toImport: any.Import[FT]
  }

  trait ClassReference[FT <: FinalTypes] extends Type[FT] with Factory[FT] {
    def className: Seq[any.Name[FT]]
    def toScala: String = className.map(_.toScala).mkString(".")

    def toImport: any.Import[FT] = importStatement(className)
  }

  trait Name[FT <: FinalTypes] extends any.Name[FT] with Factory[FT] {
    def component: String
    def toScala: String = component
  }

  trait Util[FT <: FinalTypes] extends Factory[FT] {
    def reify[T](tpe: OfHostType[T], value: T): any.Expression[FT] = {
      ???
    }

    def resolveImport(tpe: any.Type[FT]): any.Import[FT] = tpe.toImport
    def getFreshName(basedOn: any.Name[FT]): any.Name[FT] = {
      val id = UUID.randomUUID().toString.replace("-", "")
      name(s"${basedOn.component}_${id}")
    }
  }

  trait Method[FT <: FinalTypes] extends oo.Method[FT] with Factory[FT] with Util[FT] {

    def findClass(qualifiedName: any.Name[FT]*): any.Type[FT] =
      classReference(qualifiedName)

    def toScala: String = {
      val overrideMod = if (isOverride) "override" else ""
      val privateMod = if (!isPublic) "private" else ""
      val mods = Seq(overrideMod, privateMod).mkString(" ")

      val params = parameters.map(p => s"${p._1.toScala} : ${p._2.toScala}").mkString(",")
      val returnTpe = returnType.map(_.toScala).getOrElse("Unit")
      val body = if (!isAbstract) {
        s"""= {
           |  ${imports.map(_.toScala).mkString("\n    ")}
           |  ${statements.map(_.toScala).mkString("\n    ")}
           |}
    }
    """.stripMargin
      } else ""

      s"""${mods} def ${name.toScala}(${params}): ${returnTpe} ${body}""".stripMargin
    }
  }

  trait Constructor[FT <: FinalTypes] extends oo.Constructor[FT] with Factory[FT] {

  }

  trait Field[FT <: FinalTypes] extends oo.Field[FT] with Factory[FT] {
    def toScala: String = {
      val initExp = init.map(exp => s" = ${exp.toScala}").getOrElse("")
      s"var ${name.toScala}: ${tpe.toScala}${initExp}"
    }
  }

  trait Class[FT <: FinalTypes] extends oo.Class[FT] with Factory[FT] with Util[FT] {
    def toScala: String = {
      val kind = if (isInterface) "trait" else "class"
      val abstractMod = if (isAbstract && !isInterface) "abstract" else ""
      val extendsClause = if (parents.nonEmpty) parents.mkString("extends", "with", "") else ""
      val fieldDecls = fields.map(_.toScala).mkString("\n  ")

      s"""
         |${abstractMod} ${kind} ${name} ${extendsClause} {
         |  ${fieldDecls}
         |""".stripMargin
    }
  }

  trait CompilationUnit[FT <: FinalTypes] extends oo.CompilationUnit[FT] with Factory[FT] {

  }

  trait Factory[FT <: FinalTypes]
    extends any.Factory[FT]
    with oo.Factory[FT]
    with imperative.Factory[FT]
    with arithmetic.Factory[FT]
    with assertions.Factory[FT]
    with boolean.Factory[FT]
    with equals.Factory[FT]
    with operatorExpression.Factory[FT]
    with strings.Factory[FT] {

    def name(name: String): Name[FT]
    def importStatement(components: Seq[any.Name[FT]]): Import[FT]
    def classReference(name: Seq[any.Name[FT]]): ClassReference[FT]

    implicit def convert(other: any.Import[FT]): Import[FT]
    implicit def convert(other: any.Statement[FT]): Statement[FT]
    implicit def convert(other: any.Type[FT]): Type[FT]
    implicit def convert(other: any.Name[FT]): Name[FT]
    implicit def convert(other: any.Expression[FT]): Expression[FT]
    implicit def convert(other: any.ArgumentExpression[FT]): ArgumentExpression[FT]
    implicit def convert(other: any.Project[FT]): Project[FT]
    implicit def convert(other: any.CompilationUnit[FT]): CompilationUnit[FT]
    implicit def convert(other: any.Method[FT]): Method[FT]
    implicit def convert(other: oo.Class[FT]): Class[FT]
    implicit def convert(other: oo.Constructor[FT]): Constructor[FT]
    implicit def convert(other: oo.Field[FT]): Field[FT]
    implicit def convert(other: oo.MemberAccessExpression[FT]): MemberAccessExpression[FT]
    implicit def convert(other: oo.SelfReferenceExpression[FT]): SelfReferenceExpression[FT]
    implicit def convert(other: oo.ObjectInstantiationExpression[FT]): ObjectInstantiationExpression[FT]
    implicit def convert(other: oo.CastExpression[FT]): CastExpression[FT]
    implicit def convert(other: oo.InstanceOfExpression[FT]): InstanceOfExpression[FT]
    implicit def convert(other: oo.SuperReferenceExpression[FT]): SuperReferenceExpression[FT]
    implicit def convert(decl: imperative.DeclareVariable[FT]): DeclareVariable[FT]
    implicit def convert(assignVariable: imperative.AssignVariable[FT]): AssignVariable[FT]
    implicit def convert(ifThenElse: imperative.IfThenElse[FT]): IfThenElse[FT]
    implicit def convert(whileLoop: imperative.While[FT]): While[FT]
    implicit def convert(operator: operatorExpression.Operator[FT]): Operator[FT]
    implicit def convert(binaryExpression: operatorExpression.BinaryExpression[FT]): BinaryExpression[FT]
    implicit def convert(unaryExpression: operatorExpression.UnaryExpression[FT]): UnaryExpression[FT]
  }

  object Finalized {
    class FinalTypes extends scala.FinalTypes {
      type DeclareVariable = this.type
      type AssignVariable = this.type
      type IfThenElse = this.type
      type While = this.type
      type Class = this.type
      type Constructor = this.type
      type Field = this.type
      type MemberAccessExpression = this.type
      type SelfReferenceExpression = this.type
      type ObjectInstantiationExpression = this.type
      type CastExpression = this.type
      type InstanceOfExpression = this.type
      type SuperReferenceExpression = this.type
      override type Operator = Finalized.Operator
      override type BinaryExpression = Finalized.BinaryExpression
      override type UnaryExpression = Finalized.UnaryExpression
      override type Import = Finalized.Import
      type Statement = this.type
      override type Type = Finalized.Type
      override type Name = Finalized.Name
      override type Expression = Finalized.Expression
      type ArgumentExpression = this.type
      type CompilationUnit = this.type
      type Project = this.type
      override type Method = Finalized.Method
    }
    trait Factory extends scala.Factory[FinalTypes] {

      def name(name: String): Name = Name(name)
      def importStatement(components: Seq[any.Name[FinalTypes]]): Import = Import(components)
      def classReference(name: Seq[any.Name[FinalTypes]]): ClassReference = ???


      def operator(name: any.Name[FinalTypes]): operatorExpression.Operator[FinalTypes] = ???
      def binaryExpression(operator: operatorExpression.Operator[FinalTypes], left: any.Expression[FinalTypes], right: any.Expression[FinalTypes]): operatorExpression.BinaryExpression[FinalTypes] = ???
      def unaryExpression(operator: operatorExpression.Operator[FinalTypes], operand: any.Expression[FinalTypes]): operatorExpression.UnaryExpression[FinalTypes] = ???
      implicit def convert(operator: operatorExpression.Operator[FinalTypes]): Operator = ???
      implicit def convert(binaryExpression: operatorExpression.BinaryExpression[FinalTypes]): BinaryExpression = ???
      implicit def convert(unaryExpression: operatorExpression.UnaryExpression[FinalTypes]): UnaryExpression = ???
      def compilationUnit(name: Seq[any.Name[FinalTypes]], imports: Seq[any.Import[FinalTypes]], classes: Seq[oo.Class[FinalTypes]]): oo.CompilationUnit[FinalTypes] = ???
      def method(
        name: any.Name[FinalTypes], imports: Set[any.Import[FinalTypes]], statements: Seq[any.Statement[FinalTypes]], returnType: Option[any.Type[FinalTypes]], parameters: Seq[(any.Name[FinalTypes], any.Type[FinalTypes])], typeLookupMap: Map[TypeRep, any.Type[FinalTypes]], isAbstract: Boolean, isPublic: Boolean, isOverride: Boolean
      ): Method = new Method(name = name, imports = imports, statements = statements, returnType = returnType, parameters = parameters, typeLookupMap = typeLookupMap, isAbstract = isAbstract, isPublic = isPublic, isOverride = isOverride)
      def cls(
        name: any.Name[FinalTypes], imports: Seq[any.Import[FinalTypes]], parents: Seq[any.Type[FinalTypes]], implemented: Seq[any.Type[FinalTypes]], fields: Seq[oo.Field[FinalTypes]], methods: Seq[any.Method[FinalTypes]], constructors: Seq[oo.Constructor[FinalTypes]], typeLookupMap: Map[TypeRep, any.Type[FinalTypes]], isAbstract: Boolean, isInterface: Boolean, isStatic: Boolean
      ): Class[FinalTypes] = ???
      def constructor(
        imports: Set[any.Import[FinalTypes]], statements: Seq[any.Statement[FinalTypes]], parameters: Seq[(any.Name[FinalTypes], any.Type[FinalTypes])], typeLookupMap: Map[TypeRep, any.Type[FinalTypes]], superInitialization: Option[Seq[any.Expression[FinalTypes]]], fieldInitializers: Seq[(any.Name[FinalTypes], any.Expression[FinalTypes])]
      ): Constructor[FinalTypes] = ???
      def field(name: any.Name[FinalTypes], tpe: any.Type[FinalTypes], init: Option[any.Expression[FinalTypes]]): oo.Field[FinalTypes] = ???
      def memberAccessExpression(owner: oo.Expression[FinalTypes], field: any.Name[FinalTypes]): oo.MemberAccessExpression[FinalTypes] = ???
      def objectInstantiationExpression(tpe: any.Type[FinalTypes], constructorArguments: Seq[any.Expression[FinalTypes]]): oo.ObjectInstantiationExpression[FinalTypes] = ???
      def castExpression(tpe: any.Type[FinalTypes], expression: any.Expression[FinalTypes]): oo.CastExpression[FinalTypes] = ???
      def instanceOfExpression(tpe: any.Type[FinalTypes], expression: any.Expression[FinalTypes]): oo.InstanceOfExpression[FinalTypes] = ???
      def superReferenceExpression(parentType: any.Type[FinalTypes]): oo.SuperReferenceExpression[FinalTypes] = ???
      def selfReferenceExpression: oo.SelfReferenceExpression[FinalTypes] = ???
      implicit def convert(other: any.Project[FinalTypes]): Project[FinalTypes] = ???
      implicit def convert(other: any.CompilationUnit[FinalTypes]): CompilationUnit[FinalTypes] = ???
      implicit def convert(other: any.Method[FinalTypes]): Method = other.getSelfMethod
      implicit def convert(other: oo.Class[FinalTypes]): Class[FinalTypes] = ???
      implicit def convert(other: oo.Constructor[FinalTypes]): Constructor[FinalTypes] = ???
      implicit def convert(other: oo.Field[FinalTypes]): Field[FinalTypes] = ???
      implicit def convert(other: oo.MemberAccessExpression[FinalTypes]): MemberAccessExpression[FinalTypes] = ???
      implicit def convert(other: oo.SelfReferenceExpression[FinalTypes]): SelfReferenceExpression[FinalTypes] = ???
      implicit def convert(other: oo.ObjectInstantiationExpression[FinalTypes]): ObjectInstantiationExpression[FinalTypes] = ???
      implicit def convert(other: oo.CastExpression[FinalTypes]): CastExpression[FinalTypes] = ???
      implicit def convert(other: oo.InstanceOfExpression[FinalTypes]): InstanceOfExpression[FinalTypes] = ???
      implicit def convert(other: oo.SuperReferenceExpression[FinalTypes]): SuperReferenceExpression[FinalTypes] = ???
      def addOp(): arithmetic.AddOp[FinalTypes] = ???
      def subOp(): arithmetic.SubOp[FinalTypes] = ???
      def multOp(): arithmetic.MultOp[FinalTypes] = ???
      def divOp(): arithmetic.DivOp[FinalTypes] = ???
      def modOp(): arithmetic.ModOp[FinalTypes] = ???
      def ltOp(): arithmetic.LtOp[FinalTypes] = ???
      def leOp(): arithmetic.LeOp[FinalTypes] = ???
      def equals(tpe: any.Type[FinalTypes], left: any.Expression[FinalTypes], right: any.Expression[FinalTypes]): equals.Equals[FinalTypes] = ???
      def andOp(): boolean.AndOp[FinalTypes] = ???
      def orOp(): boolean.OrOp[FinalTypes] = ???
      def notOp(): boolean.NotOp[FinalTypes] = ???
      def trueExp(): boolean.True[FinalTypes] = ???
      def falseExp(): boolean.False[FinalTypes] = ???
      def declareVariable(name: any.Name[FinalTypes], tpe: any.Type[FinalTypes], initializer: Option[any.Expression[FinalTypes]]): imperative.DeclareVariable[FinalTypes] = ???
      def assignVariable(name: any.Name[FinalTypes], expression: any.Expression[FinalTypes]): imperative.AssignVariable[FinalTypes] = ???
      def liftExpression(expression: any.Expression[FinalTypes]): imperative.LiftExpression[FinalTypes] = ???
      def returnExpression(expression: any.Expression[FinalTypes]): imperative.Return[FinalTypes] = ???
      def ifThenElse(condition: any.Expression[FinalTypes], ifBranch: Seq[any.Statement[FinalTypes]], elseIfBranches: Seq[(any.Expression[FinalTypes], Seq[any.Statement[FinalTypes]])], elseBranch: Seq[any.Statement[FinalTypes]]): imperative.IfThenElse[FinalTypes] = ???
      def whileLoop(condition: any.Expression[FinalTypes], body: Seq[any.Statement[FinalTypes]]): imperative.While[FinalTypes] = ???
      def convert(decl: imperative.DeclareVariable[FinalTypes]): DeclareVariable[FinalTypes] = ???
      def convert(assignVariable: imperative.AssignVariable[FinalTypes]): AssignVariable[FinalTypes] = ???
      def convert(ifThenElse: imperative.IfThenElse[FinalTypes]): IfThenElse[FinalTypes] = ???
      def convert(whileLoop: imperative.While[FinalTypes]): While[FinalTypes] = ???
      def toStringOp(): strings.ToStringOp[FinalTypes] = ???
      def appendStringOp(): strings.AppendStringOp[FinalTypes] = ???
      def stringLengthOp(): strings.StringLengthOp[FinalTypes] = ???
      def assertTrueOp(): assertions.AssertTrueOp[FinalTypes] = ???
      val finalTypes: FinalTypes = new FinalTypes
      def project(compilationUnits: Set[any.CompilationUnit[FinalTypes]]): any.Project[FinalTypes] = ???
      def argumentExpression(parameterName: any.Name[FinalTypes]): any.ArgumentExpression[FinalTypes] = ???
      implicit def convert(other: any.Import[FinalTypes]): Import = other.getSelfImport
      implicit def convert(other: any.Statement[FinalTypes]): Statement[FinalTypes] = ???
      implicit def convert(other: any.Type[FinalTypes]): Type = ???
      implicit def convert(other: any.Name[FinalTypes]): Name = ???
      implicit def convert(other: any.Expression[FinalTypes]): Expression = ???
      implicit def convert(other: any.ArgumentExpression[FinalTypes]): ArgumentExpression[FinalTypes] = ???
    }

    case class Name(override val component: String) extends scala.Name[FinalTypes] with Factory {
      override def getSelfName: this.type = this
    }

    case class Method(name: any.Name[FinalTypes],
      override val imports: Set[any.Import[FinalTypes]],
      override val statements: Seq[any.Statement[FinalTypes]],
      override val returnType: Option[any.Type[FinalTypes]],
      override val parameters: Seq[(any.Name[FinalTypes], any.Type[FinalTypes])],
      override val typeLookupMap: Map[TypeRep, any.Type[FinalTypes]],
      override val isAbstract: Boolean,
      override val isPublic: Boolean,
      override val isOverride: Boolean) extends scala.Method[FinalTypes] with Factory {
      def getSelfMethod: this.type = this
    }

    case class Import(override val components: Seq[any.Name[FinalTypes]]) extends scala.Import[FinalTypes] with Factory {
      def getSelfImport: this.type = this
    }

    trait Type extends scala.Type[FinalTypes] with Factory {
      def getSelfType: this.type = this
    }

    case class ClassReference(override val className: Seq[any.Name[FinalTypes]]) extends Type with scala.ClassReference[FinalTypes] with Factory {
    }

    trait Operator extends scala.Operator[FinalTypes] with Factory {
      override def getSelfOperator: this.type = this
    }

    trait Expression extends scala.Expression[FinalTypes] with Factory {
      override def getSelfExpression: this.type = this
    }

    case class BinaryExpression(override val operator: operatorExpression.Operator[FinalTypes],
      override val left: any.Expression[FinalTypes],
      override val right: any.Expression[FinalTypes])
      extends Expression with scala.BinaryExpression[FinalTypes] with Factory {
      override def getSelfBinaryExpression: this.type = this
    }

    case class UnaryExpression(
      override val operator: operatorExpression.Operator[FinalTypes],
      override val operand: any.Expression[FinalTypes])
      extends Expression with scala.UnaryExpression[FinalTypes] with Factory {
      override def getSelfUnaryExpression: this.type = this
    }
  }
}
