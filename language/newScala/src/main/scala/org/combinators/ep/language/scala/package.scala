package org.combinators.ep.language

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.abstractions.TypeRep.OfHostType
import org.combinators.ep.generator.NameProvider
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.oo
import org.combinators.ep.language.inbetween.imperative
import org.combinators.ep.language.inbetween.ffi.arithmetic
import org.combinators.ep.language.inbetween.ffi.assertions
import org.combinators.ep.language.inbetween.ffi.boolean
import org.combinators.ep.language.inbetween.ffi.eqls
import org.combinators.ep.language.inbetween.ffi.operatorExpression
import org.combinators.ep.language.inbetween.ffi.strings

import java.util.UUID

package object scala {
  trait FinalTypes
    extends any.FinalTypes
      with oo.FinalTypes
      with imperative.FinalTypes
      with operatorExpression.FinalTypes {
    type ReifiedScalaValue[T] <: Expression
  }

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
    def toScala: String = {
      val init = this.initializer.map(ie => s" = ${ie.toScala}").getOrElse("")
      s"""
         |var ${this.name.toScala}${init}
         |""".stripMargin
    }
  }

  trait AssignVariable[FT <: FinalTypes] extends imperative.AssignVariable[FT] with Statement[FT] with Factory[FT] {
    def toScala: String = {
      s"""
         |${this.variable.toScala} = ${this.assignmentExpression.toScala}
         |""".stripMargin
    }
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
    def toScala: String = {
      s"""
         |while (${condition.toScala}) {
         |  ${body.map(_.toScala).mkString("\n  ")}
         |}""".stripMargin
    }
  }

  trait Expression[FT <: FinalTypes] extends oo.Expression[FT] with Factory[FT] {
    def toScala: String
  }

  trait ReifiedScalaValue[FT <: FinalTypes, T] extends Expression[FT] with Factory[FT] {
    def getSelfAsReifiedScalaValue: finalTypes.ReifiedScalaValue[T]
    val ofHostType: OfHostType[T]
    val value: T

    def toScala: String = value.toString
  }

  trait ArgumentExpression[FT <: FinalTypes] extends Expression[FT] with any.ArgumentExpression[FT] with Factory[FT] {
    def toScala: String = s"${parameterName.toScala}"
  }

  trait MemberAccessExpression[FT <: FinalTypes] extends Expression[FT] with oo.MemberAccessExpression[FT] with Factory[FT] {
    def toScala: String = s"${owner.toScala}.${field.toScala}"
  }

  trait SelfReferenceExpression[FT <: FinalTypes] extends Expression[FT] with oo.SelfReferenceExpression[FT] with Factory[FT] {
    def toScala: String = s"this"
  }

  trait ObjectInstantiationExpression[FT <: FinalTypes] extends Expression[FT] with oo.ObjectInstantiationExpression[FT] with Factory[FT] {
    def toScala: String = {
      val bodyScala = body.map(_.classBodyDefinitionToScala).getOrElse("")
      s"""new ${tpe.toScala}(${constructorArguments.map(_.toScala).mkString(",")})${bodyScala}"""
    }
  }

  trait CastExpression[FT <: FinalTypes] extends Expression[FT] with oo.CastExpression[FT] with Factory[FT] {
    def toScala: String = s"${this.expression.toScala}.asInstanceOf[${tpe.toScala}]"
  }

  trait InstanceOfExpression[FT <: FinalTypes] extends Expression[FT] with oo.InstanceOfExpression[FT] with Factory[FT] {
    def toScala: String = {
      s"${this.expression.toScala}.isInstanceOf[${this.tpe.toScala}]"
    }
  }

  trait SuperReferenceExpression[FT <: FinalTypes] extends Expression[FT] with oo.SuperReferenceExpression[FT] with Factory[FT] {
    def toScala: String = {
      s"super[${parentType.toScala}]"
    }
  }

  trait BinaryExpression[FT <: FinalTypes] extends Expression[FT] with operatorExpression.BinaryExpression[FT] with Factory[FT] {
    def toScala: String = s"(${left.toScala} ${operator.toScala} ${right.toScala})"
  }

  trait UnaryExpression[FT <: FinalTypes] extends Expression[FT] with operatorExpression.UnaryExpression[FT] with Factory[FT] {
    def toScala: String = s"($operator $operand)"
  }

  trait Operator[FT <: FinalTypes] extends operatorExpression.Operator[FT] with Factory[FT] {
    def toScala: String
  }

  trait AddOp[FT <: FinalTypes] extends arithmetic.AddOp[FT] with Operator[FT] with Factory[FT] {
    override def toScala: String = "+"
  }

  trait SubOp[FT <: FinalTypes] extends arithmetic.SubOp[FT] with Operator[FT] with Factory[FT] {
    override def toScala: String = "-"
  }

  trait MultOp[FT <: FinalTypes] extends arithmetic.MultOp[FT] with Operator[FT] with Factory[FT] {
    override def toScala: String = "*"
  }

  trait DivOp[FT <: FinalTypes] extends arithmetic.DivOp[FT] with Operator[FT] with Factory[FT] {
    override def toScala: String = "*"
  }

  trait ModOp[FT <: FinalTypes] extends arithmetic.ModOp[FT] with Operator[FT] with Factory[FT] {
    override def toScala: String = "*"
  }

  trait LtOp[FT <: FinalTypes] extends arithmetic.LtOp[FT] with Operator[FT] with Factory[FT] {
    override def toScala: String = "*"
  }

  trait LeOp[FT <: FinalTypes] extends arithmetic.LeOp[FT] with Operator[FT] with Factory[FT] {
    override def toScala: String = "*"
  }

  trait AndOp[FT <: FinalTypes] extends boolean.AndOp[FT] with Operator[FT] with Factory[FT] {
    override def toScala: String = "&&"
  }

  trait OrOp[FT <: FinalTypes] extends boolean.OrOp[FT] with Operator[FT] with Factory[FT] {
    override def toScala: String = "||"
  }

  trait NotOp[FT <: FinalTypes] extends boolean.NotOp[FT] with Operator[FT] with Factory[FT] {
    override def toScala: String = "~"
  }

  trait True[FT <: FinalTypes] extends boolean.True[FT] with Expression[FT] with Factory[FT] {
    override def toScala: String = "true"
  }

  trait False[FT <: FinalTypes] extends boolean.False[FT] with Expression[FT] with Factory[FT] {
    override def toScala: String = "false"
  }

  trait Equals[FT <: FinalTypes] extends eqls.Equals[FT] with Expression[FT] with Factory[FT] {
    def toScala: String = s"${left.toScala} == ${right.toScala}"
  }

  trait Return[FT <: FinalTypes] extends any.Return[FT] with Statement[FT] with Factory[FT] {
    def toScala: String = s"return ${this.expression}"
  }

  trait LiftExpression[FT <: FinalTypes] extends imperative.LiftExpression[FT] with Statement[FT] with Factory[FT] {
    def toScala: String = s"${this.expression};"
  }

  trait Type[FT <: FinalTypes] extends any.Type[FT] with Factory[FT] {
    def toScala: String
    def toImport: Option[any.Import[FT]]
  }

  trait ClassReferenceType[FT <: FinalTypes] extends oo.ClassReferenceType[FT] with Type[FT] with Factory[FT] {
    def toScala: String = qualifiedClassName.map(_.toScala).mkString(".")

    def toImport: Option[any.Import[FT]] = Some(importStatement(qualifiedClassName))
  }

  trait Name[FT <: FinalTypes] extends any.Name[FT] with Factory[FT] {
    def component: String
    def mangled: String
    def toScala: String = component
  }

  trait Util[FT <: FinalTypes] extends Factory[FT] {
    def nameProvider: NameProvider[any.Name[FT]]
    def reify[T](tpe: OfHostType[T], value: T): any.Expression[FT] = reifiedScalaValue(tpe, value)

    def resolveImport(tpe: any.Type[FT]): Option[any.Import[FT]] = tpe.toImport
    def getFreshName(basedOn: any.Name[FT]): any.Name[FT] = {
      val id = UUID.randomUUID().toString.replace("-", "")
      nameProvider.mangle(s"${basedOn.component}_${id}")
    }
  }

  trait Method[FT <: FinalTypes] extends oo.Method[FT] with Factory[FT] with Util[FT] {

    def findClass(qualifiedName: any.Name[FT]*): any.Type[FT] =
      classReferenceType(qualifiedName:_*)

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

  trait Constructor[FT <: FinalTypes] extends oo.Constructor[FT] with scala.Method[FT] with Factory[FT] {


    override def name: any.Name[FT] = nameProvider.mangle(constructedType.map(_.toScala).getOrElse(""))
    override def returnType: Option[any.Type[FT]] = constructedType

    override def toScala: String = {
      val importDecls = imports.map(_.toScala).mkString("\n    ")
      val params = parameters.map(p => s"${p._1.toScala} : ${p._2.toScala}").mkString(",")
      val superInitDecls = if (superInitialization.isDefined) {
        s"super.${superInitialization.get._1.toScala}(${superInitialization.get._2.map(_.toScala).mkString(", ")})"
      } else ""
      val fieldInitDecls = fieldInitializers.map(fi =>{
         s"this.${fi._1.toScala} = ${fi._2.toScala}"
      }).mkString("  \n")
      val bodyDecls =
        s"""
        |  ${statements.map(_.toScala).mkString("\n    ")}
        """.stripMargin

      s"""${constructedType.get.toScala}(${params}) = {
         |  ${importDecls}
         |  ${superInitDecls}
         |  ${fieldInitDecls}
         |  ${bodyDecls}
         |}""".stripMargin
    }
  }

  trait Field[FT <: FinalTypes] extends oo.Field[FT] with Factory[FT] {
    def toScala: String = {
      val initExp = init.map(exp => s" = ${exp.toScala}").getOrElse("")
      s"var ${name.toScala}: ${tpe.toScala}${initExp}"
    }
  }

  trait Class[FT <: FinalTypes] extends oo.Class[FT] with Factory[FT] with Util[FT] {


    def findClass(qualifiedName: any.Name[FT]*): any.Type[FT] = {
      classReferenceType(qualifiedName: _*)
    }

    def classBodyDefinitionToScala: String = {
      val importDecls = imports.map(_.toScala).mkString("\n    ")
      val fieldDecls = fields.map(_.toScala).mkString("\n  ")
      val methodDecls = methods.map(_.toScala).mkString("\n  ")
      val constructorDecls = constructors.map(_.toScala).mkString("\n  ")
      s"""
         |{
         |  ${importDecls}
         |  ${fieldDecls}
         |  ${constructorDecls}
         |  ${methodDecls}
         |}""".stripMargin
    }

    def toScala: String = {
      val kind = if (isInterface) "trait" else if (isStatic) "object" else "class"
      val abstractMod = if (isAbstract && !isInterface) "abstract" else ""
      val extendsClause = {
        val supers = (parents ++ implemented)
        if (supers.nonEmpty) supers.mkString("extends", "with", "") else ""
      }

      s"""
         |${abstractMod} ${kind} ${name} ${extendsClause} ${classBodyDefinitionToScala}""".stripMargin
    }
  }

  trait ApplyExpression[FT <: FinalTypes] extends Expression[FT] with any.ApplyExpression[FT] with Factory[FT] {
    def toScala : String = s"${function.toScala}(${arguments.map(_.toScala).mkString(",")}"
  }

  trait VariableReferenceExpression[FT <: FinalTypes] extends Expression[FT] with imperative.VariableReferenceExpression[FT] with Factory[FT] {
    def toScala: String = s"${name.toScala}"
  }

  trait CompilationUnit[FT <: FinalTypes] extends oo.CompilationUnit[FT] with Factory[FT] with Util[FT] {
    def toScala: String = {
      val importDecls = imports.map(_.toScala).mkString("\n    ")
      val clsDecls = classes.map(_.toScala).mkString("\n\n")
      s"""
         |${importDecls}
         |${clsDecls}
         |""".stripMargin
    }
  }

  trait ToStringOp[FT <: FinalTypes] extends strings.ToStringOp[FT] with Operator[FT] {
    def toScala: String = ".toString()"
  }

  trait AppendStringOp[FT <: FinalTypes] extends strings.AppendStringOp[FT] with Operator[FT] {
    def toScala: String = "++"
  }
  trait StringLengthOp[FT <: FinalTypes] extends strings.StringLengthOp[FT] with Operator[FT] {
    def toScala: String = ".length"
  }
  trait AssertTrueOp[FT <: FinalTypes] extends assertions.AssertTrueOp[FT] with Operator[FT] {
    def toScala: String = "assert"
  }

  trait Factory[FT <: FinalTypes]
    extends any.Factory[FT]
    with oo.Factory[FT]
    with imperative.Factory[FT]
    with arithmetic.Factory[FT]
    with assertions.Factory[FT]
    with boolean.Factory[FT]
    with eqls.Factory[FT]
    with operatorExpression.Factory[FT]
    with strings.Factory[FT] {

    def name(name: String, mangled: String): Name[FT]
    def importStatement(components: Seq[any.Name[FT]]): Import[FT]

    def reifiedScalaValue[T](ofHostType: OfHostType[T], value: T): ReifiedScalaValue[FT, T]

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

    implicit def convert[T](reifiedScalaValue: ReifiedScalaValue[FT, T]): ReifiedScalaValue[FT, T]
    implicit def convert(other: any.ApplyExpression[FT]): ApplyExpression[FT]
    implicit def convert(other: oo.ClassReferenceType[FT]): ClassReferenceType[FT]
    implicit def convert(varRef: imperative.VariableReferenceExpression[FT]): VariableReferenceExpression[FT]
  }

  object Finalized {
    class FinalTypes extends scala.FinalTypes {
      override type DeclareVariable = Finalized.DeclareVariable
      override type AssignVariable = Finalized.AssignVariable
      override type IfThenElse = Finalized.IfThenElse
      override type While = Finalized.While
      override type Class = Finalized.Class
      override type Constructor = Finalized.Constructor
      override type Field = Finalized.Field
      override type MemberAccessExpression = Finalized.MemberAccessExpression
      override type SelfReferenceExpression = Finalized.SelfReferenceExpression
      override type ObjectInstantiationExpression = Finalized.ObjectInstantiationExpression
      override type CastExpression = Finalized.CastExpression
      override type InstanceOfExpression = Finalized.InstanceOfExpression
      override type SuperReferenceExpression = Finalized.SuperReferenceExpression
      override type Operator = Finalized.Operator
      override type BinaryExpression = Finalized.BinaryExpression
      override type UnaryExpression = Finalized.UnaryExpression
      override type Import = Finalized.Import
      override type Statement = Finalized.Statement
      override type Type = Finalized.Type
      override type Name = Finalized.Name
      override type Expression = Finalized.Expression
      override type ApplyExpression = Finalized.ApplyExpression
      override type ArgumentExpression = Finalized.ArgumentExpression
      override type CompilationUnit = Finalized.CompilationUnit
      override type Project = Finalized.Project
      override type Method = scala.Method[FinalTypes]
      override type ReifiedScalaValue[T] = Finalized.ReifiedScalaValue[T]
      override type ClassReferenceType = Finalized.ClassReferenceType
      override type VariableReferenceExpression = Finalized.VariableReferenceExpression
    }

    trait Factory extends scala.Factory[FinalTypes] {
      val finalTypes: FinalTypes = new FinalTypes

      def name(name: String, mangled: String): Name = Name(name, mangled)
      override def importStatement(components: Seq[any.Name[FinalTypes]]): Import = Import(components)
      override def binaryExpression(operator: operatorExpression.Operator[FinalTypes], left: any.Expression[FinalTypes], right: any.Expression[FinalTypes]): operatorExpression.BinaryExpression[FinalTypes] = BinaryExpression(operator, left, right)
      override def unaryExpression(operator: operatorExpression.Operator[FinalTypes], operand: any.Expression[FinalTypes]): operatorExpression.UnaryExpression[FinalTypes] = unaryExpression(operator, operand)
      implicit def convert(operator: operatorExpression.Operator[FinalTypes]): Operator = operator.getSelfOperator
      implicit def convert(binaryExpression: operatorExpression.BinaryExpression[FinalTypes]): BinaryExpression = binaryExpression.getSelfBinaryExpression
      implicit def convert(unaryExpression: operatorExpression.UnaryExpression[FinalTypes]): UnaryExpression = unaryExpression.getSelfUnaryExpression
      override def compilationUnit(name: Seq[any.Name[FinalTypes]], imports: Seq[any.Import[FinalTypes]], classes: Seq[oo.Class[FinalTypes]]): oo.CompilationUnit[FinalTypes] = CompilationUnit(name, imports, classes)

      override def cls(
        name: any.Name[FinalTypes], imports: Seq[any.Import[FinalTypes]], parents: Seq[any.Type[FinalTypes]], implemented: Seq[any.Type[FinalTypes]], fields: Seq[oo.Field[FinalTypes]], methods: Seq[any.Method[FinalTypes]], constructors: Seq[oo.Constructor[FinalTypes]], typeLookupMap: Map[TypeRep, any.Type[FinalTypes]], isAbstract: Boolean, isInterface: Boolean, isStatic: Boolean
      ): Class = Class(
        name = name,
        imports = imports,
        parents = parents,
        implemented = implemented,
        fields = fields,
        methods = methods,
        constructors = constructors,
        typeLookupMap = typeLookupMap,
        isAbstract = isAbstract,
        isInterface = isInterface,
        isStatic = isStatic)
      override def constructor(
        constructedType: Option[any.Type[FinalTypes]],
        imports: Set[any.Import[FinalTypes]],
        statements: Seq[any.Statement[FinalTypes]],
        parameters: Seq[(any.Name[FinalTypes], any.Type[FinalTypes])],
        typeLookupMap: Map[TypeRep, any.Type[FinalTypes]],
        superInitialization: Option[(any.Type[FinalTypes], Seq[any.Expression[FinalTypes]])],
        fieldInitializers: Seq[(any.Name[FinalTypes], any.Expression[FinalTypes])]): Constructor =
        Constructor(
          constructedType = constructedType,
          imports = imports,
          statements = statements,
          parameters = parameters,
          typeLookupMap = typeLookupMap,
          superInitialization = superInitialization,
          fieldInitializers = fieldInitializers
        )

      override def field(name: any.Name[FinalTypes], tpe: any.Type[FinalTypes], init: Option[any.Expression[FinalTypes]]): oo.Field[FinalTypes] = Field(name, tpe, init)
      override def memberAccessExpression(owner: any.Expression[FinalTypes], field: any.Name[FinalTypes]): MemberAccessExpression = MemberAccessExpression(owner, field)

      override def objectInstantiationExpression(tpe: any.Type[FinalTypes], constructorArguments: Seq[any.Expression[FinalTypes]], body: Option[oo.Class[FinalTypes]] = Option.empty): oo.ObjectInstantiationExpression[FinalTypes] =
        ObjectInstantiationExpression(tpe, constructorArguments, body)
      override def castExpression(tpe: any.Type[FinalTypes], expression: any.Expression[FinalTypes]): oo.CastExpression[FinalTypes] = CastExpression(tpe, expression)
      override def instanceOfExpression(tpe: any.Type[FinalTypes], expression: any.Expression[FinalTypes]): oo.InstanceOfExpression[FinalTypes] = InstanceOfExpression(tpe, expression)
      override def superReferenceExpression(parentType: any.Type[FinalTypes]): oo.SuperReferenceExpression[FinalTypes] = SuperReferenceExpression(parentType)
      override def selfReferenceExpression: oo.SelfReferenceExpression[FinalTypes] = SelfReferenceExpression()
      implicit def convert(other: any.Project[FinalTypes]): Project = other.getSelfProject
      implicit def convert(other: any.CompilationUnit[FinalTypes]): CompilationUnit = other.getSelfCompilationUnit
      implicit def convert(other: any.Method[FinalTypes]): Method = other.getSelfMethod
      implicit def convert(other: oo.Class[FinalTypes]): Class = other.getSelfClass
      implicit def convert(other: oo.Constructor[FinalTypes]): Constructor = other.getSelfConstructor
      implicit def convert(other: oo.Field[FinalTypes]): Field = other.getSelfField
      implicit def convert(other: oo.MemberAccessExpression[FinalTypes]): MemberAccessExpression = other.getSelfMemberAccessExpression
      implicit def convert(other: oo.SelfReferenceExpression[FinalTypes]): SelfReferenceExpression = other.getSelfSelfReferenceExpression
      implicit def convert(other: oo.ObjectInstantiationExpression[FinalTypes]): ObjectInstantiationExpression = other.getSelfObjectInstantiationExpression
      implicit def convert(other: oo.CastExpression[FinalTypes]): CastExpression = other.getSelfCastExpression
      implicit def convert(other: oo.InstanceOfExpression[FinalTypes]): InstanceOfExpression = other.getSelfInstanceOfExpression
      implicit def convert(other: oo.SuperReferenceExpression[FinalTypes]): SuperReferenceExpression = other.getSelfSuperReferenceExpression
      override def addOp(): arithmetic.AddOp[FinalTypes] = AddOp()
      override def subOp(): arithmetic.SubOp[FinalTypes] = SubOp()
      override def multOp(): arithmetic.MultOp[FinalTypes] = MultOp()
      override def divOp(): arithmetic.DivOp[FinalTypes] = DivOp()
      override def modOp(): arithmetic.ModOp[FinalTypes] = ModOp()
      override def ltOp(): arithmetic.LtOp[FinalTypes] = LtOp()
      override def leOp(): arithmetic.LeOp[FinalTypes] = LeOp()
      override def equals(tpe: any.Type[FinalTypes], left: any.Expression[FinalTypes], right: any.Expression[FinalTypes]): eqls.Equals[FinalTypes] = Equals(tpe, left, right)
      override def andOp(): boolean.AndOp[FinalTypes] = AndOp()
      override def orOp(): boolean.OrOp[FinalTypes] = OrOp()
      override def notOp(): boolean.NotOp[FinalTypes] = NotOp()
      override def trueExp(): boolean.True[FinalTypes] = True()
      override def falseExp(): boolean.False[FinalTypes] = False()
      override def declareVariable(name: any.Name[FinalTypes], tpe: any.Type[FinalTypes], initializer: Option[any.Expression[FinalTypes]]): imperative.DeclareVariable[FinalTypes] =
        DeclareVariable(name, tpe, initializer)
      override def assignVariable(variable: any.Expression[FinalTypes], expression: any.Expression[FinalTypes]): imperative.AssignVariable[FinalTypes] =
        AssignVariable(variable, expression)
      override def liftExpression(expression: any.Expression[FinalTypes]): imperative.LiftExpression[FinalTypes] =
        LiftExpression(expression)
      override def returnExpression(expression: any.Expression[FinalTypes]): any.Return[FinalTypes] =
        Return(expression)
      override def ifThenElse(condition: any.Expression[FinalTypes], ifBranch: Seq[any.Statement[FinalTypes]], elseIfBranches: Seq[(any.Expression[FinalTypes], Seq[any.Statement[FinalTypes]])], elseBranch: Seq[any.Statement[FinalTypes]]): imperative.IfThenElse[FinalTypes] =
        IfThenElse(condition, ifBranch, elseIfBranches, elseBranch)
      override def whileLoop(condition: any.Expression[FinalTypes], body: Seq[any.Statement[FinalTypes]]): imperative.While[FinalTypes] =
        While(condition, body)
      implicit def convert(decl: imperative.DeclareVariable[FinalTypes]): DeclareVariable = decl.getSelfDeclareVariable
      implicit def convert(assignVariable: imperative.AssignVariable[FinalTypes]): AssignVariable = assignVariable.getSelfAssignVariable
      implicit def convert(ifThenElse: imperative.IfThenElse[FinalTypes]): IfThenElse = ifThenElse.getSelfIfThenElse
      implicit def convert(whileLoop: imperative.While[FinalTypes]): While = whileLoop.getSelfWhile
      override def toStringOp(): strings.ToStringOp[FinalTypes] = ToStringOp()
      override def appendStringOp(): strings.AppendStringOp[FinalTypes] = AppendStringOp()
      override def stringLengthOp(): strings.StringLengthOp[FinalTypes] = StringLengthOp()
      override def assertTrueOp(): assertions.AssertTrueOp[FinalTypes] = AssertTrueOp()

      override def project(compilationUnits: Set[any.CompilationUnit[FinalTypes]]): any.Project[FinalTypes] = Project(compilationUnits)
      override def argumentExpression(parameterName: any.Name[FinalTypes]): any.ArgumentExpression[FinalTypes] = ArgumentExpression(parameterName)
      implicit def convert(other: any.Import[FinalTypes]): Import = other.getSelfImport
      implicit def convert(other: any.Statement[FinalTypes]): Statement = other.getSelfStatement
      implicit def convert(other: any.Type[FinalTypes]): Type = other.getSelfType
      implicit def convert(other: any.Name[FinalTypes]): Name = other.getSelfName
      implicit def convert(other: any.Expression[FinalTypes]): Expression = other.getSelfExpression
      implicit def convert(other: any.ArgumentExpression[FinalTypes]): ArgumentExpression = other.getSelfArgumentExpression

      override def reifiedScalaValue[T](ofHostType: OfHostType[T], value: T): ReifiedScalaValue[T] = ReifiedScalaValue(ofHostType, value)
      implicit def convert[T](other: scala.ReifiedScalaValue[FinalTypes, T]): ReifiedScalaValue[T] = other.getSelfAsReifiedScalaValue
      implicit def convert(other: any.ApplyExpression[FinalTypes]): ApplyExpression = other.getSelfApplyExpression
      implicit def convert(other: oo.ClassReferenceType[FinalTypes]): ClassReferenceType = other.getSelfClassReferenceType
      implicit def convert(varRef: imperative.VariableReferenceExpression[FinalTypes]): VariableReferenceExpression = varRef.getSelfVariableReferenceExpression
      override def variableReferenceExpression(name: any.Name[FinalTypes]): imperative.VariableReferenceExpression[FinalTypes] = VariableReferenceExpression(name)
      override def clsMethod(
        name: any.Name[FinalTypes],
        imports: Set[any.Import[FinalTypes]],
        statements: Seq[any.Statement[FinalTypes]],
        returnType: Option[any.Type[FinalTypes]],
        parameters: Seq[(any.Name[FinalTypes], any.Type[FinalTypes])],
        typeLookupMap: Map[TypeRep, any.Type[FinalTypes]],
        isAbstract: Boolean,
        isStatic: Boolean,
        isPublic: Boolean,
        isOverride: Boolean
      ): oo.Method[FinalTypes] =
        Method(
          name = name,
          imports = imports,
          statements = statements,
          returnType = returnType,
          parameters = parameters,
          typeLookupMap = typeLookupMap,
          isAbstract = isAbstract,
          isStatic = isStatic,
          isPublic = isPublic,
          isOverride = isOverride
        )
      override def classReferenceType(qualifiedClassName: any.Name[FinalTypes]*): oo.ClassReferenceType[FinalTypes] = ClassReferenceType(qualifiedClassName)
      override def applyExpression(function: any.Expression[FinalTypes], arguments: Seq[any.Expression[FinalTypes]]): any.ApplyExpression[FinalTypes] = ApplyExpression(function, arguments)
    }

    case class Name(override val component: String, override val mangled: String) extends scala.Name[FinalTypes] with Factory {
      override def getSelfName: this.type = this
    }

    trait Util extends scala.Util[FinalTypes] with Factory {
      override def nameProvider: NameProvider[any.Name[FinalTypes]] = new ScalaNameProvider[FinalTypes](this)
    }

    case class Method(
      override val name: any.Name[FinalTypes],
      override val imports: Set[any.Import[FinalTypes]],
      override val statements: Seq[any.Statement[FinalTypes]],
      override val returnType: Option[any.Type[FinalTypes]],
      override val parameters: Seq[(any.Name[FinalTypes], any.Type[FinalTypes])],
      override val typeLookupMap: Map[TypeRep, any.Type[FinalTypes]],
      override val isAbstract: Boolean,
      override val isStatic: Boolean,
      override val isPublic: Boolean,
      override val isOverride: Boolean
    ) extends scala.Method[FinalTypes] with Util {
      def getSelfMethod: this.type = this
    }

    case class Import(override val components: Seq[any.Name[FinalTypes]]) extends scala.Import[FinalTypes] with Factory {
      def getSelfImport: this.type = this
    }

    trait Type extends scala.Type[FinalTypes] with Factory {
      def getSelfType: this.type = this
    }

    case class ClassReferenceType(override val qualifiedClassName: Seq[any.Name[FinalTypes]]) extends scala.ClassReferenceType[FinalTypes] with Type with Factory {
      def getSelfClassReferenceType: this.type = this
    }

    trait Operator extends scala.Operator[FinalTypes] with Factory {
      override def getSelfOperator: this.type = this
    }

    trait Expression extends scala.Expression[FinalTypes] with Factory {
      override def getSelfExpression: this.type = this
    }

    case class BinaryExpression(
      override val operator: operatorExpression.Operator[FinalTypes],
      override val left: any.Expression[FinalTypes],
      override val right: any.Expression[FinalTypes]
    )
      extends Expression with scala.BinaryExpression[FinalTypes] with Factory {
      override def getSelfBinaryExpression: this.type = this

    }

    case class UnaryExpression(
      override val operator: operatorExpression.Operator[FinalTypes],
      override val operand: any.Expression[FinalTypes]
    )
      extends Expression with scala.UnaryExpression[FinalTypes] with Factory {
      override def getSelfUnaryExpression: this.type = this
    }

    case class ReifiedScalaValue[T](
      override val ofHostType: OfHostType[T],
      override val value: T
    ) extends Expression with scala.ReifiedScalaValue[FinalTypes, T] with Factory {
      override def getSelfAsReifiedScalaValue: this.type = this
    }

    trait Statement extends scala.Statement[FinalTypes] with Factory {
      def getSelfStatement: this.type = this
    }

    case class DeclareVariable(
      override val name: any.Name[FinalTypes],
      override val tpe: any.Type[FinalTypes],
      override val initializer: Option[any.Expression[FinalTypes]]
    ) extends Statement with scala.DeclareVariable[FinalTypes] with Factory {
      override def getSelfDeclareVariable: this.type = this
    }

    case class AssignVariable(
      override val variable: any.Expression[FinalTypes],
      override val assignmentExpression: any.Expression[FinalTypes]
    ) extends Statement with scala.AssignVariable[FinalTypes] with Factory {
      override def getSelfAssignVariable: this.type = this
    }

    case class IfThenElse(
      override val condition: any.Expression[FinalTypes],
      override val ifBranch: Seq[any.Statement[FinalTypes]],
      override val elseIfBranches: Seq[(any.Expression[FinalTypes], Seq[any.Statement[FinalTypes]])],
      override val elseBranch: Seq[any.Statement[FinalTypes]]
    ) extends Statement with scala.IfThenElse[FinalTypes] with Factory {
      override def getSelfIfThenElse: this.type = this
    }

    case class While(
      override val condition: any.Expression[FinalTypes],
      override val body: Seq[any.Statement[FinalTypes]]
    ) extends Statement with scala.While[FinalTypes] with Factory {
      override def getSelfWhile: this.type = this
    }


    case class Class(
      override val name: any.Name[FinalTypes],
      override val imports: Seq[any.Import[FinalTypes]] = Seq.empty,
      override val parents: Seq[any.Type[FinalTypes]] = Seq.empty,
      override val implemented: Seq[any.Type[FinalTypes]] = Seq.empty,
      override val fields: Seq[oo.Field[FinalTypes]] = Seq.empty,
      override val methods: Seq[any.Method[FinalTypes]] = Seq.empty,
      override val constructors: Seq[oo.Constructor[FinalTypes]] = Seq.empty,
      override val typeLookupMap: Map[TypeRep, any.Type[FinalTypes]] = Map.empty,
      override val isAbstract: Boolean = false,
      override val isInterface: Boolean = false,
      override val isStatic: Boolean = false
    ) extends scala.Class[FinalTypes] with Util {
      override def getSelfClass: this.type = this
    }

    case class Constructor(
      override val constructedType: Option[any.Type[FinalTypes]],
      override val imports: Set[any.Import[FinalTypes]],
      override val statements: Seq[any.Statement[FinalTypes]],
      override val parameters: Seq[(any.Name[FinalTypes], any.Type[FinalTypes])],
      override val typeLookupMap: Map[TypeRep, any.Type[FinalTypes]],
      override val superInitialization: Option[(any.Type[FinalTypes], Seq[any.Expression[FinalTypes]])],
      override val fieldInitializers: Seq[(any.Name[FinalTypes], any.Expression[FinalTypes])]) extends scala.Constructor[FinalTypes] with Util {
      override def getSelfMethod: this.type = this
      override def getSelfConstructor: this.type = this
      override def toScala: String = {
        super[Constructor].toScala
      }
    }

    case class Field(
      override val name: any.Name[FinalTypes],
      override val tpe: any.Type[FinalTypes],
      override val init: Option[any.Expression[FinalTypes]] = Option.empty
    ) extends scala.Field[FinalTypes] with Factory {
      override def getSelfField: this.type = this
    }

    case class MemberAccessExpression(
      override val owner: any.Expression[FinalTypes],
      override val field: any.Name[FinalTypes]
    ) extends Expression with scala.MemberAccessExpression[FinalTypes] with Factory {
      override def getSelfMemberAccessExpression: this.type = this
    }

    case class SelfReferenceExpression() extends Expression with scala.SelfReferenceExpression[FinalTypes] with Factory {
      override def getSelfSelfReferenceExpression: this.type = this
    }

    case class ObjectInstantiationExpression(
      override val tpe: any.Type[FinalTypes],
      override val constructorArguments: Seq[any.Expression[FinalTypes]],
      override val body: Option[oo.Class[FinalTypes]] = Option.empty
    ) extends Expression with scala.ObjectInstantiationExpression[FinalTypes] with Factory {
      override def getSelfObjectInstantiationExpression: this.type = this
    }

    case class CastExpression(
      override val tpe: any.Type[FinalTypes],
      override val expression: any.Expression[FinalTypes]
    ) extends Expression with scala.CastExpression[FinalTypes] with Factory {
      override def getSelfCastExpression: this.type = this
    }

    case class InstanceOfExpression(
      override val tpe: any.Type[FinalTypes],
      override val expression: any.Expression[FinalTypes]
    ) extends Expression with scala.InstanceOfExpression[FinalTypes] with Factory {
      override def getSelfInstanceOfExpression: this.type = this
    }

    case class SuperReferenceExpression(
      override val parentType: any.Type[FinalTypes]
    ) extends Expression with scala.SuperReferenceExpression[FinalTypes] with Factory {
      override def getSelfSuperReferenceExpression: this.type = this
    }

    case class ArgumentExpression(
      override val parameterName: any.Name[FinalTypes]
    ) extends Expression with scala.ArgumentExpression[FinalTypes] with Factory {
      override def getSelfArgumentExpression: this.type = this
    }

    case class CompilationUnit(
      override val name: Seq[any.Name[FinalTypes]] = Seq.empty,
      override val imports: Seq[any.Import[FinalTypes]] = Seq.empty,
      override val classes: Seq[oo.Class[FinalTypes]] = Seq.empty
    ) extends scala.CompilationUnit[FinalTypes] with Util {
      override def getSelfCompilationUnit: this.type = this
    }

    case class Project(override val compilationUnits: Set[any.CompilationUnit[FinalTypes]]) extends scala.Project[FinalTypes] with Factory {
      override def getSelfProject: this.type = this
    }


    case class AddOp() extends scala.AddOp[FinalTypes] with Operator with Factory

    case class SubOp() extends scala.SubOp[FinalTypes] with Operator with Factory

    case class MultOp() extends scala.MultOp[FinalTypes] with Operator with Factory

    case class DivOp() extends scala.DivOp[FinalTypes] with Operator with Factory

    case class ModOp() extends scala.ModOp[FinalTypes] with Operator with Factory

    case class LtOp() extends scala.LtOp[FinalTypes] with Operator with Factory

    case class LeOp() extends scala.LeOp[FinalTypes] with Operator with Factory

    case class Equals(
      override val tpe: any.Type[FinalTypes],
      override val left: any.Expression[FinalTypes],
      override val right: any.Expression[FinalTypes]
    ) extends scala.Equals[FinalTypes] with Expression with Factory

    case class AndOp() extends scala.AndOp[FinalTypes] with Operator with Factory
    case class OrOp() extends scala.OrOp[FinalTypes] with Operator with Factory
    case class NotOp() extends scala.NotOp[FinalTypes] with Operator with Factory
    case class True() extends scala.True[FinalTypes] with Expression with Factory
    case class False() extends scala.False[FinalTypes] with Expression with Factory

    case class Return(expression: any.Expression[FinalTypes]) extends scala.Return[FinalTypes] with Statement with Factory
    case class LiftExpression(expression: any.Expression[FinalTypes]) extends scala.LiftExpression[FinalTypes] with Statement with Factory

    case class ToStringOp() extends scala.ToStringOp[FinalTypes] with Operator

    case class AppendStringOp() extends scala.AppendStringOp[FinalTypes] with Operator

    case class StringLengthOp() extends scala.StringLengthOp[FinalTypes] with Operator

    case class AssertTrueOp() extends scala.AssertTrueOp[FinalTypes] with Operator

    case class ApplyExpression(
      override val function: any.Expression[FinalTypes],
      override val arguments: Seq[any.Expression[FinalTypes]]
    ) extends scala.ApplyExpression[FinalTypes] with Expression {
      override def getSelfApplyExpression: this.type = this
    }

    case class VariableReferenceExpression(override val name: any.Name[FinalTypes]) extends scala.VariableReferenceExpression[FinalTypes] with Expression with Factory {
      override def getSelfVariableReferenceExpression: this.type = this
    }
  }
}
