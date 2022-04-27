package org.combinators.ep.language.haskell.ast

import scala.language.implicitConversions

case class Name(components: String*)
case class SimpleName(name: String)

case class CompilationUnit(
  name: Name,
  imports: Seq[Import],
  typeDecls: Seq[TypeDecl],
  typeClassDecls: Seq[TypeClassDecl],
  typeClassInstances: Seq[TypeClassInstance],
  funDecls: Seq[(Option[FunDecl], FunBody)]
)

case class Import(name: Name, qualified: Boolean)

case class TypeDecl(name: SimpleName, params: Seq[SimpleName], cases: Seq[TypeCase])

case class TypeCase(name: SimpleName, attributes: Seq[Type])

sealed trait Type
case class TyApp(name: Name, args: Seq[Type]) extends Type
case class Arrow(source: Type, target: Type) extends Type
case class ListTy(inner: Type) extends Type
case class TupleTy(inner: Type*) extends Type

case class TypeClassDecl(
  name: SimpleName,
  params: Seq[SimpleName],
  functions: Seq[(FunDecl, Option[FunBody])]
)

case class TypeClassConstraint(name: Name, args: Seq[Type])

case class TypeClassInstance(name: Name, args: Seq[Type], constraints: Seq[TypeClassConstraint], functions: Seq[FunBody])

case class FunDecl(name: SimpleName, constraints: Seq[TypeClassConstraint], tpe: Type)
case class FunBody(name: SimpleName, params: Seq[SimpleName], bodyExp: Expression)

sealed trait Expression
case class Ref(name: Name) extends Expression
case class App(function: Expression, argument: Expression) extends Expression
case class Lambda(variable: SimpleName, body: Expression) extends Expression
case class Let(variable: SimpleName, value: Expression, body: Expression) extends Expression
case class Case(exp: Expression, ofs: Seq[MatchExpression]) extends Expression
case class IfThenElse(exp: Expression, thenPart: Expression, elsePart: Expression) extends Expression
case class IntValue(num: Int) extends Expression
case class DoubleValue(num: Double) extends Expression
case class HSString(text: String) extends Expression
case class HSList(entries: Expression*) extends Expression
case class HSTuple(entries: Expression*) extends Expression
case class WildCard() extends Expression

case class MatchExpression(pattern: Expression, body: Expression)

object AST {
  trait Doc { self =>
    def <>(other: Doc): Doc
    def indent(offset: Int = 2): Doc
    def layout(): String
  }

  case object nil extends Doc {
    override def <>(other: Doc): Doc = other
    override def layout(): String = ""
    override def indent(offset: Int = 2): Doc = nil
  }

  case class Text(text: String, rest: Doc) extends Doc {
    override def <>(other: Doc): Doc = Text(text, other.<>(rest))
    override def layout(): String = text + rest.layout()
    override def indent(offset: Int = 2): Doc = Text(text, rest.indent(offset))
  }

  case class Line(indent: Int, content: Doc) extends Doc {
    override def <>(other: Doc): Doc = Line(indent, other.<>(content))
    override def layout(): String = "\n" + " ".repeat(indent) + content.layout()
    override def indent(offset: Int = 2): Doc = Line(indent+offset, content.indent(offset))
  }

  object newline extends Line(0, nil)
  implicit def str(text: String): Doc = Text(text, nil)

  trait PrettyPrintable[A] {
    def pretty: Doc = prettyPrec(0)
    def prettyPrec(prec: Int): Doc = pretty
  }


  sealed trait Assoc {
    val operators: Map[String, Int]
    def infixPrec(exp: Expression): Int =
      exp match {
        case Ref(Name(simpleName)) => operators(simpleName)
        case _ => 9
      }
  }
  case object Left extends Assoc {
    val operators: Map[String, Int] =
      Map(
        "!!" -> 9,
        "*" -> 7,
        "`mod`" -> 7,
        "`rem`" -> 7,
        "`quot`" -> 7,
        "+" -> 6,
        "-" -> 6,
        ">>" -> 1,
        ">>=" -> 1
      )
  }
  case object Right extends Assoc {
    val operators: Map[String, Int] =
      Map(
        "." -> 9,
        "^" -> 8,
        "^^" -> 8,
        "**" -> 8,
        ":" -> 5,
        "++" -> 5,
        "&&" -> 3,
        "||" -> 2,
        "$" -> 0,
        "$!" -> 0,
        "$!" -> 0,
        "`seq`" -> 0
    )
  }
  case object Non extends Assoc {
    val operators: Map[String, Int] =
      Map(
        "==" -> 4,
        "/=" -> 4,
        "<" -> 4,
        "<=" -> 4,
        ">" -> 4,
        ">=" -> 4,
        "`elem`" -> 4,
        "`notElem`" -> 4
      )
  }
  def assoc(e: Expression): Option[Assoc] =
    e match {
      case Ref(Name(s))  if Left.operators.keySet.contains(s) =>
        Some(Left)
      case Ref(Name(s)) if Right.operators.keySet.contains(s) =>
        Some(Right)
      case Ref(Name(s)) if Non.operators.keySet.contains(s) =>
        Some(Non)
      case _ => None
    }

  def parens(d: Doc): Doc = "(" <> d <> ")"

  implicit def prettyPrintableMatchExp(exp: MatchExpression): PrettyPrintable[MatchExpression] =
    new PrettyPrintable[MatchExpression] {
      override def pretty: Doc = exp.pattern.pretty <> " -> " <> newline <> exp.body.pretty.indent()
    }

  implicit def prettyPrintableExpression(exp: Expression): PrettyPrintable[Expression] =
    new PrettyPrintable[Expression] {
      override def prettyPrec(prec: Int): Doc =
        exp match {
          case Ref(Name(names@_*)) => str(names.mkString("."))
          case App(App(op, x), y) =>
            assoc(op) match {
              case Some(Left) =>
                val iPrec = Left.infixPrec(op)
                val inner = x.prettyPrec(iPrec) <> " " <> op.prettyPrec(iPrec) <> " " <> y.prettyPrec(iPrec + 1)
                if (prec > iPrec) parens(inner) else inner
              case Some(Right) =>
                val iPrec = Right.infixPrec(op)
                val inner = x.prettyPrec(iPrec + 1) <> " " <> op.prettyPrec(iPrec) <> " " <> y.prettyPrec(iPrec)
                if (prec > iPrec) parens(inner) else inner
              case Some(Non) =>
                val iPrec = Non.infixPrec(op)
                val inner = x.prettyPrec(iPrec + 1) <> " " <> op.prettyPrec(iPrec) <> " " <> y.prettyPrec(iPrec + 1)
                if (prec > iPrec) parens(inner) else inner
              case None =>
                val iPrec = 10
                val inner = x.prettyPrec(iPrec + 1) <> " " <> op.prettyPrec(iPrec) <> " " <> y.prettyPrec(iPrec + 1)
                if (prec > iPrec) parens(inner) else inner
            }
          case App(f, x) =>
            val iPrec = 10
            val inner = f.prettyPrec(iPrec) <> " " <> x.prettyPrec(iPrec + 1)
            if (prec > iPrec) parens(inner) else inner
          case Lambda(v, b) =>
            parens("\\ " <> v.name <> " -> " <> b.prettyPrec(0))
          case Let(variable, value, body) => 
            "let " <> variable.name <> " = " <> value.pretty.indent() <> " in " <> newline <>
              body.pretty
          case Case(exp, ofs) =>
            val iPrec = 0
            val exps = ofs.foldLeft[Doc](newline)((d, mexp) => d <> newline <> mexp.pretty)
            val inner = "case " <> exp.prettyPrec(iPrec) <> " of" <> exps.indent()
            if (prec > iPrec) parens(inner) else inner
          case IfThenElse(exp, thenPart, elsePart) =>
            val iPrec = 0
            val inner =
              "if " <> exp.prettyPrec(iPrec) <> newline <>
                "then " <> thenPart.prettyPrec(iPrec).indent() <>
                "else " <> elsePart.prettyPrec(iPrec).indent()
            if (prec > iPrec) parens(inner) else inner
          case IntValue(i) =>
            str(String.valueOf(i))
          case DoubleValue(d) =>
            str(String.valueOf(d))
          case HSString(text) =>
            str(s""""$text"""")
          case HSList(entries@_*) =>
            entries.foldLeft[Doc]("[ ")((d, e) => d <> ", " <> e.pretty) <> "]"
          case HSTuple(entries@_*) =>
            entries.foldLeft[Doc]("( ")((d, e) => d <> ", " <> e.pretty) <> ")"
          case WildCard() =>
            str("_")
        }
    }

  implicit def prettyPrintableType(ty: Type): PrettyPrintable[Type] =
    new PrettyPrintable[Type] {
      override def prettyPrec(prec: Int): Doc =
        ty match {
          case ListTy(inner) => "[" <> inner.pretty <> "]"
          case TupleTy(inner@_*) => "(" <> inner.tail.foldLeft[Doc](inner.head.pretty)((d, t) => d <> ", " <> t.pretty) <> ")"
          case Arrow(source, target) =>
            val iPrec = 1
            val inner = source.prettyPrec(iPrec+1) <> " -> " <> target.prettyPrec(iPrec)
            if (prec > iPrec) parens(inner) else inner
          case TyApp(name, args) =>
            val iPrec = 2
            val inner = args.foldLeft[Doc](name.components.mkString("."))((d, arg) => d <> " " <> arg.prettyPrec(iPrec+1))
            if (prec > iPrec && (name.components.size > 1 || args.nonEmpty)) parens(inner) else inner

        }
    }

  implicit def prettyPrintableTypeCase(typeCase: TypeCase): PrettyPrintable[TypeCase] =
    new PrettyPrintable[TypeCase] {
      override def pretty: Doc = {
        typeCase.attributes.foldLeft[Doc](typeCase.name.name)((d, t) => d <> " " <> t.prettyPrec(3))
      }
    }

  implicit def prettyPrintableTypeDecl(tyDecl: TypeDecl): PrettyPrintable[TypeDecl] =
    new PrettyPrintable[TypeDecl] {
      override def pretty: Doc = {
        "data " <> tyDecl.name.name <> tyDecl.params.foldLeft[Doc](nil)((d, p) => d <> " " <> p.name) <>
          tyDecl.cases
            .zip("=" +: (1 until tyDecl.cases.size).map(_ => "|"))
            .foldLeft[Doc](nil)((d, c) => d <> newline <> str(c._2) <> " " <> c._1.pretty)
            .indent()
      }
    }

  implicit def prettyPrintableTypeClassDecl(typeClassDecl: TypeClassDecl): PrettyPrintable[TypeClassDecl] =
    new PrettyPrintable[TypeClassDecl] {
      override def pretty: Doc = {
        val head = typeClassDecl.params.foldLeft[Doc]("class " <> typeClassDecl.name.name)((d, p) => d <> " " <> p.name) <> " where"
        val body = typeClassDecl.functions.foldLeft[Doc](nil){
          case (d, (decl, None)) =>
            d <> newline <> decl.pretty
          case (d, (decl, Some(body))) =>
            d <> newline <> decl.pretty <> newline <> body.pretty
        }
        head <> body.indent()
      }
    }

  implicit def prettyPrintableTyClassConstraint(constraint: TypeClassConstraint): PrettyPrintable[TypeClassConstraint] =
    new PrettyPrintable[TypeClassConstraint] {
      override def pretty: Doc = {
        val name = str(constraint.name.components.mkString("."))
        parens(constraint.args.foldLeft[Doc](name)((d, t) => d <> " " <> t.prettyPrec(3)))
      }
    }

  implicit def prettyPrintableFunDecl(funDecl: FunDecl): PrettyPrintable[FunDecl] =
    new PrettyPrintable[FunDecl] {
      override def pretty: Doc =
        funDecl.name.name <> " :: " <>
          funDecl.constraints
            .zip((1 until funDecl.constraints.size).map(_ => ", ") :+ " => ")
            .foldLeft[Doc](nil)((d, c) => c._1.pretty <> c._2 <> d) <>
          funDecl.tpe.pretty
    }

  implicit def prettyPrintableFunBody(funBody: FunBody): PrettyPrintable[FunBody] =
    new PrettyPrintable[FunBody] {
      override def pretty: Doc =
        funBody.name.name <>
          funBody.params.foldLeft[Doc](nil)((d, p) => d <> " " <> p.name) <> " = " <>
          funBody.bodyExp.pretty.indent()
    }

  implicit def prettyPrintableImport(imp: Import): PrettyPrintable[Import] =
    new PrettyPrintable[Import] {
      override def pretty: Doc = {
        "import" <> (if (imp.qualified) { " qualified "} else {" "}) <> imp.name.components.mkString(".")
      }

    }

  implicit def prettyPrintableTypeClassInstance(typeClassInstance: TypeClassInstance): PrettyPrintable[TypeClassInstance] =
    new PrettyPrintable[TypeClassInstance] {
      override def pretty: Doc = {
        val constraints =
          typeClassInstance.constraints
            .zip((1 until typeClassInstance.constraints.size).map(_ => ", ") :+ " => ")
            .foldLeft[Doc](nil)((d, c) => c._1.pretty <> c._2 <> d)
        val name: Doc =
          if (typeClassInstance.name.components.size > 1) parens(typeClassInstance.name.components.mkString("."))
          else typeClassInstance.name.components.head
        val head = typeClassInstance.args.foldLeft[Doc]("instance " <> constraints <> name)((d, p) => d <> " " <> p.pretty) <> " where"
        val body = typeClassInstance.functions.foldLeft[Doc](nil)((d, b) => d <> newline <> b.pretty)
        head <> body.indent()
      }
    }

  implicit def prettyPrintableCompilationUnit(compilationUnit: CompilationUnit): PrettyPrintable[CompilationUnit] =
    new PrettyPrintable[CompilationUnit] {
      override def pretty: Doc = {
        val imps = compilationUnit.imports
          .sortBy(i => i.name.components.mkString("."))
          .map(i => i.pretty)          
          .foldLeft[Doc](nil)((d, i) => d <> newline <> i)
        val tyDecls = compilationUnit.typeDecls
          .foldLeft[Doc](nil)((d, td) => d <> newline <> newline <> td.pretty)
        val clsDecls = compilationUnit.typeClassDecls
          .foldLeft[Doc](nil)((d, cd) => d <> newline <> newline <> cd.pretty)
        val instDecls = compilationUnit.typeClassInstances
          .foldLeft[Doc](nil)((d, ci) => d <> newline <> newline <> ci.pretty)
        val funDecls = compilationUnit.funDecls
          .foldLeft[Doc](nil)((d, fd) => d <> newline <> newline <> fd._1.map(_.pretty <> newline).getOrElse(nil) <> fd._2.pretty)
        "module " <> compilationUnit.name.components.mkString(".") <> " where" <>
          imps <>
          tyDecls <>
          clsDecls <>
          instDecls <>
          funDecls
      }
    }

}
