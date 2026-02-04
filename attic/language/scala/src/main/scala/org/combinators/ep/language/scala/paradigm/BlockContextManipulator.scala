package org.combinators.ep.language.scala.paradigm   /*DI:LD:AI*/

import org.combinators.ep.language.scala.{CtorCtxt, MethodBodyCtxt}

import scala.meta._

trait BlockContextManipulator[Ctxt] {
  def getBlock(ctxt: Ctxt): Term.Block
  def copyWithBlock(ctxt: Ctxt, blockStmt: Term.Block): Ctxt
  def nextBlockContext(ctxt: Ctxt): Ctxt = copyWithBlock(ctxt, Term.Block(List.empty))
}

object BlockContextManipulator {
  def inMethodContext[AP <: AnyParadigm](base: AP): BlockContextManipulator[MethodBodyCtxt] =
      new BlockContextManipulator[MethodBodyCtxt] {
        def getBlock(ctxt: MethodBodyCtxt): Term.Block =
          ctxt.method match {
            case definition: Defn.Def =>
              definition.body match {
                case blk: Term.Block => blk
                case term => Term.Block(List(term))
              }
            case decl: Decl.Def => Term.Block(List.empty)
          }
        def copyWithBlock(ctxt: MethodBodyCtxt, blockStmt: Term.Block): MethodBodyCtxt = {
          val newMethod =
            ctxt.method match {
              case definition: Defn.Def => definition.copy(body = blockStmt)
              case decl: Decl.Def =>
                Defn.Def(
                  mods = decl.mods,
                  name = decl.name,
                  tparams = decl.tparams,
                  paramss = decl.paramss,
                  decltpe = Some(decl.decltpe),
                  body = blockStmt
                )
            }
          ctxt.copy(method = newMethod)
        }
      }


  def inConstructorContext[AP <: AnyParadigm](base: AP): BlockContextManipulator[CtorCtxt] =
    new BlockContextManipulator[CtorCtxt] {
      def getBlock(ctxt: CtorCtxt): Term.Block =
        ctxt.ctor match {
          case _: Ctor.Primary => Term.Block(List.empty)
          case secondary: Ctor.Secondary => Term.Block(secondary.stats)
        }
      def copyWithBlock(ctxt: CtorCtxt, blockStmt: Term.Block): CtorCtxt = {
        val newCtor =
          ctxt.ctor match {
            case primary: Ctor.Primary =>
              if (blockStmt.stats.isEmpty) primary
              else Ctor.Secondary(
                primary.mods,
                Name.Anonymous(),
                primary.paramss,
                Init(
                  tpe = Type.Singleton(Term.This(Name.Anonymous())),
                  name = Name.Anonymous(),
                  argss = primary.paramss.map(_.map(param => Term.Name(param.name.value)))
                ),
                blockStmt.stats
              )
            case secondary: Ctor.Secondary =>
              secondary.copy(stats = blockStmt.stats)
          }
        ctxt.copy(ctor = newCtor)
      }
    }
}