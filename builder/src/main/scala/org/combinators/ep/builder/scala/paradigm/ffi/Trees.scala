package org.combinators.ep.builder.scala.paradigm.ffi

import org.combinators.cogen.{Command, FileWithPath, Understands}
import org.combinators.cogen.paradigm.{Reify, ToTargetLanguageType}
import org.combinators.ep.builder.inbetween.paradigm.ffi.Trees as Trs
import org.combinators.ep.language.inbetween.ContextRegistry
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.inbetween.oo.OOParadigm
import org.combinators.ep.language.scala.ast.BaseAST

trait Trees extends Trs {
  override val _base: AnyParadigm { val ast: TreesAST & BaseAST }
  val oo:OOParadigm.WithBase[_base.type]
  val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method]
  val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor]
  val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class]

  override val treeLibrary: Seq[FileWithPath] = _base.ast.scalaTreesOps.treeLibrary

  val treesInMethods: TreesIn[_base.ast.any.Method] = {
    import _base.ast.any.*
    class TreesInMethods(
           override val registry: ContextRegistry[_base.type, Method] = methodRegistry,
           override val canToTargetLanguage: Understands[Method, ToTargetLanguageType[Type]] = _base.methodBodyCapabilities.canTransformTypeInMethodBody,
         ) extends TreesIn[Method] {
      override def canReifyInCtxt[T]: Understands[Method, Reify[T, Expression]] = _base.methodBodyCapabilities.canReifyInMethodBody
    }
    new TreesInMethods()
  }
  val treesInConstructors: TreesIn[_base.ast.oo.Constructor] = {
    import _base.ast.any.*
    import _base.ast.oo.Constructor
    class TreesInConstructors(
        override val registry: ContextRegistry[_base.type, Constructor] = constructorRegistry,
        override val canToTargetLanguage: Understands[Constructor, ToTargetLanguageType[Type]] = oo.constructorCapabilities.canTranslateTypeInConstructor,
      ) extends TreesIn[Constructor] {
      override def canReifyInCtxt[T]: Understands[Constructor, Reify[T, Expression]] = oo.constructorCapabilities.canReifyInConstructor[T]
    }
    new TreesInConstructors()
  }

  val treesInClasses: TreesIn[_base.ast.oo.Class] = {
    import _base.ast.any.*
    import _base.ast.oo.Class as Cls
    class TreesInClasses(
         override val registry: ContextRegistry[_base.type, Cls] = classRegistry,
         override val canToTargetLanguage: Understands[Cls, ToTargetLanguageType[Type]] = oo.classCapabilities.canTranslateTypeInClass,
       ) extends TreesIn[Cls] {
      override def canReifyInCtxt[T]: Understands[Cls, Reify[T, Expression]] = new Understands[Cls, Reify[T, Expression]] {
        def perform(context: Cls, command: Reify[T, Expression]): (Cls, Expression) = {
          Command.runGenerator(context.reifyLookupMap(command.tpe)(command.value), context)
        }
      }
    }
    new TreesInClasses()
  }
}


object Trees {
  type WithBase[B <: AnyParadigm, OO <: OOParadigm.WithBase[B]] =
    Trees {
      val _base: B
      val oo: OO
    }


  def apply[AST <: TreesAST & BaseAST, B <: AnyParadigm.WithAST[AST]](
                base: B,
                _oo: OOParadigm.WithBase[base.type],
                _methodRegistry: ContextRegistry[base.type, base.ast.any.Method],
                _constructorRegistry: ContextRegistry[base.type, base.ast.oo.Constructor],
                _classRegistry: ContextRegistry[base.type, base.ast.oo.Class],
              ): Trees.WithBase[base.type, _oo.type] = {
    class Trs(
                override val _base: base.type = base,
                override val oo: _oo.type = _oo,
                override val methodRegistry: ContextRegistry[base.type, _base.ast.any.Method] = _methodRegistry,
                override val constructorRegistry: ContextRegistry[base.type, _base.ast.oo.Constructor] = _constructorRegistry,
                override val classRegistry: ContextRegistry[base.type, _base.ast.oo.Class] = _classRegistry,
              ) extends Trees {}
    new Trs()
  }
}
