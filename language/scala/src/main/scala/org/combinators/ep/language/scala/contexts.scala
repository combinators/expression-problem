package org.combinators.ep.language.scala   /*DI:LD:AI*/

case class ProjectCtxt(
  resolver: ContextSpecificResolver,
  units: Seq[(Seq[scala.meta.Name], scala.meta.Source)],
  testUnits: Seq[(Seq[scala.meta.Name], scala.meta.Source)],
  extraDependencies: Seq[String]
)
case class CompilationUnitCtxt(
  resolver: ContextSpecificResolver,
  filename: String,
  unit: scala.meta.Source,
  isTest: Boolean,
  companionDefinitions: Seq[scala.meta.Defn]
)
case class TypeCtxt(
  resolver: ContextSpecificResolver,
  tpe: scala.meta.Type.Name => scala.meta.Defn.Enum,
  extraImports: Seq[scala.meta.Import]
)

case class ClassCtxt(
  resolver: ContextSpecificResolver,
  cls: scala.meta.Defn,
  extraImports: Seq[scala.meta.Import]
)
case class TestCtxt(
  resolver: ContextSpecificResolver,
  extraImports: Seq[scala.meta.Import],
  testClass: scala.meta.Defn.Class
)
case class MethodBodyCtxt(
  resolver: ContextSpecificResolver,
  extraImports: Seq[scala.meta.Import],
  method: scala.meta.Stat
)
case class CtorCtxt(
  resolver: ContextSpecificResolver,
  extraImports: Seq[scala.meta.Import],
  ctor: scala.meta.Ctor
)
case class TypeParamCtxt(
  param: scala.meta.Type.Param
)
