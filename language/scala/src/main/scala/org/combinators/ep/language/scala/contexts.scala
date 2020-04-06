package org.combinators.ep.language.scala

case class ProjectCtxt(
  resolver: ContextSpecificResolver,
  units: Seq[scala.meta.Source],
  testUnits: Seq[scala.meta.Source],
  extraDependencies: Seq[String]
)
case class CompilationUnitCtxt(
  resolver: ContextSpecificResolver,
  filename: String,
  unit: scala.meta.Source,
  isTest: Boolean
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
  ctor: scala.meta.Stat
)
case class TypeParamCtxt(
  param: scala.meta.Type.Param
)