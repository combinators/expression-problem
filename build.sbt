lazy val commonSettings = Seq(
  version := "1.0.0-SNAPSHOT",
  organization := "org.combinators",
  
  scalaVersion := "2.12.4",

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeRepo("releases")
  ),

  scalacOptions in Compile ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:implicitConversions"
  ),

  scalacOptions in (Compile,doc) ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-groups",
    "-language:implicitConversions"
  ),


  libraryDependencies ++= Seq(
    "org.combinators" %% "templating" % "1.0.0-RC1+4-ca285511",

    "commons-io" % "commons-io" % "2.15.1",
    "org.scalactic" %% "scalactic" % "3.0.5" % "test",
    "com.chuusai" %% "shapeless" % "2.3.2",
    guice,
    "junit" % "junit" % "4.12",
    "org.scalameta" %% "scalameta" % "3.7.4"
  )
)

lazy val root = Project(id = "expression-problem", base = file("."))
  .settings(commonSettings: _*)
  .enablePlugins(SbtTwirl)
  .settings(
    moduleName := "expression-problem",

    TwirlKeys.templateFormats += ("java" -> "org.combinators.templating.twirl.JavaFormat"),
    TwirlKeys.templateImports := Seq(),
    TwirlKeys.templateImports += "org.combinators.templating.twirl.Java",
    TwirlKeys.templateImports += "com.github.javaparser.ast._",
    TwirlKeys.templateImports += "com.github.javaparser.ast.body._",
    TwirlKeys.templateImports += "com.github.javaparser.ast.comments._",
    TwirlKeys.templateImports += "com.github.javaparser.ast.expr._",
    TwirlKeys.templateImports += "com.github.javaparser.ast.stmt._",
    TwirlKeys.templateImports += "com.github.javaparser.ast.`type`._",

    unmanagedResourceDirectories in Compile += sourceDirectory.value / "main" / "java"
  )
