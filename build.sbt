import play.sbt.PlayLayoutPlugin
import play.twirl.sbt.SbtTwirl

lazy val commonSettings = Seq(
  version := "1.0.0-SNAPSHOT",
  organization := "org.combinators",
  
  scalaVersion := "2.12.4",

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeRepo("releases")
  ),

  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    /** For proper documentation, must include     "-groups",  **/
    "-language:implicitConversions"
  ),

  libraryDependencies ++= Seq(
    "org.combinators" %% "cls-scala" % "2.0.0-RC1",
    "org.combinators" %% "templating" % "1.0.0-RC1+4-ca285511",
    "org.combinators" %% "cls-scala-presentation-play-git" % "1.0.0-RC1+8-63d5cf0b",
    "org.scalactic" %% "scalactic" % "3.0.5" % "test",
    "org.scalatest" %% "scalatest" % "3.0.5" % "test",
    "com.chuusai" %% "shapeless" % "2.3.2",
    guice,
    "junit" % "junit" % "4.12",
    "org.scalameta" %% "scalameta" % "3.7.4"
  )
)

lazy val root = (Project(id = "expression-problem", base = file(".")))
  .settings(commonSettings: _*)
  .enablePlugins(SbtTwirl)
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin)
  .settings(
    moduleName := "expression-problem",

    sourceDirectories in (Compile, TwirlKeys.compileTemplates) := Seq(
      sourceDirectory.value / "main" / "java-templates",
      sourceDirectory.value / "main" / "python-templates"
    ),
    TwirlKeys.templateFormats += ("java" -> "org.combinators.templating.twirl.JavaFormat"),
    TwirlKeys.templateFormats += ("py" -> "org.combinators.templating.twirl.PythonFormat"),
    TwirlKeys.templateImports := Seq(),
    TwirlKeys.templateImports += "org.combinators.templating.twirl.Java",
    TwirlKeys.templateImports += "org.combinators.templating.twirl.Python",
    TwirlKeys.templateImports += "com.github.javaparser.ast._",
    TwirlKeys.templateImports += "com.github.javaparser.ast.body._",
    TwirlKeys.templateImports += "com.github.javaparser.ast.comments._",
    TwirlKeys.templateImports += "com.github.javaparser.ast.expr._",
    TwirlKeys.templateImports += "com.github.javaparser.ast.stmt._",
    TwirlKeys.templateImports += "com.github.javaparser.ast.`type`._",

    unmanagedResourceDirectories in Compile += sourceDirectory.value / "main" / "java",

    PlayKeys.playMonitoredFiles ++= (sourceDirectories in (Compile, TwirlKeys.compileTemplates)).value
  )

