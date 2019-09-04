import play.sbt.PlayLayoutPlugin
import play.twirl.sbt.SbtTwirl


/** Settings shared globally. Updating to next version of SBT. **/
lazy val commonSettings = Seq(
  version := "1.0.0-SNAPSHOT",
  organization := "org.combinators",

  scalaVersion := "2.12.9",

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeRepo("releases")
  ),

  scalacOptions in (Compile) ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:implicitConversions",
    "-Ypartial-unification",
    "-language:higherKinds"
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
    "org.combinators" %% "cls-scala-presentation-play-git" % "1.0.0-RC1+8-63d5cf0b",
    "org.scalactic" %% "scalactic" % "3.0.5" % "test",
    "org.scalatest" %% "scalatest" % "3.0.5" % "test",
    "org.scalameta" %% "scalameta" % "3.7.4",
    "org.typelevel" %% "cats-core" % "2.0.0-RC1",
    "org.typelevel" %% "cats-free" % "2.0.0-RC1"
  )
)

/** The core components to model expression problem code generators and domains.
  * Things in here are (DI, LI, AI).
  */
lazy val core = (Project(id = "core", base = file("core")))
  .settings(commonSettings: _*)
  .settings(
    moduleName := "expression-problem-core",
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
  )

/** Template for a subproject for a specific domain named `domainName`.
  * These projects should be (DD, LI, AI).
  */
def standardDomainProject(domainName: String): Project =
  (Project(id = s"domain-$domainName", base = file(s"domain/$domainName")))
    .settings(commonSettings: _*)
    .settings(
      moduleName := s"expression-problem-domain-$domainName"
    )
    .dependsOn(core)

/** The domain of math with arithmetic expressions. **/
lazy val domainMath = standardDomainProject("math")
/** The domain of geometric shapes. **/
lazy val domainShape = standardDomainProject("shape")

/** Template for a subproject for a specific language and its EP approaches.
  * Contains code in the set {DD, DI} x LD x {AD, AI}.
  * Includes startable play server to host generated solutions.
  */
def standardLanguageProject(languageName: String): Project =
  (Project(id = s"language-$languageName", base = file(s"language/$languageName")))
    .settings(commonSettings: _*)
    .enablePlugins(PlayScala)
    .disablePlugins(PlayLayoutPlugin)
    .settings(
      moduleName := s"expression-problem-language-$languageName",
      libraryDependencies += guice
    )
    .dependsOn(core, domainMath, domainShape)


lazy val languageJava = standardLanguageProject("java")
lazy val languageGJ = standardLanguageProject("gj")
lazy val languageCPP = standardLanguageProject("cpp")
lazy val languageHaskell = standardLanguageProject("haskell")
lazy val languageScala = standardLanguageProject("scala")

