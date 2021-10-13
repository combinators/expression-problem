/** Settings shared globally. **/
lazy val commonSettings = Seq(
  version := "1.0.0-SNAPSHOT",
  organization := "org.combinators",

  scalaVersion := "2.12.13",

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeRepo("releases")
  ),

  Compile/scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:implicitConversions",
    "-Ypartial-unification",
    "-language:higherKinds"
  ),

  Compile/scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:implicitConversions"
  ),

  libraryDependencies ++= Seq(
    "org.combinators" %% "jgitserv" % "0.0.1",
    "org.scalactic" %% "scalactic" % "3.2.2" % "test",
    "org.scalatest" %% "scalatest" % "3.2.2" % "test",
    "org.scalameta" %% "scalameta" % "4.4.27",
    "org.scalameta" %% "contrib" % "4.1.6",
    "org.typelevel" %% "cats-core" % "2.3.1",
    "org.typelevel" %% "cats-free" % "2.3.1",
    "org.typelevel" %% "cats-effect" % "2.3.1"
  ),
  evictionErrorLevel := Level.Info,

  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
)

/** The core components to model expression problem code generators and domains.
  * Things in here are (DI, LI, AI).
  */
lazy val core = (Project(id = "core", base = file("core")))
  .settings(commonSettings: _*)
  .settings(
    moduleName := "expression-problem-core",
    //addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
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
  */
def standardLanguageProject(languageName: String): Project =
  (Project(id = s"language-$languageName", base = file(s"language/$languageName")))
    .settings(commonSettings: _*)
    .settings(
      moduleName := s"expression-problem-language-$languageName",
    )
    .dependsOn(core, domainMath, domainShape)


lazy val languageJava =
  standardLanguageProject("java")
    .settings(libraryDependencies += "com.github.javaparser" % "javaparser-core" % "3.19.0")
lazy val languageScala = standardLanguageProject("scala")

lazy val helloWorld:Project =
  (Project(id = s"helloworld", base = file(s"helloworld")))
    .settings(commonSettings: _*)
    .settings(
      moduleName := s"helloworld",
    )
    .dependsOn(core, languageJava, languageScala)

//lazy val languageGJ = standardLanguageProject("gj")
//lazy val languageCPP = standardLanguageProject("cpp")
//lazy val languageHaskell = standardLanguageProject("haskell")