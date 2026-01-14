/** Settings shared globally. **/
lazy val commonSettings = Seq(
  version := "1.0.0-SNAPSHOT",
  organization := "org.combinators",

  scalaVersion := "3.7.4",
  
  resolvers += Resolver.typesafeRepo("releases"),
  resolvers ++= Resolver.sonatypeOssRepos("releases"),

  Compile/scalacOptions ++= Seq(
    "-explain",
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-Xkind-projector:underscores",
  ),

  libraryDependencies ++= Seq(
    "commons-io"% "commons-io" % "2.19.0",
    "org.combinators" %% "templating" % "1.1.5",
    "org.scalactic" %% "scalactic" % "3.2.19" % "test",
    "org.scalatest" %% "scalatest" % "3.2.19" % "test",
    "org.typelevel" %% "cats-core" % "2.13.0",
    "org.typelevel" %% "cats-free" % "2.13.0",
    "org.typelevel" %% "cats-effect" % "3.6.1"
  ),
  evictionErrorLevel := Level.Info,
)


/** The code generation infrastructure used in languages.
  * Things in here are (DI, LI, AI).
  */
lazy val cogen = (Project(id = "cogen", base = file("cogen")))
  .settings(commonSettings: _*)
  .settings(
    moduleName := "expression-problem-cogen",
  )


/** The core components to model expression problem code generators and domains.
  * Things in here are (DI, LI, AI).
  */
lazy val core = (Project(id = "core", base = file("core")))
  .settings(commonSettings: _*)
  .settings(
    moduleName := "expression-problem-core",
  )
  .dependsOn(cogen)

lazy val approach = (Project(id = "approach", base = file("approach")))
  .settings(commonSettings: _*)
  .settings(
    moduleName := "expression-problem-approach",
  )
  .dependsOn(cogen, core)


/** Template for a subproject for a specific domain named `domainName`.
  * These projects should be (DD, LI, AI).
  */
def standardDomainProject(domainName: String): Project =
  (Project(id = s"domain-$domainName", base = file(s"domain/$domainName")))
    .settings(commonSettings: _*)
    .settings(
      moduleName := s"expression-problem-domain-$domainName"
    )
    .dependsOn(cogen, core)

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
    .dependsOn(cogen)

lazy val languageJava =
  standardLanguageProject("java")
    .settings(libraryDependencies += "com.github.javaparser" % "javaparser-core" % "3.26.4")

lazy val helloWorldProject: Project =
  (Project(id = s"helloWorld", base = file(s"helloworld")))
    .settings(commonSettings: _*)
    .settings(
      moduleName := s"expression-problem-language-helloworld",
    )
    .dependsOn(cogen, languageJava, languageNewScala)

lazy val languageInbetween =
  standardLanguageProject("inbetween")

lazy val languageNewScala =
  standardLanguageProject("newScala")
    .dependsOn(languageInbetween)

lazy val builder =
  (Project(id = s"builder", base = file(s"builder")))
    .settings(commonSettings: _*)
    .settings(
      moduleName := s"expression-problem-language-builder",
    )
    .dependsOn(core, cogen, approach, domainMath, domainShape, languageJava, languageNewScala, helloWorldProject)


