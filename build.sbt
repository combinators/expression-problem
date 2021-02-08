/** Settings shared globally. **/
lazy val commonSettings = Seq(
  version := "1.0.0-SNAPSHOT",
  organization := "org.combinators",

  scalaVersion := "2.12.13",

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
    "org.combinators" %% "templating" % "1.1.0",
    "org.combinators" %% "jgitserv" % "0.0.1",
    "org.scalactic" %% "scalactic" % "3.2.2" % "test",
    "org.scalatest" %% "scalatest" % "3.2.2" % "test",
    "org.scalameta" %% "scalameta" % "4.3.8",
    "org.scalameta" %% "contrib" % "4.1.6",
    "org.typelevel" %% "cats-core" % "2.3.1",
    "org.typelevel" %% "cats-free" % "2.3.1",
    "org.typelevel" %% "cats-effect" % "2.3.1"
  ),

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


lazy val languageJava = standardLanguageProject("java")
lazy val languageGJ = standardLanguageProject("gj")
lazy val languageCPP = standardLanguageProject("cpp")
lazy val languageHaskell = standardLanguageProject("haskell")
lazy val languageScala = standardLanguageProject("scala")


// TODO: This should be standalone at a later point.
/*lazy val jgitserv =
  Project(id = "jgitserv", base = file("jgitserv"))
    .settings(
      version := "1.0.0-SNAPSHOT",
      organization := "org.combinators",
      moduleName := "expression-problem-core",
      
      scalaVersion := "2.12.13",

      resolvers ++= Seq(
        Resolver.sonatypeRepo("releases"),
        Resolver.typesafeRepo("releases")
      ),

      scalacOptions in (Compile) ++= Seq(
        "-unchecked",
        "-deprecation",
        "-feature",
        "-language:implicitConversions"
      ),
      libraryDependencies ++= Seq(
        "com.github.finagle" %% "finchx-core" % "0.31.0",
        "org.eclipse.jgit" % "org.eclipse.jgit" % "5.4.0.201906121030-r",
        "commons-io" % "commons-io" % "2.6",
        "org.combinators" %% "templating" % "1.1.0",
        "ch.qos.logback" % "logback-classic" % "1.2.3"
      ),
      addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
    )
*/
