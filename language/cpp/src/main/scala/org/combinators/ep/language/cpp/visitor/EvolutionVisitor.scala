package org.combinators.ep.language.cpp.visitor    /*DD:LD:AD*/

import javax.inject.Inject
import org.combinators.cls.git.Results
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.language.cpp.CPPFileUtils.PersistCPPFile
import org.combinators.ep.deployment.CodeGenerationController
import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.math.MathDomain
import org.combinators.ep.generator.FileWithPath
import org.combinators.ep.language.cpp._

/**
  * Build with:
  *
  * g++ *.cpp -I ../cpputest/include -L ../cpputest/cpputest_build/lib -lCppUTest -lCppUTestExt -std=c++11
  *
  * Each test_ex.cpp file has its own main
  * @param web
  * @param app
  */
abstract class Foundation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
  extends CodeGenerationController[CPPFile](web, app)
{
  val gen:WithDomain[MathDomain] with CPPVisitorGenerator with TestGenerator

override lazy val generatedCode:Seq[CPPFile] =
    gen.generatedCode() ++
    gen.generateBinaryMethodHelpers() ++
    gen.generateSuite(routingPrefix)

  /**
    * Add all helper classes to be external artifacts.
    * Has to be lazy so subclasses can compute model.
    */
  override lazy val results:Results = gen.helperClasses().foldLeft(defaultResults(generatedCode))((former, next) => former.addExternalArtifact[FileWithPath](next))

  override val routingPrefix: Option[String] = Some("cppVisitor")
  override lazy val controllerAddress:String = gen.getModel.name
}

//class CPP_M0_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator with cpp_e0
//}
//
//class CPP_M1_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator with cpp_e0 with cpp_e1
//}
//
//class CPP_M2_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator with cpp_e0 with cpp_e1 with cpp_e2
//}
//
//class CPP_M3_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3
//}
//
//class CPP_M4_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 with cpp_e4
//}
//
//class CPP_M5_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 with cpp_e4 with cpp_e5
//}
//
//class CPP_M6_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle)
//  extends Foundation(web, app) {
//  override val gen = new WithDomain(MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 with cpp_e4 with cpp_e5 with cpp_e6
//}
