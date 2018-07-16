package example.expression

import com.github.javaparser.ast.CompilationUnit
import org.combinators.cls.interpreter.ReflectedRepository
import example.expression.visitor.ExpressionSynthesis
import expression.DomainModel
import expression.data.{Add, Eval, Lit}
import expression.extensions.{Collect, Neg, PrettyP, Sub}
import org.scalatest.FunSpec
import test.Helper

import scala.collection.JavaConverters._

class ExpressionTests extends FunSpec  {


  describe("The possible inhabited domain models") {
    // Configure the desired (sub)types and operations
    // no need to add 'Exp' to the model, since assumed always to be there
    // operations to have (including Eval). Could add SimplifyAdd
    val domainModel:DomainModel = new DomainModel(
      List(new Lit, new Add, new Neg, new Sub).asJava,
      List(new Eval, new PrettyP, new Collect).asJava
    )

    describe("(using the only possible domain model)") {
      describe("the domain model") {
        it("should have four data subtypes") {
          assert(domainModel.data.size == 4)
        }
        it("should have three ops") {
          assert(domainModel.ops.size == 3)
        }
      }

      // TODO: FIXME
      /*describe ("for synthesis") {
        val repo = new ExpressionSynthesis(domainModel) {}
        import repo._

        val Gamma = ReflectedRepository(repo, classLoader = repo.getClass.getClassLoader)

        val helper = new Helper()

        it ("Check for base classes") {
          val result = Gamma.inhabit[CompilationUnit](generated(generated.visitor))
          assert(helper.singleClass("Visitor", result))

          // ensure the number of methods is equal to the number of subtypes.
          val inhab: Iterator[CompilationUnit] = result.interpretedTerms.values.flatMap(_._2).iterator
          val unit:CompilationUnit = inhab.next

          // Count number of methods; must be one per subType. Make sure each one has name 'visit'
          assertResult(domainModel.data.size, "one method per data type")(helper.methods(unit).length)

          for (m <- helper.methods(unit)) {
            assertResult(m.getNameAsString, "method named visit")("visit")
          }
        }
      }*/
    }
  }
}
