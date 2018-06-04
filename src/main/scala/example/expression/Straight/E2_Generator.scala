package example.expression.Straight

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import example.expression.j.{TestGenerator, e2}
import org.combinators.templating.twirl.Java

trait E2_Generator extends StraightGenerator with TestGenerator with e2 {

}
