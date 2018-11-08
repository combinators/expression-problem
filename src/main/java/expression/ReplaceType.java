package expression;

import com.github.javaparser.JavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.ConstructorDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.type.Type;
import com.github.javaparser.ast.visitor.VoidVisitorAdapter;

/**
 * Given various JavaParser elements, replace base type with covariant type.
 *
 * Used by Trivially, Interpreter and Algebra.
 */
public class ReplaceType {

    public static void main (String[] args) {
        CompilationUnit unit = JavaParser.parse("public class X { Exp a; public X (Exp a) { } public Exp some(String a, Exp b) { return null; } Exp b; String c; }");
        System.out.println(unit.toString());
        replace(unit, JavaParser.parseType("Exp"), JavaParser.parseType("Replacement"));
        System.out.println(unit.toString());
    }

    public static void replace (CompilationUnit unit, Type original, Type covariant ) {
        unit.accept (new VoidVisitorAdapter<Void>() {

            public void visit(FieldDeclaration field, Void arg) {
                if (field.getElementType().equals(original)) {
                    field.getElementType().replace(covariant);
                }
            }

            public void visit(ConstructorDeclaration cons, Void arg) {
                cons.getParameters().forEach(p -> { if (p.getType().equals(original)) { p.getType().replace(covariant); }});
            }

            public void visit(MethodDeclaration method, Void arg) {
                if (method.getType().equals(original)) {
                    method.getType().replace(covariant);
                }

                method.getParameters().forEach(p -> { if (p.getType().equals(original)) { p.getType().replace(covariant); }});
            }
        }, null);
    }

    public static void replace (MethodDeclaration method, Type original, Type covariant ) {
        method.accept (new VoidVisitorAdapter<Void>() {

            public void visit(Type tpe, Void arg) {
                if (tpe.equals(original)) {
                    tpe.replace(covariant);
                }
            }
        }, null);

        method.getParameters().forEach(p -> { if (p.getType().equals(original)) { p.getType().replace(covariant); }});
    }
}
