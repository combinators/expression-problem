package example.expression.visitor.e2;

import expression.data.Add;
import expression.data.Eval;
import expression.data.Lit;
import expression.extensions.PrettyP;
import expression.extensions.Sub;
import expression.instances.Expression;
import expression.instances.UnitSuite;

public class TestCases {

    static Expression exp = new Expression(new expression.instances.BinaryExp(new Add(),
            new expression.instances.BinaryExp(new Sub(),
                    new expression.instances.Lit(new Lit(),1.0),
                    new expression.instances.Lit(new Lit(),2.0)),
            new expression.instances.BinaryExp(new Add(),
                    new expression.instances.Lit(new Lit(),8.0),
                    new expression.instances.Lit(new Lit(),2.0))));

    public static UnitSuite add(UnitSuite suite) {
        exp.add(new Eval(), 9.0);

        exp.add(new PrettyP(), "((1.0-2.0)+(8.0+2.0))");

        suite.addTest(exp);
        return suite;
    }
}