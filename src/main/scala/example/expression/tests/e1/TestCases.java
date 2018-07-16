package example.expression.tests.e1;

import expression.data.Eval;
import expression.data.Lit;
import expression.extensions.Sub;
import expression.instances.Expression;
import expression.instances.UnitSuite;

public class TestCases {

    static Expression exp = new Expression( new expression.instances.BinaryExp(new Sub(),
                    new expression.instances.Lit(new Lit(),1.0),
                    new expression.instances.Lit(new Lit(),2.0)));

    public static UnitSuite add(UnitSuite suite) {
        exp.add(new Eval(), -1.0);
        suite.addTest(exp);

        return suite;
    }
}
