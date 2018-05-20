package example.expression.tests.e3;

import expression.data.Add;
import expression.data.Eval;
import expression.data.Lit;
import expression.extensions.Divd;
import expression.extensions.Mult;
import expression.extensions.Neg;
import expression.extensions.PrettyP;
import expression.instances.Expression;
import expression.instances.UnitSuite;

public class TestCases {

    /** This is invoked from the superclass, not directly here. */
    static Expression test = new Expression(
            new expression.instances.BinaryExp(new Add(),
                    new expression.instances.BinaryExp(new Mult(),
                            new expression.instances.Lit(new Lit(),5),
                            new expression.instances.Lit(new Lit(),7)),
                    new expression.instances.BinaryExp(new Divd(),
                            new expression.instances.Lit(new Lit(),18),
                            new expression.instances.UnaryExp(new Neg(), new expression.instances.Lit(new Lit(),9))
                    )
            )
    );

    public static UnitSuite add(UnitSuite suite) {

        test.add(new Eval(), 33.0);
        test.add(new PrettyP(), "((5.0*7.0)+(18.0/-9.0))");

        suite.addTest(test);
        return suite;
    }
}