package example.expression.visitor.e4;

import expression.DomainModel;
import expression.data.Add;
import expression.data.Eval;
import expression.data.Lit;
import expression.extensions.*;
import expression.instances.Expression;
import expression.instances.Instance;
import expression.instances.UnitSuite;
import expression.operations.SimplifyExpr;

import java.util.ArrayList;

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

        ArrayList<Instance> collected = new ArrayList<>();
        collected.add(new expression.instances.Lit(new Lit(),5.0));
        collected.add(new expression.instances.Lit(new Lit(),7.0));
        collected.add(new expression.instances.Lit(new Lit(),18.0));
        collected.add(new expression.instances.Lit(new Lit(),9.0));

        // will be hard to check for equality since literals will
        // appear in any order (and might be duplicated).
        test.add(new Collect(), collected);
        suite.addTest(test);

        // (5*7) / 1 --> just (5*7)
        Instance mult = new expression.instances.BinaryExp(new Mult(),
                    new expression.instances.Lit(new Lit(),5.0),
                    new expression.instances.Lit(new Lit(),7.0));

        Expression exp = new Expression(new expression.instances.BinaryExp(new Divd(),
                mult,
                new expression.instances.Lit(new Lit(),1.0)));

        exp.add(new SimplifyExpr(), mult);


        suite.addTest(exp);
        return suite;
    }
}