package expression.instances;

import expression.data.Eval;
import expression.extensions.Collect;
import expression.extensions.PrettyP;

import java.util.ArrayList;

public class SimpleTest {
    public UnitSuite build() {
        UnitSuite suite = new UnitSuite();

        Instance test = new SimpleStructure().build();

        // these are the expected results for the given operations
        UnitTest base = new UnitTest(test, new Eval(), new Double(35));
        UnitTest second = new UnitTest(test, new PrettyP(), "((5*7)+(8/9))");

        ArrayList<Instance> collected = new ArrayList<>();
        collected.add(new expression.instances.Lit(5));
        collected.add(new expression.instances.Lit(7));
        collected.add(new expression.instances.Lit(8));
        collected.add(new expression.instances.Lit(9));

        UnitTest third = new UnitTest(test, new Collect(), collected);

        suite.add(base);
        suite.add(second);
        suite.add(third);

        return suite;
    }
}
