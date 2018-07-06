package example.expression.evolution;

import expression.DomainModel;
import expression.data.Add;
import expression.data.Eval;
import expression.data.Lit;
import expression.history.History;

import java.util.Arrays;
import java.util.Collections;

public class J0 {

    public static History extend(History h) {
        h.extend("m0", new DomainModel(
            Arrays.asList(new Lit(), new Add()),
            Collections.singleton(new Eval())
        ));

        return h;
    }
}
