package example.expression.evolution;

import expression.DomainModel;
import expression.extensions.Collect;
import expression.history.History;
import expression.operations.SimplifyExpr;

import java.util.Arrays;
import java.util.Collections;

public class E4 {

    public static History extend(History h) {

        h.extend("e4",  new DomainModel(
            Collections.emptyList(),
            Arrays.asList(new Collect(), new SimplifyExpr())

        ));

        return h;
    }
}
