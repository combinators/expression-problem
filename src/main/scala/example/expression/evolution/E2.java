package example.expression.evolution;

import expression.DomainModel;
import expression.extensions.PrettyP;
import expression.history.History;

import java.util.Collections;

public class E2 {

    public static History extend(History h) {

        h.extend("e2",  new DomainModel(
            Collections.emptyList(),
            Collections.singleton(new PrettyP())
        ));

        return h;
    }
}
