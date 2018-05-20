package example.expression.evolution;

import expression.DomainModel;
import expression.extensions.Sub;
import expression.history.History;

import java.util.Collections;

public class E1 {

    public static History extend(History h) {

        h.extend("e1",  new DomainModel(
            Collections.singleton(new Sub()),
            Collections.emptyList()
        ));

        return h;
    }
}
