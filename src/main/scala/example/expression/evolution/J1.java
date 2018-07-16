package example.expression.evolution;

import expression.DomainModel;
import expression.extensions.Sub;
import expression.history.History;

import java.util.Collections;

public class J1 {

    public static History extend(History h) {

        h.extend("m1",  new DomainModel(
            Collections.singleton(new Sub()),
            Collections.emptyList()
        ));

        return h;
    }
}
