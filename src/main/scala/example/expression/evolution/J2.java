package example.expression.evolution;

import expression.DomainModel;
import expression.extensions.PrettyP;
import expression.history.History;

import java.util.Collections;

public class J2 {

    public static History extend(History h) {

        h.extend("m2",  new DomainModel(
            Collections.emptyList(),
            Collections.singleton(new PrettyP())
        ));

        return h;
    }
}
