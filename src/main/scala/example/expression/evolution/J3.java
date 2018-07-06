package example.expression.evolution;

import expression.DomainModel;
import expression.extensions.Divd;
import expression.extensions.Mult;
import expression.extensions.Neg;
import expression.history.History;

import java.util.Arrays;
import java.util.Collections;

public class J3 {

    public static History extend(History h) {

        h.extend("m3",  new DomainModel(
            Arrays.asList(new Neg(), new Mult(), new Divd()),
            Collections.emptyList()
        ));

        return h;
    }
}
