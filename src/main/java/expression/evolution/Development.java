package expression.evolution;

import expression.DomainModel;
import expression.data.Add;
import expression.data.Eval;
import expression.data.Lit;
import expression.extensions.*;
import expression.history.History;
import expression.operations.SimplifyExpr;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;

public class Development {

    // base.e1.e2.e3.e4.e5 is the evolution
    static final String baseDomain = "e0";
    static final String evolution1 = "e1";
    static final String evolution2 = "e2";
    static final String evolution3 = "e3";
    static final String evolution4 = "e4";

    /** This is the starting point for all evolutions. Currently linear, but could have divergence as needed. */

    // HACK:
    // TODO: FIXME: ISSUE is the the Domain Model doesn't know its parent, whic hmakes the rest of the code
    // break. Not sure how to handle this....
    public final static History<DomainModel> base = new History<>(
        new DomainModel(
            new ArrayList<>(Arrays.asList(new Lit(), new Add())),
            new ArrayList<>(Collections.singletonList(new Eval())))
        , baseDomain);

    public final  static boolean evolution1Success = base.extend(baseDomain,
        new DomainModel(
            new ArrayList<>(Collections.singletonList(new Sub())),
            new ArrayList<>(Collections.emptyList()))
        , evolution1);

    public final static boolean evolution2Success = base.extend(evolution1,
        new DomainModel(
            new ArrayList<>(Collections.emptyList()),
            new ArrayList<>(Collections.singletonList(new PrettyP())))
        , evolution2);

    public final static boolean evolution3Success = base.extend(evolution2,
        new DomainModel(
            new ArrayList<>(Arrays.asList(new Neg(), new Mult(), new Divd())),
            new ArrayList<>(Collections.emptyList()))
        , evolution3);

    public final static boolean evolution4Success = base.extend(evolution3,
        new DomainModel(
            new ArrayList<>(Collections.emptyList()),
            new ArrayList<>(Arrays.asList(new Collect(), new SimplifyExpr())))
        , evolution4);

}
