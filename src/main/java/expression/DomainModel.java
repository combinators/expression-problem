package expression;

import java.util.*;

/**
 * Represents the desired features.
 *
 * Note that 'Exp' is not in the domain of data types, only sub-types are.
 */
public class DomainModel {

    /** Desired data types. */
    public List<Exp> data = new ArrayList<>();

    /** Desired operations. */
    public List<Operation> ops = new ArrayList<>();

    public DomainModel() {
        super();
    }

    /**
     * Construct initial Domain model from list of subtypes and operations.
     *
     * @param data    desired subtypes
     * @param ops     desired operations
     */
    public DomainModel(Collection<Exp> data, Collection<Operation> ops) {
        this.data.addAll(data);
        this.ops.addAll(ops);
    }

    /**
     * Merge to create a new domain model, with merged data and operations.
     *
     * @param dm    Domain Model used to merge with current domain model.
     * @return      Returns a new DomainModel object merging data and operations.
     */
    public DomainModel merge(DomainModel dm) {
        Set<Exp> mergedData = new HashSet<>(data);
        mergedData.addAll(dm.data);

        Set<Operation> mergedOps = new HashSet<>(ops);
        mergedOps.addAll(dm.ops);

        return new DomainModel(mergedData, mergedOps);
    }

}
