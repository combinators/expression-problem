@(rootPackage:Name)

package @{Java(rootPackage)};

public class Leaf implements Tree {
    public final Object value;

    public Leaf(Object e) {
        value = e;
    }

    public java.util.Optional<Leaf> asLeaf() { return java.util.Optional.of(this); }
}
