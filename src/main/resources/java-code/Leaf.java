public class Leaf implements Tree {
    public final Object value;

    public boolean equals (Object o) {
        if (o == null) { return false; }
        if (o instanceof Tree) {
            return same((Tree) o);
        }
        return false;
    }

    public Leaf(Object e) {
        value = e;
    }

    public java.util.Optional<Leaf> asLeaf() { return java.util.Optional.of(this); }

    public String toString() {
        return "(Leaf:" + value + ")";
    }
}
