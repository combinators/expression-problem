public class Node implements Tree {
    public final int label;
    java.util.List<Tree> subtrees = new java.util.ArrayList<Tree>();

    public boolean equals (Object o) {
        if (o == null) { return false; }
        if (o instanceof Tree) {
            return same((Tree) o);
        }
        return false;
    }

    public Node(java.util.List<Tree> children, int label) {
        this.label = label;
        subtrees.addAll(children);
    }

    public java.util.Optional<Node> asNode() { return java.util.Optional.of(this); }

    public String toString() {
        String children = "";
        for (Tree t : subtrees) {
            children += t.toString() + ", ";
        }
        return "[Label=" + label + ", " + children + "]";
    }
}