@(rootPackage:Name)

package @{Java(rootPackage)};

public class Node implements Tree {
    public final int label;
    java.util.List<Tree> subtrees = new java.util.ArrayList<Tree>();

    public Node(java.util.List<Tree> children, int label) {
        this.label = label;
        subtrees.addAll(children);
    }

    public java.util.Optional<Node> asNode() { return java.util.Optional.of(this); }
}