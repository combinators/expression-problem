package expression.instances;

public class StringVisitor extends Visitor {

    StringBuffer buffer = new StringBuffer();

    @Override
    void visit(Instance op) {
        buffer.append(op.toString());
    }

    public String toString() {
        return buffer.toString();
    }
}
