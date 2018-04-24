package expression.instances;

import java.util.ArrayList;
import java.util.Iterator;

public class UnitSuite implements Iterable<UnitTest> {
    ArrayList<UnitTest> tests = new ArrayList<>();

    public void add(UnitTest test) {
        tests.add(test);
    }

    @Override
    public Iterator<UnitTest> iterator() {
       return tests.iterator();
    }
}
