package make;

import java.io.*;
import java.util.*;

/**
 * Create the routes file to be used by Play engine and individual Scala classes that represent the specific
 * evolutions for different languages and approaches.
 */
public class Create {

    /** Where routes file is to be placed for play. */
    static final String routes = "src" + File.separator + "main" + File.separator + "resources" + File.separator + "routes";

    /** All synthesized scala build files are stored in this package. */
    static final String destination = "src" + File.separator + "main" + File.separator + "scala" + File.separator + "build";

    /** Known Java Variations to generate. */
    static final Evolution[] standardEvolutions  = {
            new Evolution ("M0"),
            new Evolution ("M1", "M0"),
            new Evolution ("M2", "M1"),
            new Evolution ("M3", "M2"),
            new Evolution ("M4", "M3"),
            new Evolution ("M5", "M4"),
            new Evolution ("M6", "M5"),
            new Evolution ("M7", "M6"),
    };

    /** Known Java Variations to generate. */
    static final Evolution[] gjEvolutions  = {
            new Evolution ("M0"),
            new Evolution ("M1", "M0"),
    };

    static Instance getJavaName = new Instance() {
        public String instance(String name) {
            if (name.charAt(0) == 'M') {
                return "e" + name.charAt(1);
            }

            return name.toLowerCase();
        }
    };

    static Instance getCPPName = new Instance() {
        public String instance(String name) {
            if (name.charAt(0) == 'M') {
                return "cpp_e" + name.charAt(1);
            }

            // nothing else.
            return name.toLowerCase();
        }
    };

    static Instance getHaskellName = new Instance() {
        public String instance(String name) {
            if (name.charAt(0) == 'M') {
                return "e" + name.charAt(1);
            }

            // nothing else.
            return name.toLowerCase();
        }
    };

    static Instance getGJName = new Instance() {
        public String instance(String name) {
            if (name.charAt(0) == 'M') {
                return "e" + name.charAt(1);
            }

            return name.toLowerCase();
        }
    };

    /** Known Java Variations to generate. */
    static final Evolution[] independentEvolutions  = {
            new Evolution("I1", "M1"),
            new Evolution("I2", "I1"),
            new Evolution("P1", "M2")
    };

    // HACK. TODO: Has to be M3 first otherwise ordering in generated code doesn't match.
    static final Evolution[] mergedEvolutions  = {
            new MergedEvolution("C1", "M3", "I2")
    };

    static final Evolution[] shapeEvolutions  = {
            new Evolution("S0"),
            new Evolution("S1", "S0")
    };

    // each language has a number of possible variations. Names of languages are used to
    // construct package names, i.e., "ep.j" and "ep.haskell"
    static final Language lang_java = new Language("j")
            .addEvolutions(standardEvolutions)
            .addEvolutions(independentEvolutions)
            //.addEvolutions(shapeEvolutions)
            .addMapping(getJavaName)
            .addEvolutions(mergedEvolutions)
            .add("algebra", "WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator")
            .add("extensibleVisitor", "WithDomain(MathDomain) with ExtensibleVisitorGenerator with ExtensibleVisitorTestGenerator")
            .add("interpreter", "WithDomain(MathDomain) with InterpreterGenerator with InterpreterTestGenerator")
            .add("oo", "WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator")
            .add("trivially", "WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator")
            .add("visitor", "WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator");
    static final Language lang_haskell = new Language("haskell")
            .addMapping(getHaskellName)
            .addEvolutions(standardEvolutions)
            .add("alacarte", "WithDomain[MathDomain] with ALaCarteGenerator with ALaCarteTestGenerator")
            .add("grow", "WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator")
            .add("straight", "WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator");
    static final Language lang_cpp = new Language("cpp")
            .addMapping(getCPPName)
            .addEvolutions(standardEvolutions)
            .add("oo", "WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator")
            .add("visitor", "WithDomain(MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator")
            .add("visitorTable", "WithDomain(MathDomain) with CPPVisitorTableGenerator with CPPTableTestGenerator");
    static final Language lang_gj = new Language("gj")
            .addMapping(getGJName)
            .addEvolutions(gjEvolutions)
            .add("wadler", "WithDomain(MathDomain) with WadlerGenerator with UnitTestGenerator");  // not really anything good

    /** Could have used reflection, but this is simpler. */
    static final Language[] allLanguages = { lang_java, lang_haskell, lang_cpp, lang_gj };

    static String create(Language lang, Evolution ev, String packageStruct) {
        String name = ev.name;
        String scalaClass = "class " + name + "_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app) {";
        String evolutions = "";

        // lower-case names for all evolutions. Must make sure no duplicates
        // LAST ONE selected is the name of the evolution to be selected.
        Iterator<String> past = ev.evolutions();

        while (past.hasNext()) {
            String ps = past.next();
            String trait  = lang.mapping.instance(ps);
            if (evolutions.equals("")) {
                evolutions = trait;
            } else {
                evolutions = trait + " with " + evolutions;
            }
        }

        String override = "override val gen = new " + packageStruct.replace("[", "(").replace("]", ")") + " with " + evolutions;

        return scalaClass + "\n" + override + "\n}";
    }

    /**
     * Each Routes entry is of the following form:
     *
     * ->    /                              ep.scala.oo.M0_Variation
     *
     * @param args
     */
    public static void main(String[] args) throws Exception {
        File rf = new File (routes);
        PrintWriter routesFile = new PrintWriter(rf);

        File output = new File(destination);
        if (!output.exists()) {
            output.mkdir();
        }

        // for each language
        for (Language lang : allLanguages) {
            File langDir = new File (output, lang.name);
            if (!langDir.exists()) {
                langDir.mkdir();
            }

            // for each family
            for (String variation: lang) {
                String packageName = "build." + lang.name + "." + variation;
                String packageStruct = lang.constructors.get(variation);
                File varDir = new File (langDir, variation);
                if (!varDir.exists()) {
                    varDir.mkdir();
                }
                File build = new File (varDir, "build.scala");
                System.out.println ("  " + lang.name + "\t" + variation );

                try {
                    PrintWriter pw_output = new PrintWriter(build);

                    pw_output.println("package " + packageName);
                    pw_output.println("/* Generated: " + new Date() + " */");
                    pw_output.println("import ep.domain._");
                    pw_output.println("import ep." + lang.name + "._");
                    pw_output.println("import ep." + lang.name + "." + variation + "._");
                    pw_output.println("import javax.inject.Inject");
                    pw_output.println("import org.webjars.play.WebJarsUtil");
                    pw_output.println("import play.api.inject.ApplicationLifecycle");

                    //String traits = "";
                    routesFile.print ("# " + variation + "(" + lang.name + ") evolutions: ");
                    for (Evolution ev : lang.evolutions) {
                        System.out.print (ev.name + ", ");
                        String clazzDefinition = create(lang, ev, packageStruct);

                        pw_output.println("/* ");
                        pw_output.println(" * " + variation + " solution in " + lang.name + " for " + ev.name);
                        pw_output.println(" * ");
                        pw_output.println(" * @group evolutions ");
                        pw_output.println(" */");

                        pw_output.println(clazzDefinition);
                        // output routes information
                        routesFile.println ("->\t/\t\t " + packageName + "." + ev.name + "_Variation");
                    }
                    System.out.println();
                    pw_output.close();
                } catch (IOException ioe) {
                    ioe.printStackTrace();
                }
            }
        }

        routesFile.close();
        System.out.println ("Generated Routes file: " + rf.getAbsoluteFile());
    }

}
