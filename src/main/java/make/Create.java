package make;

import java.io.*;
import java.util.*;

/**
 *
 */
public class Create {
    /** Known Java Variations to generate. */
    static final Evolution[] standardEvolutions  = {
            new Evolution ("M0"),
            new Evolution ("M1", "M0"),
            new Evolution ("M2", "M1"),
            new Evolution ("M3", "M2"),
            new Evolution ("M4", "M3"),
            new Evolution ("M5", "M4"),
            new Evolution ("M6", "M5"),
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
            return name.toLowerCase();
        }
    };

    /** Known Java Variations to generate. */
    static final Evolution[] independentEvolutions  = {
            new Evolution("I1", "M1"),
            new Evolution("I2", "I1"),
    };

    static final Evolution[] mergedEvolutions  = {
            new MergedEvolution("C1", "I2", "M3")
    };

    static final Evolution[] shapeEvolutions  = {
            new Evolution("S0"),
            new Evolution("S1", "S0")
    };

    /** Where routes file is to be placed for play. */
    static final String resources = "src" + File.separator + "main" + File.separator + "resources" + File.separator + "routes";

    /** All synthesized scala build files are stored in this package. */
    static final String destination = "src" + File.separator + "main" + File.separator + "scala" + File.separator + "build";

    // each language has a number of possible variations. Names of languages are used to
    // construct package names, i.e., "ep.j" and "ep.haskell"
    static final Language lang_java = new Language("j")
            .addEvolutions(standardEvolutions)
            .addEvolutions(independentEvolutions)
            .addEvolutions(shapeEvolutions)
            .addMapping(getJavaName)
            .addEvolutions(mergedEvolutions)
            .add("oo", "WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator")
            .add("algebra", "WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator")
            .add("extensibleVisitor", "WithDomain(MathDomain) with ExtensibleVisitorGenerator with ExtensibleVisitorTestGenerator")
            .add("trivially", "new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator")
            .add("visitor", "new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator");
    static final Language lang_haskell = new Language("haskell")
            .addMapping(getHaskellName)
            .addEvolutions(standardEvolutions)
            .add("alacarte", "WithDomain[MathDomain] with ALaCarteGenerator with ALaCarteTestGenerator")
            .add("grow", "new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator")
            .add("sraight", "new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator");
    static final Language lang_cpp = new Language("cpp")
            .addMapping(getCPPName)
            .addEvolutions(standardEvolutions)
            .add("oo", "WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator")
            .add("visitor", "WithDomain(MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator")
            .add("visitorTable", "WithDomain(MathDomain) with CPPVisitorTableGenerator with CPPTableTestGenerator");
    static final Language lang_gj = new Language("gj")
            .addMapping(getGJName)
            .addEvolutions(standardEvolutions)
            .add("gj", "WithDomain(MathDomain) with WadlerGenerator with TestGenerator");  // not really anything good

    /** Could have used reflection, but this is simpler. */
    static final Language[] allLanguages = { lang_java, lang_haskell, lang_cpp, lang_gj };

    static String create(Language lang, Evolution ev, String packageStruct) {
        String name = ev.name;
        String scalaClass = "class " + name + "_Variation @Inject()(web: WebJarsUtil, app: ApplicationLifecycle) extends Foundation(web, app)";
        String evolutions = "";

        // lower-case names for all evolutions
        Iterator<String> past = ev.evolutions();
        while (past.hasNext()) {
            String ps = past.next();
            evolutions = evolutions + " with " + lang.mapping.instance(ps);
        }

        String override = "override val gen = new " + packageStruct + evolutions;

        String clazz = scalaClass + "\n" + override + "\n}\n}";
        return clazz;
    }

    public static void main(String[] args) {
        String imports = "";
//                package build.j.oo
//
//        /* Generated:  */
//import ep.domain._
//import ep.j._
//import ep.j.oo._
//import javax.inject.Inject
//import org.webjars.play.WebJarsUtil
//import play.api.inject.ApplicationLifecycle

        File output = new File(destination);

        // for each language
        for (Language lang : allLanguages) {
            File langDir = new File (output, lang.name);

            // for each family
            for (String variation: lang) {
                String packageName = "ep." + lang.name + "." + variation;
                String packageStruct = lang.constructors.get(variation);
                File varDir = new File (langDir, variation);

                for (Evolution ev : lang.evolutions) {
                    System.out.println(lang.name + "\t" + variation + "\t" + ev.name);
                    String clazzDefinition = create(lang, ev, packageStruct);

                    File build = new File (varDir, "build.scala");

                }
            }
        }
    }

}
