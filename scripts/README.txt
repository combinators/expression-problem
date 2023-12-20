1. Generate all evolutions for all approaches

  1a. Execute runAllMain.bat, which computes code for all possible pairs, storing directories in target\analysis,
      as target\analysis\src-%%a-%%e where %%a is the approach and %%e is the evolution.

       Main File:    org.combinators.ep.language.java.DirectToDiskMain
       Evolutions:   M0 M1 M2 M3 M4 M5 M6 M7 M7I2 M8 M9 I1 A1 A1M3 A1M3I2 A3 I2
       Approaches:   oo visitor visitorSideEffect extensibleVisitor interpreter dispatch trivially coco algebra

  Note: Once done, move "target\analysis" to "target\analysis-Main" so it is not affected by following

  1b. Execute runAl.bat, which computes code for all possible pairs, storing directories in target\analysis,
      as target\analysis\src-%%a-%%e where %%a is the approach and %%e is the evolution.

       Main File:    org.combinators.ep.language.java.systemJ.DirectToDiskMainJ
       Evolutions:   M0 J1 J2 J3 K1 K2 J4 J5 J6 K2J6 J7 J8
       Approaches:   oo visitor visitorSideEffect extensibleVisitor interpreter dispatch trivially coco algebra

2. Produce full report

  `python3 compare.py [EVOLUTION-JSON]` where EVOLUTION-JSON is either "system-j.json" or "system-main.json".
  Note that the JSON files look like the following, so you can specify the location of the analysis
  directory.

         {
           "directory" : "analysis-J",
           "evolutions" : [
             { "J1" : ["M0"] },
             { "J2" : ["J1"] },
             ...
          }


  This script assesses whether an approach minimally satisfies the Expression Problem. That is, future evolutions
  do not require any changes to former artifacts.

  Note that it does not identify situations where, for example, non-trivial code is copied and used in a subsequent
  evolution. For example, the 'interpreter' approach requires code to be copies with producer operations.

     // Within original M4 evolution
     // ------------------------------
     public Exp simplify() {
            if ((Double.valueOf(((Exp) this.left).eval()).equals(0.0) || Double.valueOf(((Exp) this.right).eval()).equals(0.0))) {
                return new Lit(0.0);
            } else if (Double.valueOf(((Exp) this.left).eval()).equals(1.0)) {
                return ((Exp) this.right).simplify();
            } else if (Double.valueOf(((Exp) this.right).eval()).equals(1.0)) {
                return ((Exp) this.left).simplify();
            } else {
                return new ep.m4.Mult(((Exp) this.left).simplify(), ((Exp) this.right).simplify());
            }
        }

     // Within original M4 evolution
     // ------------------------------
     public Exp simplify() {
         if ((Double.valueOf(((Exp) this.left).eval()).equals(0.0) || Double.valueOf(((Exp) this.right).eval()).equals(0.0))) {
             return new Lit(0.0);
         } else if (Double.valueOf(((Exp) this.left).eval()).equals(1.0)) {
             return ((Exp) this.right).simplify();
         } else if (Double.valueOf(((Exp) this.right).eval()).equals(1.0)) {
             return ((Exp) this.left).simplify();
         } else {
             return new ep.m7i2.Mult(((Exp) this.left).simplify(), ((Exp) this.right).simplify());
         }
     }