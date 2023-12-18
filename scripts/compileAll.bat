@echo off
@REM run all tests

@REM compiler code to compute time
javac Time.java

@REM move into proper directory and create target
cd ..

mkdir target\make

for %%a in (oo visitor visitorSideEffect extensibleVisitor interpreter dispatch trivially coco algebra) do (

  @REM for each approach x model, execute to generate into target\ep2
  for %%e in (M0 J1 J2 J3 K1 K2 J4 J5 J6 K2J6 J7 J8) do (
     @echo off

     @REM: You will need to remove " from the generated file
     echo language-java/runMain org.combinators.ep.language.java.systemJ.DirectToDiskMainJ %%a %%e >> target\makeJ\commands.sbt
  )
)

echo language-java/runMain org.combinators.ep.language.java.DirectToDiskMain exit >> target\makeJ\commands.sbt
