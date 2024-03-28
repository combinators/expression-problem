@echo off
@REM run all tests

@REM compiler code to compute time
javac Time.java

@REM move into proper directory and create target
cd ..

@REM directory in target/ where generated code was placed...
set DIR=ep3

@REM directory in target/ where analysis files will be generated
mkdir target\analysis

@REM bring over the evolution specification for ease of use later
copy scripts\system-j.json target\analysis

for %%a in (oo extensibleVisitor interpreter trivially coco algebra visitor visitorSideEffect dispatch) do (
  @echo off
  echo %%a
  echo %%a > target\analysis\jacoco.%%a

  @REM for each approach x model, execute to generate into target\ep3
  for %%e in (M0 J1 J2 J3 K1 K2 J4 J5 J6 K2J6 J7 J8) do (
     @echo off
     echo ====================================== >> target\analysis\jacoco.%%a
     echo %%e-Generate                           >> target\analysis\jacoco.%%a
     java -cp scripts Time                       >> target\analysis\jacoco.%%a
     echo ====================================== >> target\analysis\jacoco.%%a
     @REM: KEEP? set JAVA_HOME=%SAVED_JAVA_HOME%

     call sbt "language-java/runMain org.combinators.ep.language.java.systemJ.DirectToDiskMainJ %%a %%e"

     @REM generated into target\%DIR%
     cd target
     cd %DIR%
     xcopy src ..\analysis\src-%%a-%%e /E/H/C/I

     @REM: KEEP? set JAVA_HOME=C:\Users\heineman\Development\jdk-11.0.21+9

     @REM run Jacoco twice: the first time compiles. The second time only instruments
     @REM doesn't seem to be any way to avoid instrumentation in the second pass, but
     @REM at least this doesn't conflate initial compilation time
     echo ====================================== >> ..\analysis\jacoco.%%a
     echo %%e-Compile-Begin                      >> ..\analysis\jacoco.%%a
     java -cp ..\..\scripts Time                 >> ..\analysis\jacoco.%%a
     echo ====================================== >> ..\analysis\jacoco.%%a
     call sbt jacoco       >> ..\analysis\jacoco.%%a

     echo ====================================== >> ..\analysis\jacoco.%%a
     echo %%e-Test-Begin                         >> ..\analysis\jacoco.%%a
     java -cp ..\..\scripts Time                 >> ..\analysis\jacoco.%%a
     echo ====================================== >> ..\analysis\jacoco.%%a

     call sbt jacoco       >> ..\analysis\jacoco.%%a
     echo                  >> ..\analysis\jacoco.%%a

     echo ====================================== >> ..\analysis\jacoco.%%a
     echo %%e-Test-End                           >> ..\analysis\jacoco.%%a
     java -cp ..\..\scripts Time                 >> ..\analysis\jacoco.%%a
     echo ====================================== >> ..\analysis\jacoco.%%a

     @REM back up to main
     cd ..\..
  )
)