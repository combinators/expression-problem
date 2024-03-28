@echo off
@REM run all tests

@REM compiler code to compute time
javac Time.java

@REM move into proper directory and create target
cd ..

@REM directory in target/ where generated code was placed...
set DIR=ep2

@REM directory in target/ where analysis files will be generated
mkdir target\analysis

@REM bring over the evolution specification for ease of use later
copy scripts\system-d1d2.json target\analysis

for %%a in (oo extensibleVisitor interpreter trivially coco algebra visitor visitorSideEffect dispatch) do (
  @echo off
  echo %%a
  echo %%a > target\analysis\jacoco.%%a

  @REM for each approach x model, execute to generate into target\ep2
  for %%e in (M0 M1 D1 D2 D1D2 D3) do (
     @echo off
     echo ====================================== >> target\analysis\jacoco.%%a
     echo %%e-Generate                           >> target\analysis\jacoco.%%a
     java -cp scripts Time                       >> target\analysis\jacoco.%%a
     echo ====================================== >> target\analysis\jacoco.%%a
     @REM: KEEP? set JAVA_HOME=%SAVED_JAVA_HOME%

     sbt "language-java/runMain org.combinators.ep.language.java.DirectToDiskMainD1D2 %%a %%e"

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
     sbt jacoco            >> ..\analysis\jacoco.%%a

     echo ====================================== >> ..\analysis\jacoco.%%a
     echo %%e-Test-Begin                         >> ..\analysis\jacoco.%%a
     java -cp ..\..\scripts Time                 >> ..\analysis\jacoco.%%a
     echo ====================================== >> ..\analysis\jacoco.%%a

     sbt jacoco            >> ..\analysis\jacoco.%%a
     echo                  >> ..\analysis\jacoco.%%a

     echo ====================================== >> ..\analysis\jacoco.%%a
     echo %%e-Test-End                           >> ..\analysis\jacoco.%%a
     java -cp ..\..\scripts Time                 >> ..\analysis\jacoco.%%a
     echo ====================================== >> ..\analysis\jacoco.%%a

     @REM back up to main
     cd ..\..
  )
)