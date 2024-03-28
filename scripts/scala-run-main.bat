@echo off
@REM Generate All Scala Code. NOTE: This might not work as standalone script depending on sbt version

@REM compiler code to compute time
javac Time.java

@REM move into proper directory and create target
cd ..

@REM directory in target/ where generated code is placed...
set DIR=ep2

@REM directory in target/ where analysis files will be generated
mkdir target\analysis

@REM bring over the evolution specification for ease of use later
copy scripts\system-main.json target\analysis

for %%a in (oo extensibleVisitor interpreter trivially coco algebra visitor visitorSideEffect) do (
  @echo off
  echo %%a
  echo %%a > target\analysis\jacoco.%%a

  @REM for each approach x model, execute to generate into target\%DIR%
  for %%e in (M0 M1 M2 M3 M4 M5 M6 M7 M7I2 M8 M9 I1 A1 A1M3 A1M3I2 A3 I2 O1 O2 OA O1OA OD1 OD2 OD3 OO1 OO2 OO3) do (
     @echo off
     echo ====================================== >> target\analysis\jacoco.%%a
     echo %%e-Generate                           >> target\analysis\jacoco.%%a
     java -cp scripts Time                       >> target\analysis\jacoco.%%a
     echo ====================================== >> target\analysis\jacoco.%%a
     @REM: KEEP? set JAVA_HOME=%SAVED_JAVA_HOME%

     call sbt "language-newScala/runMain org.combinators.ep.language.scala.codegen.DirectToDiskMain %%a %%e"

     @REM generated into target\%DIR%
     cd target
     cd %DIR%
     xcopy src ..\analysis\src-%%a-%%e /E/H/C/I

     @REM No test cases yet. Just compile
     echo ====================================== >> ..\analysis\jacoco.%%a
     echo %%e-Compile-Begin                      >> ..\analysis\jacoco.%%a
     java -cp ..\..\scripts Time                 >> ..\analysis\jacoco.%%a
     echo ====================================== >> ..\analysis\jacoco.%%a
     call sbt compile       >> ..\analysis\jacoco.%%a

     @REM back up to main
     cd ..\..
  )
)
