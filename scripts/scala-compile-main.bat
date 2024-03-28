@echo off
@REM ASSUMES GenerateAll has already been run, with output in ep-all

@REM compiler code to compute time
javac Time.java

@REM move into proper directory and create target
cd ..

@REM directory in target/ where generated code is placed...
set DIR=ep-all
set TAB=..

@REM directory in target/ where analysis files will be generated
mkdir target\analysis

@REM bring over the evolution specification for ease of use later
copy scripts\system-main.json target\analysis

for %%a in (oo extensibleVisitor interpreter coco algebra visitor visitorSideEffect trivially) do (
  @echo off
  echo %%a
  echo %%a > target\analysis\jacoco.%%a

  @REM for each approach x model, execute to generate into target\%DIR%
  for %%e in (M0 M1 M2 M3 M4 M5 M6 M7 M7I2 M8 M9 I1 A1 A1M3 A1M3I2 A3 I2 O1 O2 OA O1OA OD1 OD2 OD3 OO1 OO2 OO3) do (

     @REM generated into target\%DIR%
     cd target
     cd %DIR%

     @REM No test cases yet. Just compile
     cd %%a
     cd %%e
     echo %TAB% %%e

     echo ====================================== >> ..\..\..\analysis\jacoco.%%a
     echo %%e-Compile-Begin                      >> ..\..\..\analysis\jacoco.%%a
     java -cp ..\..\..\..\scripts Time           >> ..\..\..\analysis\jacoco.%%a
     echo ====================================== >> ..\..\..\analysis\jacoco.%%a

     call sbt compile       >> ..\..\..\analysis\jacoco.%%a

     @REM back up to main
     cd ..\..\..\..
  )
)
