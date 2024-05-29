@echo off
cd ..\..

@REM First run GenerateAllMain
@REM in Main which creates subdirectories in "target\ep-java"

@REM bring over the evolution specification for ease of use later
copy scripts\systems\system-j.json target\ep-java
set TAB=..

for %%a in (oo extensibleVisitor interpreter coco algebra visitor visitorSideEffect dispatch trivially) do (
  @echo off
  echo %%a

  @REM for each approach x model, execute to generate into target\%DIR%
  for %%e in (M0 M1 M2 M3 M4 M5 M6 M7 M7I2 M8 M9 I1 A1 A1M3 A1M3I2 A3 I2 O1 O2 OA O1OA OD1 OD2 OD3 OO1 OO2 OO3) do (
     echo %TAB% %%e
     call scripts\java-compile.bat %%a %%e ep-java
  )
)

@REM So this script can be used by others, return to start where launched
cd target\ep-java
