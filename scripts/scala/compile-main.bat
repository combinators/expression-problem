@echo off
cd ..\..

@REM First run GenerateAll in Main which creates subdirectories in "target\ep-all"

@REM directory in target/ where analysis files will be generated
mkdir target\analysis

@REM bring over the evolution specification for ease of use later
copy scripts\systems\system-main.json target\analysis
set TAB=..

for %%a in (oo extensibleVisitor interpreter coco algebra visitor visitorSideEffect trivially) do (
  @echo off
  echo %%a

  @REM for each approach x model, execute to generate into target\%DIR%
  for %%e in (M0 M1 M2 M3 M4 M5 M6 M7 M7I2 M8 M9 I1 A1 A1M3 A1M3I2 A3 I2 O1 O2 OA O1OA OD1 OD2 OD3 OO1 OO2 OO3) do (
     echo %TAB% %%e
     call scripts\scala-compile.bat %%a %%e ep-all
  )
)
