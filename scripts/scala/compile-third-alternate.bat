@echo off
cd ..\..

@REM First run GenerateAllThirdAlternate in Main which creates subdirectories in "target\ep-all-third-alternate"

@REM directory in target/ where analysis files will be generated
mkdir target\analysis

@REM bring over the evolution specification for ease of use later
copy scripts\systems\system-third-alternate.json target\analysis
set TAB=..

for %%a in (oo extensibleVisitor interpreter coco algebra visitor visitorSideEffect trivially) do (
  @echo off
  echo %%a

  @REM for each approach x model, execute to generate into target\%DIR%
  for %%e in (M0 X1 X2 X3 X2X3 X4) do (
     echo %TAB% %%e
     call scripts\scala-compile.bat %%a %%e ep-all-third-alternate
  )
)
