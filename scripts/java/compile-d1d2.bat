@echo off
cd ..\..

@REM First run GenerateAllD1D2 in Main which creates subdirectories in "target\ep-java-d1d2"

@REM bring over the evolution specification for ease of use later
copy scripts\systems\system-d1d2.json target\ep-java-d1d2
set TAB=..

for %%a in (oo extensibleVisitor interpreter coco algebra visitor visitorSideEffect dispatch trivially) do (
  @echo off
  echo %%a

  @REM for each approach x model, execute to generate into target\%DIR%
  for %%e in (M0 D1 D2 D1D2 D3) do (
     echo %TAB% %%e
     call scripts\java-compile.bat %%a %%e ep-java-d1d2
  )
)

@REM So this script can be used by others, return to start where launched
cd target\ep-java-d1d2
