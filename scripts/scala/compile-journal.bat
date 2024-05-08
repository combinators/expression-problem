@echo off
cd ..\..

@REM First run GenerateAllJ in Main which creates subdirectories in "target\ep-scala-journal"

@REM bring over the evolution specification for ease of use later
copy scripts\systems\system-journal.json target\ep-scala-journal
set TAB=..

for %%a in (oo extensibleVisitor interpreter coco algebra visitor visitorSideEffect trivially) do (
  @echo off
  echo %%a

  @REM for each approach x model, execute to generate into target\%DIR%
  for %%e in (M0 M1 M2 I1 I2 N1 M2_ABS M3 M3I1 I2M3I1N1) do (
     echo %TAB% %%e
     call scripts\scala-compile.bat %%a %%e ep-scala-journal
  )
)

@REM So this script can be used by others, return to start where launched
cd target\ep-scala-journal