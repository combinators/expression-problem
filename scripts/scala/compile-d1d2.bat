@echo off
cd ..\..

@REM First run GenerateAllD1D2 in Main which creates subdirectories in "target\ep-all-d1d2"

@REM bring over the evolution specification for ease of use later
copy scripts\systems\system-d1d2.json target\analysis
set TAB=..

for %%a in (oo extensibleVisitor interpreter coco algebra visitor visitorSideEffect trivially) do (
  @echo off
  echo %%a

  @REM for each approach x model, execute to generate into target\%DIR%
  for %%e in (M0 D1 D2 D1D2 D3) do (
     echo %TAB% %%e
     call scripts\scala-compile.bat %%a %%e ep-all-d1d2
  )
)
