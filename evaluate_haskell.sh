#!/bin/bash

# A working Glasgow Haskell Compiler must be installed
export GHC=ghc

# Needs Test (HUnit) and Data to work
export HUNIT=../../../../../language/haskell/src/main/resources/haskell-code/Test
export HDATA=../../../../../language/haskell/src/main/resources/haskell-code/Data

if [ ! -d target ]
then
  echo "No target directory"
  exit
fi

cd target

if [ ! -d ep-firstVersion ]
then
  echo "ep-firstVersion has not yet been generated"
  exit
fi

cd ep-firstVersion

if [ ! -d haskell ]
then
  echo "No Haskell EP approaches have been generated"
  exit
fi

# All output will appear in the haskell-report.txt
echo REPORT > haskell-report.txt

cd haskell

for d in `ls`
do
  echo $d >> ../haskell-report.txt
  echo $d
  cd $d
    for m in `ls`
    do
      echo $m >> ../../haskell-report.txt
      cd $m
         echo "compiling $d $m"
         echo "compiling $d $m" >> ../../../haskell-report.txt 2>&1

         if [ ! -d Data ]
         then
           cp -r $HDATA .
         fi
         if [ ! -d Test ]
         then
           cp -r $HUNIT .
         fi

         for tc in `ls Main*.hs`
         do
             $GHC $tc -o a.out >> ../../../haskell-report.txt
             if [ $? -eq 0 ]
             then
                 echo $d.${tc%.*} successful
                 echo $d.${tc%.*} successful >> ../../../haskell-report.txt
                 ./a.out >> ../../../haskell-report.txt 2>&1
                 if [ $? -eq 0 ]
                 then
                     echo ${tc%.*} success
                 else
                     echo ${tc%.*} failed
                     echo $d.${tc%.*} failed >> ../../../haskell-report.txt
                 fi

             else
                 echo "failure on compile $d.$m"
                 echo "failure on compile $d.$m" >> ../../../haskell-report.txt
             fi
         done
      cd ..
    done
  cd ..
done

echo "-------------"                >> ../haskell-report.txt
echo "FINALLY DONE"                 >> ../haskell-report.txt
echo "-------------"                >> ../haskell-report.txt

ls -l ../haskell-report.txt
