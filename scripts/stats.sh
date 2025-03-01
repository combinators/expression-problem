#!/bin/bash

# Execute this Bash script inside of a directory, like "ep-java-j" which contains
# All the code for a specific system, for each evolution, for each approach.
#
# Depends on CLOC line counting program: https://github.com/AlDanial/cloc
#
# Command line is either "java" or "scala". If no command line arg, default to java

if [ $# -eq 0 ]
then
   LANGUAGE=java
   TOP=ep
else
   LANGUAGE=$1
   TOP=mathdomain
fi

# location of CLOC
CLOC="../cloc-1.86.pl"

APPROACHES="algebra coco extensibleVisitor interpreter oo trivially visitor"

VERSIONS="M0 J1 J2 K1 K2 J3 J4 J5 J6 K2J6 J7 J8"

echo "approach,evolution,files,MAIN,#blank,#comment,#code,TEST,#blank,#comment,#code"
for a in $APPROACHES
do
   for v in $VERSIONS
   do
     FILES=`find $a/$v/src/main/$LANGUAGE/$TOP -name "*.$LANGUAGE"`
     TEST_FILES=`find $a/$v/src/test/$LANGUAGE/$TOP -name "*.$LANGUAGE"`

     OUT=`$CLOC --csv $FILES | grep SUM`
     TEST_OUT=`$CLOC --csv $TEST_FILES | grep SUM`

     echo $a,$v,$OUT,$TEST_OUT
   done
done
