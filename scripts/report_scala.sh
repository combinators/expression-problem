#!/bin/bash

# Run this script in the directory that contains the generated code for different approaches.

# Update as needed
# get CSV of results FILES,BLANK,COMMENT,CODE
CLOC=cloc-1.86.pl

# Choose desired stage
STAGE="J8"

echo Approach,Trait,Class,AbsClass,Def,Override
for a in `ls -d oo algebra coco extensibleVisitor interpreter trivially visitor`
do
    NUM_TRAIT=`find $a/$STAGE/src/main/scala/mathdomain -name "*.scala" -exec grep "trait\(\s*\)[A-Za-z]" {} \; | wc -l`

    NUM_CLASS=`find $a/$STAGE/src/main/scala/mathdomain -name "*.scala" -exec grep "class\(\s*\)[A-Za-z]" {} \; | grep -v "case\(\s*\)class" | grep -v "abstract
\(\s*\)class" | wc -l`

    NUM_ABS=`find $a/$STAGE/src/main/scala/mathdomain -name "*.scala" -exec grep "abstract\(\s*\)class" {} \; | wc -l`

    NUM_OVERRIDE=`find $a/$STAGE/src/main/scala/mathdomain -name "*.scala" -exec grep "override\(\s*\)def" {} \; | wc -l`

    NUM_DEF=`find $a/$STAGE/src/main/scala/mathdomain -name "*.scala" -exec grep "def\(\s*\)[A-Za-z]" {} \;|wc -l`

    NUM_DEF=$((NUM_DEF - NUM_OVERRIDE))

    echo $a,$NUM_TRAIT,$NUM_CLASS,$NUM_ABS,$NUM_DEF,$NUM_OVERRIDE
done
