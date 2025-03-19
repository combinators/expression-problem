#!/bin/bash

# Investigate concerns -- domain (D), language (L), and approach (A)

# get CSV of results FILES,BLANK,COMMENT,CODE
CLOC=../cloc-1.86.pl"

# all DD/DI
DI=`perl $CLOC ../core ../domain ../language --exclude-content="DD:L[DI]:A[DI]" --force-lang=scala --csv | grep -v SUM | tail -1`
DD=`perl $CLOC ../core ../domain ../language --exclude-content="DI:L[DI]:A[DI]" --force-lang=scala --csv | grep -v SUM | tail -1`

# all LD/LI
LI=`perl $CLOC ../core ../domain ../language --exclude-content="D[DI]:LD:A[DI]" --force-lang=scala --csv | grep -v SUM | tail -1`
LD=`perl $CLOC ../core ../domain ../language --exclude-content="D[DI]:LI:A[DI]" --force-lang=scala --csv | grep -v SUM | tail -1`

# all AD/AI
AI=`perl $CLOC ../core ../domain ../language --exclude-content="D[DI]:L[DI]:AD" --force-lang=scala --csv | grep -v SUM | tail -1`
AD=`perl $CLOC ../core ../domain ../language --exclude-content="D[DI]:L[DI]:AI" --force-lang=scala --csv | grep -v SUM | tail -1`

echo "DI,$DI"
echo "DD,$DD"

echo "LI,$LI"
echo "LD,$LD"

echo "AI,$AI"
echo "AD,$AD"
echo

echo "DD:LD:AD"
perl $CLOC `find ../ -name "*.scala" -exec grep "DD:LD:AD" {} \; -print | grep "^\.\."` --by-file



echo "DI:LD:AD"
perl $CLOC `find ../ -name "*.scala" -exec grep "DI:LD:AD" {} \; -print | grep "^\.\."` --by-file

echo "DD:LI:AD"
perl $CLOC `find ../ -name "*.scala" -exec grep "DD:LI:AD" {} \; -print | grep "^\.\."` --by-file

echo "DD:LI:AI"
perl $CLOC `find ../ -name "*.scala" -exec grep "DD:LD:AI" {} \; -print | grep "^\.\."` --by-file



echo "DD:LI:AI"
perl $CLOC `find ../ -name "*.scala" -exec grep "DD:LI:AI" {} \; -print | grep "^\.\."` --by-file

echo "DI:LD:AI"
perl $CLOC `find ../ -name "*.scala" -exec grep "DI:LD:AI" {} \; -print | grep "^\.\."` --by-file

echo "DI:LI:AD"
perl $CLOC `find ../ -name "*.scala" -exec grep "DI:LI:AD" {} \; -print | grep "^\.\."` --by-file
