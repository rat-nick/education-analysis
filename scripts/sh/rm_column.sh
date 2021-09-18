#!bin/bash

for FILE in $(ls $1)
do
    cut -f2- $FILE > "cens$FILE"
done
