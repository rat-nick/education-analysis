#!/bin/bash
IFS=''
mkdir $1
for FILE in *$1*;
do 
    #echo $FILE
    readarray -d " " -t strarr <<< "$FILE"
    echo ${strarr[0]}-${strarr[1]}
    xlsx2csv -s  $FILE > $1/${strarr[0]}-${strarr[1]}.csv   
done