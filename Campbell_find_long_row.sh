#!/bin/bash

FILES=$1
#FILES="*_5Min.dat"
#FILES="*_Hourly.dat"

for file in $FILES 
do
echo $file

sed '1,8d' $file | awk ' length > max { max=length;row=NR } END{ print row" "max}'

echo "===================="
continue
done
exit

