#!/bin/bash

if [ "" == "$1" ]
then
    echo "no file specified..."
    exit 1
fi

IN_CODE=0

while IFS='' read -r line || [ -n "$line" ]
do
    if [ "\`\`\`" == "${line:0:3}" ]
    then
        IN_CODE=$(((IN_CODE + 1) % 2))

        continue
    fi

    if [ 1 == $IN_CODE ]
    then
        echo "$line"
    fi
done < "$1"
