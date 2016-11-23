#!/bin/bash

if [ "" == "$1" ]
then
    echo "no file specified..."
    exit 1
fi

pdflatex -shell-escape $1
