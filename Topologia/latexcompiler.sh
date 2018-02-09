#!/bin/bash

archivo=sucesiones

if `test $# -ge 1` 
then
	archivo=$1
fi

if `! test -f $archivo.pdf`
then
	convert xc:none -page A4 $archivo.pdf
fi
	
emacs $archivo.tex &
evince $archivo.pdf &
latexmk -pvc -pdf $archivo.tex
