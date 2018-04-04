#! /bin/bash

#Use Text_example.txt

#Ejercicio 1. 
#Replace every “line” with new line character (“\n”)

cd ~/Data/shell
cat Text_example.txt | sed "s/line/\n/g"
