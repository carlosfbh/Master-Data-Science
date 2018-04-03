#! /bin/bash

#Ejercicio 6.

#Enunciado. How many words do first 5 lines of the ~/Data/shell/Finn.txt have?

cd ~/Data/shell
cat Finn.txt | head -5 | wc -w
