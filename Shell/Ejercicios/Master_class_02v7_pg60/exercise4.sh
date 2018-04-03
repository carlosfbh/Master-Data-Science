#! /bin/bash

#Ejercicio 4.

#Enunciado. Print content of ~/Data/shell/Text_example.txt except first 2 and last 3 lines.

cd ~/Data/shell
cat -n Text_example.txt | head -n -3 | tail -n -2


