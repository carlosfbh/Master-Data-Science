#! /bin/bash

#Ejercicio 2.

#Enunciado. Save last 20 commands used at command line to a file. (hint use history and redirect the output)

cat -n ~/.history | tail -20 > ~/history_20last_commands.txt


