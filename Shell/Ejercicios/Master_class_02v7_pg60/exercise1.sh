#! /bin/bash

#Ejercicio 1.

#Enunciado. Save the information of 3 largest files located inside your home directory into a three_largest_file.txt. (hint: use ls with sort option and pipe the result)

cd
ls -lSa | head -4 | tail -3 > three_largest_file.txt

