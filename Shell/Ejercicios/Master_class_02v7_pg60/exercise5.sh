#! /bin/bash

#Ejercicio 5.

#Enunciado. How many lines does ~/Data/opentraveldata/optd_aircraft.csv file have?

cd ~/Data/opentraveldata
wc -l optd_aircraft.csv

#-l para que nos saque el número de líneas
#-c para que nos saque el número de caracteres
#-w para que nos saque el número de palabras
