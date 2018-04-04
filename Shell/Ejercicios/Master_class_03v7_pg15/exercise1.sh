#! /bin/bash

#Ejercicio 1.
#Go to ~/Data/opentraveldata
#Change the delimiter of optd_aircraft.csv to “,”

cd ~/Data/opentraveldata
head optd_aircraft.csv | tr "^" ","
