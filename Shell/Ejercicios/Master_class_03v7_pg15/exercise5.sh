#! /bin/bash

#Ejercicio 5.
#Go to ~/Data/opentraveldata
#Use optd_airlines.csv to obtain the airline with the most flights?

cd ~/Data/opentraveldata
cat optd_airlines.csv | head -1 | tr "^" "\n" | wc -l
cat optd_airlines.csv | sort -t "^" -k14 -n -r | head -1

