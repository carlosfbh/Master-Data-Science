#! /bin/bash

# Ejercicio 4.
# Go to ~/Data/opentraveldata Get the line with the highest number of engines from optd_aircraft.csv by using sort.

cd ~/Data/opentraveldata
cat optd_aircraft.csv | sort -t "^" -k 7 -n -r | head -1
