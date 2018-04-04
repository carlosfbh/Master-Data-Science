#! /bin/bash

#Go to ~/Data/opentraveldata

#Exercise2
#Use grep to obtain the number of airlines with prefix “aero” in their name from optd_airlines.csv

cd ~/Data/opentraveldata
cat optd_airlines.csv | cut -d "^" -f 8 | grep -i "^aero" | wc -l


