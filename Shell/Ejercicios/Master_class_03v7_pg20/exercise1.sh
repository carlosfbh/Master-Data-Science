#! /bin/bash

#Go to ~/Data/opentraveldata

#Exercise1
#Use grep to extract all 7x7 or 3xx (where x can be any number) airplane models from optd_aircraft.csv.

cd ~/Data/opentraveldata
cat optd_aircraft.csv | cut -d "^" -f 3 | grep -n "7[0-9]7"
cat optd_aircraft.csv | cut -d "^" -f 3 | grep -n "3[0-9][0-9]"



