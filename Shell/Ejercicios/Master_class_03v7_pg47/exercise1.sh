#! /bin/bash

#1. 
#Use csvstat to find out how many different manufactures are in the file

cd ~/Data/opentraveldata/
csvstat -d "^" optd_aircraft.csv | less -S
#Hay 76
