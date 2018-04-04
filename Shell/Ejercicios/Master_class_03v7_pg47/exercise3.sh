#! /bin/bash


#3. 
#What are the top 5 manufacturers?

cd ~/Data/opentraveldata/
cat optd_aircraft.csv | csvcut -d "^" -c "manufacturer" | csvsort | uniq -c | csvsort -r | tail -n+2 | head -5
