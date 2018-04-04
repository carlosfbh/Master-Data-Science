#! /bin/bash


#4. Using csvgrep, get only the records with manufacturer equal to Airbus and save them to a file with pipe (|) delimiter.

cat optd_aircraft.csv | csvgrep -d "^" -c "manufacturer" -m "Airbus" > Airbus_data.csv
cat Airbus_data.csv | csvlook | less -S
