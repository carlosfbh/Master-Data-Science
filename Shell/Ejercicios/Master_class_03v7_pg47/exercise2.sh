#! /bin/bash

#2. 
#Extract the column manufacturer and then by using pipes, sort, uniq and wc find out how many manufacturers are in the file. Why does this number #differ to the number reported in csvstat?

cd ~/Data/opentraveldata/

cat optd_aircraft.csv | csvcut -d "^" -c "manufacturer" | csvsort | uniq | wc -l
#Salen 77, porque está contando la cabecera, habría que quitarle la primera fila

cat optd_aircraft.csv | csvcut -d "^" -c "manufacturer" | csvsort | uniq | tail -n+2 | wc -l
#Así salen 76
