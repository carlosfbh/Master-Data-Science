#! /bin/bash

#Ejercicio 6.
#Go to ~/Data/opentraveldata
#Use optd_airlines.csv to obtain number of airlines in each alliance?

#Para saber en qué columnas está la información de las alliance hago lo mismo que en el ejercicio 4 y filtro por alliance
cd ~/Data/opentraveldata
cat optd_airlines.csv | head -1 | tr "^" "\n" > ~/columns_file.txt
seq 1 `cat optd_airlines.csv | head -1 | tr "^" "\n" | wc -l` > ~/number_columns_file.txt

cd
paste number_columns_file.txt columns_file.txt | grep alliance

rm number_columns_file.txt
rm columns_file.txt


#Corto la columna que me interesa, ordeno y unifico duplicados
cd ~/Data/opentraveldata
cat optd_airlines.csv | cut -d "^" -f10 | sort -d | uniq -c

