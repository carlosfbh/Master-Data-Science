#! /bin/bash

#Ejercicio 4.
#Go to ~/Data/opentraveldata
#Print column names of optd_por_public.csv together with their columnnumber. (hint: use paste)

cd ~/Data/opentraveldata
cat optd_por_public.csv | head -1 | tr "^" "\n" > ~/columns_file.txt
seq 1 `cat optd_por_public.csv | head -1 | tr "^" "\n" | wc -l` > ~/number_columns_file.txt

cd
paste number_columns_file.txt columns_file.txt

rm number_columns_file.txt
rm columns_file.txt
