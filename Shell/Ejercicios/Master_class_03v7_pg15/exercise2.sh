#! /bin/bash

#Ejercicio 2.
#Go to ~/Data/opentraveldata
#Check if optd_por_public.csv has repeated white spaces (hint: use tr with wc)

cd ~/Data/opentraveldata
cat optd_por_public.csv | wc -l
cat optd_por_public.csv | tr "\n" "XX" | tr "  " "\n" | sort -r | uniq -c | tail -1


