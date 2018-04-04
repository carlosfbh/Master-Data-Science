#! /bin/bash

#Ejercicio 3.
#Go to ~/Data/opentraveldata
#How many columns has optd_por_public.csv? (hint: use head and tr)

cd ~/Data/opentraveldata
cat optd_por_public.csv | head -1 | tr "^" "\n" | wc -l
