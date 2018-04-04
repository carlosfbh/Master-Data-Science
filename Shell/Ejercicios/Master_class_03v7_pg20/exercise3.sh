#! /bin/bash

#Go to ~/Data/opentraveldata

#Exercise3
#How many optd_por_public.csv columns have “name” as part of their name? What are their numerical positions? (hint: use seq and paste)

cd ~/Data/opentraveldata
cat optd_por_public.csv | head -1 | tr "^" "\n" | grep -n "name"

