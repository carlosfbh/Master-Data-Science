#! /bin/bash

#3. 
#Compress “optd_por_public.csv” with bzip2 and then extract from the compressed file all the lines starting with MAD (hint: use bzcat and grep)

cd ~/Data/opentraveldata
bzip2 optd_por_public.csv
bzcat optd_por_public.csv.bz2 | grep "^MAD"
bzip2 -d optd_por_public.csv.bz2

