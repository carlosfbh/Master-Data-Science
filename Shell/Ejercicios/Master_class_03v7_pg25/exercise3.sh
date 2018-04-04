#! /bin/bash

#Use Text_example.txt

#Ejercicio 3. 
#Print ONLY the lines that DON’T contain the “line” word

cd ~/Data/shell
cat Text_example.txt
cat Text_example.txt | sed -n "/line/!p"
