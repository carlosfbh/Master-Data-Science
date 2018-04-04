#! /bin/bash

#Use Text_example.txt

#Ejercicio 2. 
#Delete lines that contain the “line” word.

cd ~/Data/shell
cat Text_example.txt | sed "/line/d"
