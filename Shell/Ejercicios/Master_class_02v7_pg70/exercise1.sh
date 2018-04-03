#! /bin/bash

# Ejercicio 1.
# Enunciado. Find all files located ONLY inside subdirectories of your home directory which have been modified in last 60min


find ~/ -type f -mmin -60
