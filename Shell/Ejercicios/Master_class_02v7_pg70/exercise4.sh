#! /bin/bash

# Ejercicio 4.
# Enunciado. Get top 3 largest files per subdirectory inside ~/Data/

find ~/Data/ -maxdepth 1 -mindepth 1 -type d -exec echo Directorio: {} \; -exec sh -c "ls -lSa {} | head -4 | tail -3" \;
