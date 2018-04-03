#! /bin/bash

# Ejercicio 5. Propuesto por el profesor
# Dado el número de motores, ¿cuántos aviones hay con esos motores?

cd ~/Data/opentraveldata

echo "Aviones con dos motores:"
cat optd_aircraft.csv | cut -d "^" -f 7 | grep 2 | wc -l

echo "Aviones con cuatro motores:"
cat optd_aircraft.csv | cut -d "^" -f 7 | grep 4 | wc -l

echo "Aviones con seis motores:"
cat optd_aircraft.csv | cut -d "^" -f 7 | grep 6 | wc -l


