library(dplyr)

#########
# 1. Haz un merge con toda la tabla de airports, toda la tabla de 
#    flights y toda la tabla de airlines (todas de nycflights13).
# 
#    Teniendo en cuenta que:
#    1. No quiero perder ningún registro de flights
#    2. Se debe cruzar tanto origen como destino respecto a airports
#    3. Se debe cruzar con airlines para ver el nombre de la 
#       aerolíneas
#    4. Se quiere crear una nueva variable que sea la diferencia en
#       altitud del aeropuerto de origen frente al de destino
#    5. Se quiere crear otra variable que es la diferencia horaria
#       entre ambas zonas (la de origen y la de destino)
#
#   Una vez tengas este cruce:
#    a) Calcula el retraso total promedio agrupado por diferencia
#       horaria entre origen y destino

# Solución

# Primero abro la librería y me familiarizo con los datos de los 3 DF

library(nycflights13)
View(flights)
View(airlines)
View(airports)


# He hecho el ejercicio cogiendo primero una muestra de 5 vuelos.
# Una vez he visto que está bien he tenido que corregir cosas para los NA.

# example <- sample_n(flights, 5)

# Primero incluyo los datos de los aeropuertos de origen.
cruce <- merge(flights, airports, 
               by.x = "origin",
               by.y = "faa",
               all.x = TRUE)

# Segundo incluyo los datos de los aeropuertos de destino
cruce <- merge(cruce, airports,
               by.x = "dest",
               by.y = "faa",
               all.x = TRUE)

# Por último incluyo los datos de los nombres de las aerolíneas
cruce <- merge(cruce, airlines,
               by = "carrier",
               all.x = TRUE)

# Creo las dos nuevas variables que me pide el enunciado
cruce <- cruce %>% 
  mutate(alt_diff = abs(alt.x - alt.y)) %>% 
  mutate(tz_diff = abs(tz.x - tz.y))

# Filtro para que no coger los datos con NA, agrupo por la variable que quiero
# Calculo con el summarise los retrasos medios por salida y llegada
cruce %>% 
  filter(!is.na(tz_diff), !is.na(dep_delay), !is.na(arr_delay)) %>% 
  group_by(tz_diff) %>% 
  summarise(avg_delay_dep = mean(dep_delay),
            avg_delay_arr = mean(arr_delay))

