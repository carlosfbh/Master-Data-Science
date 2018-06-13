##### EJERCICIOS VISUALIZACIÓN ggmap ######

library(tidyverse)
library(ggmap)
require(raster)
require(plotly)
require(tmap)

##########################################################################
# 1. Exercise: 
# Dibujar un punto en un mapa con nombre
##########################################################################

casa_zizur <- geocode("Calle Lakuntzea 7, Zizur Mayor, Spain")
mapa <- get_map(casa_zizur, zoom = 16)
mapa2 <- get_map(casa_zizur, zoom = 16, source = "stamen", maptype = "watercolor")

ggmap(mapa) + 
  geom_point(data = casa_zizur, aes(x = lon, y = lat), size = 3, col = "red3") +
  geom_text(data = casa_zizur, aes(x = lon, y = lat), label = "Casa", size = 3)

ggmap(mapa2) + 
  geom_point(data = casa_zizur, aes(x = lon, y = lat), size = 3, col = "red3") +
  geom_text(data = casa_zizur, aes(x = lon, y = lat), label = "Casa", size = 3)



##########################################################################
# 2. Exercise: 
# Dibujar en un mapa las terrazas del barrio donde está Kschool
# Utilizar la base de datos de las terrazas del csv
##########################################################################

terrazas <- read.csv("data/terrazas.csv")

kschool <- geocode("Calle Magallanes 1, Madrid, Spain")
mapa <- get_map(kschool, zoom = 16)

ggmap(mapa) + 
  geom_point(data = terrazas, aes(x = lon, y = lat), 
             size = 1.5, alpha = 0.75, col = "orange3")


##########################################################################
# 3. Exercise: 
# Mismo ejercicio que antes pero con gasolineras
# Utilizar la base de datos de carburantes del csv
##########################################################################

gasolineras <- read.csv("data/carburantes.csv", sep = ";", encoding = "UTF-8", dec = ",")

# Separador por ;
# Decimales separados por ,

# Voy a ver las provincias con menor precio de media en Gasoleo.A
p <- gasolineras %>% 
  filter(Precio.Gasoleo.A != "", !is.na(Precio.Gasoleo.A)) %>% 
  group_by(Provincia) %>% 
  summarise(avg_precio = mean(Precio.Gasoleo.A)) %>% 
  mutate(rango = rank(avg_precio)) %>% 
  arrange(rango)


# Voy a dibujar las gasolineras de Madrid teniendo en cuenta solo el Precio.Gasoleo.A
gasolineras_madrid <- gasolineras %>%
  filter(Provincia == "MADRID", Precio.Gasoleo.A != "", !is.na(Precio.Gasoleo.A)) %>% 
  select(lon = Longitud..WGS84., lat = Latitud..WGS84., Precio.Gasoleo.A)

madrid <- geocode("madrid")
mapa <- get_map(madrid)
ggmap(mapa) + 
  geom_point(data = gasolineras_madrid, aes(x = lon, y = lat, color = Precio.Gasoleo.A),
             alpha = 0.8, size = 1.5)


# Ahora voy a dibujar las que tengan un precio menor a 1.10
gasolineras_madrid <- gasolineras %>%
  filter(Provincia == "MADRID", Precio.Gasoleo.A != "", !is.na(Precio.Gasoleo.A)) %>% 
  filter(Precio.Gasoleo.A < 1.10) %>% 
  select(lon = Longitud..WGS84., lat = Latitud..WGS84., Precio.Gasoleo.A)

ggmap(mapa) + 
  geom_point(data = gasolineras_madrid, aes(x = lon, y = lat),
             alpha = 0.8, size = 2)


##########################################################################
# 4. Exercise: 
# Dibujar las gasolineras en Madrid: Gasoleos y Gasolinas
# Que cada tipo de carburante tenga su gráfico
# Utilizar el color para ver la diferencia de precios
# Guardado en pdf
##########################################################################

gasolineras <- read.csv("data/carburantes.csv", sep = ";", encoding = "UTF-8", dec = ",")

# Primero filtro los datos y selecciono las variables que quiero

gasolineras_madrid <- gasolineras %>% 
  select(lon = Longitud..WGS84., lat = Latitud..WGS84.,
         Provincia,
         Precio.Gasolina.95.Protección,
         Precio.Gasolina..98,
         Precio.Gasoleo.A,
         Precio.Nuevo.Gasoleo.A) %>% 
  filter(Provincia == "MADRID", !is.na(Precio.Gasoleo.A),
         !is.na(Precio.Nuevo.Gasoleo.A), !is.na(Precio.Gasolina..98),
         !is.na(Precio.Gasolina.95.Protección))

# Ahora transformo mi DF poniendo los tipos de carburantes en formato fila
gasolineras_madrid <- gather(gasolineras_madrid, carburante, precio, -lon, -lat, -Provincia)


madrid <- geocode("madrid")
mapa <- get_map(madrid)

ggmap(mapa) +
  geom_point(data = gasolineras_madrid, aes(x = lon, y = lat, color = precio),
             size = 1.5, alpha = 0.8) +
  ggtitle("Precios Carburantes en Madrid") +
  facet_wrap( ~ carburante) + 
  theme_bw()

ggsave("Carburantes_Madrid.pdf", width = 40, height = 20, units = "cm")



##########################################################################
# 5. Exercise: 
# Utiliza el conjunto de datos Crime que viene con el paquete ggmap
# Haz un gráfico en el mapa de Houston solo teniendo en cuenta los asaltos graves
# Un gráfico por tipo de asalto y un gráfico por día de la semana
##########################################################################

houston_crime <- crime %>% 
  filter(!offense %in% c("aggravated assault", "burglary", "theft"))

houston <- geocode("houston")
mapa <- get_map(houston, zoom = 14)

ggmap(mapa) + 
  geom_point(data = houston_crime, aes(x = lon, y =lat), 
             size = 1, alpha = 0.6, color = "red3") +
  facet_wrap( ~ offense)

ggmap(mapa) + 
  geom_point(data = houston_crime, aes(x = lon, y = lat), 
             size = 1, alpha = 0.6, color = "red3") +
  facet_wrap( ~ day)


##########################################################################
# 6. Exercise: 
# Utiliza el conjunto de datos Crime que viene con el paquete ggmap
# Haz un gráfico en el mapa de Houston solo teniendo en cuenta los asaltos graves
# Un gráfico por tipo de asalto y un gráfico en formato de densidades
##########################################################################

houston_crime <- crime %>% 
  filter(!offense %in% c("aggravated assault", "burglary", "theft"))

houston <- geocode("houston")
mapa <- get_map(houston, zoom = 14)

ggmap(mapa) + 
  stat_density2d(data = houston_crime, aes(x = lon, y = lat, alpha = ..level..),
                 fill = "red4", size = 2, geom = "polygon") + 
  facet_wrap( ~ offense)



##########################################################################
# 7. Exercise: 
# Utilizando la base de datos del paro del csv:
# Hacer un gráfico de la tasa del paro según sexo y provincia en un mapa
# Se debe diferenciar la tasa del paro según el color (solo la península)
# Coger el primer trimestre del 2011
# Guardado en pdf
##########################################################################

paro <- read.csv("data/paro.csv", encoding = "UTF-8")

# Cargo el mapa de España y filtro para quedarme la península
shape <- getData("GADM", country= "Spain", level = 2)
peninsula <- subset(shape, !NAME_1 == "Islas Canarias")


# Transformo la variable peninsula en data frame y dibujo la península
peninsula.df <- fortify(peninsula, region="CCA_2")

ggplot(peninsula.df) + 
  aes(x = long, y = lat, group = group) +
  geom_polygon(fill = "grey60", colour = "grey80", size = 0.1) +
  coord_quickmap()


paro_I_2011 <- paro %>%
  filter(Año == 2011)

# Ahora hago un merge para unificar las dos base de datos
# Tengo que transformar la columna id porque está guardada como texto
peninsula.df <- peninsula.df %>% 
  mutate(id = as.numeric(id))

peninsula_paro = inner_join(peninsula.df, paro_I_2011, by = c("id" = "Prov.id"))

ggplot(peninsula_paro) + 
  aes(x = long, y = lat, group = group, fill = Tasa.paro) + 
  geom_polygon(colour = "grey80", size = 0.1) + 
  facet_grid(Sexo ~ Trimestre) + 
  ggtitle("Tasa del paro 2011 por Trimestre") +
  scale_fill_gradient(low = "aliceblue", high = "steelblue4") +
  coord_quickmap()

ggsave("Tasa_paro_2011.pdf", width = 40, height = 20, units = "cm")


# Voy a dibujar el mismo mapa sobre un lienzo utilizando get_map
centro <- geocode("madrid, Spain")
mapa <- get_map(centro, zoom = 6, maptype = "toner-lite", source = "stamen")

ggmap(mapa) +
  geom_polygon(data = peninsula_paro, aes(x = long, y = lat, group = group, fill = Tasa.paro),
               alpha = 0.8) +
  facet_grid(~ Sexo) + 
  ggtitle("Tasa del paro 2011 por Trimestre") +
  scale_fill_gradient(low = "aliceblue", high = "steelblue4")
