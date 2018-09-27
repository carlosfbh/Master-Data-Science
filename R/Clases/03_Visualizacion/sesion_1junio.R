## Parte 2.4 de los apuntes

install.packages("maptools")
install.packages("rgdal")
install.packages("gpclib", type="source")
install.packages('rgeos')
install.packages("broom")


library(ggmap)
library(tidyverse)
library(ggplot2)
library(broom)
library(rgdal)
library(maptools)
library(rgeos)

if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()


kschool <- geocode("Calle Magallanes 1, Madrid, Spain")
mapa <- get_map(kschool, zoom = 16)
ggmap(mapa)

mapa2 <- get_map(location = kschool,zoom = 16, source = "stamen", maptype = "toner")
ggmap(mapa)

# Si queremos dibujar el nombre de Kschool en su ubicación utilizamos geom_text
ggmap(mapa2) + geom_text(x = -3.705416, y = 40.43242, label = "kschool", col = "red3")

# Otra manera de hacerlo
ggmap(mapa) + geom_text(data = kschool, aes(x=lon, y=lat), label = "kschool", col = "red3")

# Si queremos añadir un punto 
ggmap(mapa) + geom_point(data = kschool, aes(x=lon, y=lat), size = 3, col = "red3")


## Vamos a hacer un ejercicio para visualizar las terrazas en el barrio
terrazas <- read_csv("data/terrazas.csv") # Cargamos la ubicación de las terrazas

p <- ggmap(mapa) + geom_text(data = kschool, aes(x=lon, y=lat), label = "kschool", col = "red3")

# Sobre el gráfico de antes (guardado como p) donde sale Kschool vamos a representar las terrazas
p + geom_point(data = terrazas, aes(x=lon, y=lat), col = "green3", alpha = 0.8)


## Vamos a representar las gasolineras
gasolineras <- read_csv2("data/carburantes.csv")
p + geom_point(data = gasolineras, aes(x=lon, y=lat), col = "green3", alpha = 0.8)
# Da error. Hacer el ejercicio en casa


## Vamos a hacer ejercicios sobre los crímenes en Houston.
# Utilizamos el conjunto de datos crime que está dentro de ggmap

View(crime)

# Nos quedamos con los crímenes más agresivos
crimes.houston <- crime %>%
  filter(!crime$offense %in% c("auto theft", "theft", "burglary"))

# Cargamos el mapa de Houston
houston <- c(lon=-95.3698,lat= 29.76043)
HoustonMap <- get_map(houston, zoom = 14, color = "bw")
ggmap(HoustonMap)

ggmap(HoustonMap) + 
  geom_point(aes(x = lon, y = lat, colour = offense), data = crimes.houston, size = 1)

# Como hay muchos puntos dividimos el mapa en 4 trozos en función de los datos y color
ggmap(HoustonMap) + 
  geom_point(aes(x = lon, y = lat, colour = offense), data = crimes.houston) + 
  facet_wrap(~offense)

# Mejoramos
ggmap(HoustonMap) + 
  geom_point(aes(x = lon, y = lat), data = crimes.houston, size = 1,
             alpha = 0.2, col = "red3") + 
  facet_wrap(~offense)


# Es buena idea colorear según estadística, poner densidades.
# Ver punto 2.4.2


# Hay datos que no se pueden proporcionar por temas de privacidad o confidencialidad
# Por ejemplo, datos de cáncer, te los dan por zona.

# Cargamos los datos del paro
paro <- read.csv("data/paro.csv")
unique(paro$Prov.id)
# Los datos vienen por provincia


# Vamos a dibujar el mapa de españa. Con level vamos accediendo. si es 1 son Com. Aut.
install.packages("raster")
require(raster)
shape <- getData("GADM", country= "Spain", level = 2) #mapa administrativo a nivel provincial
load(file="data/provincias.RDA")
summary(shape)
plot(shape,col="lightblue")


# Si queremos ver los datos asociados a provincias
shape@data # Esto para ver el ID_1 que corresponde canarias
canarias = subset(shape, ID_1 == 14)
plot(canarias)

# Vamos a crear una columna más al archivo de paro y representar por colores por provincia
# Para mejorar la representación nos cargamos las islas canarias
peninsula <- subset(shape,!NAME_1=="Islas Canarias")
peninsula

plot(peninsula, col="lightblue")
text(coordinates(peninsula), labels=peninsula$NAME_2,cex=.5)

# Si queremos convertir nuestra variable península en DataFrame es mejor pa trabajar
# Daba error y hemos tenido que instalar muchas librerias
peninsula.df=fortify(peninsula,region="CCA_2") #convierte el shape en data.frames

ggplot() + geom_polygon(data = peninsula.df, aes(long, lat, group = group), 
                        fill="grey60",colour = "grey80", size = .1)+coord_quickmap()


# Ahora pintamos los datos del paro

Paro <- paro %>% 
  filter(AÃ.o==2011 & Trimestre=="I")

# He tenido que cambiar a número entero la columna id del DF peninsula

peninsula.paro=inner_join(peninsula.df,Paro,by=c("id"="Prov.id")) #juntamos las dos bases

ggplot() + 
  geom_polygon(data = peninsula.paro, aes(long, lat, group = group,fill=Tasa.paro), colour = "grey80", size = .1) + 
  facet_grid(~ Sexo) + scale_fill_gradient(low="aliceblue",high="steelblue4")+coord_quickmap()

mujeres=subset(peninsula.paro,Sexo="Mujeres")

ggplot() + 
  geom_polygon(data = mujeres, aes(long, lat, group = group,fill=Tasa.paro), colour = "grey80", size = .1) + 
  scale_fill_gradient(low="aliceblue",high="steelblue4")+coord_quickmap()


# Podemos dibujar este mapa sobre un lienzo obtenido mediante get_map
centro <- c(lon=-3.70379,lat= 40.41678) #geocode("Madrid, Spain")
mapa <- get_map(centro, zoom = 6,maptype="toner-lite",source="stamen")

ggmap(mapa) + geom_polygon(data = mujeres, aes(long, lat, group = group,fill=Tasa.paro),alpha=.8) +
  scale_fill_gradient(low="aliceblue",high="steelblue4")


# Esto es para hacer el gráfico interactivo. Apartado 2.5

install.packages("plotly")
require(plotly)
p<-ggplot(paro, aes(x = AÃ.o, y = Tasa.paro, color=Sexo,label=Provincia)) +
  geom_jitter(alpha=.1) + geom_smooth(se=FALSE,method="lm") 
ggplotly(p,tooltip = c("label", "color"))


install.packages("tmap")
require(tmap)
vignette("tmap-nutshell")
paro.mujeres <- Paro %>% filter(Sexo=="Mujeres" & Trimestre=="I" & AÃ.o=="2011") 
peninsula@data=inner_join(peninsula@data,paro.mujeres,by=c("CCA_2"="Prov.id"))



install.packages("gapminder")
require(gapminder)
p <- ggplot(subset(gapminder, year == 1992)) + 
  geom_point(aes(x = gdpPercap, y = lifeExp, col = continent, size = pop))
p
unique(gapminder$year)
length(gapminder$year)

ggplotly(p, tooltip = c("color"))

