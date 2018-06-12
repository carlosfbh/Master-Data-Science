##### EJERCICIOS VISUALIZACIÓN qplot & ggplot2 ######

library(tidyverse)
library(ggmap)

##########################################################################
# 1. Exercise: 
# Representar la distribución de la altura de los alumnos de la universidad 
# de Adelaide y su variación con el sexo, a partir de la base de datos survey del 
# paquete MASS.

# También representa la relación del ancho de la mano en función de la altura
##########################################################################

require(MASS)
View(survey)
survey <- survey %>% 
  filter(!is.na(Sex))
qplot(Sex, Height, data = survey, geom = "boxplot", fill = I("lightblue"))

survey <- survey %>% 
  filter(!is.na(Age)) %>% 
  mutate(gr_edades = cut(Age, breaks = seq(15, 80, 5)))

qplot(gr_edades, Height, data = survey, geom = "boxplot", fill = Sex)

# Ancho de la mano en función de la altura
qplot(Wr.Hnd, Height, data = survey, geom = c("smooth", "point"))


##########################################################################
# 2. Exercise: 
# Representar la distribución de los hábitos con el tabaco según el sexo, 
# en la muestra de alumnos de la universidad de Adelaide. Averiguar cual podría 
# ser la mejor representación (¡hay tantos chicos como chicas!).
##########################################################################

qplot(Smoke, fill = Sex, data = survey)
qplot(Sex, fill = Smoke, data = survey)

##########################################################################
# 3. Exercise: 
# Utilizando los base de datos survey, describir la relación entre la altura del alumno
# y la distancia entre sus dedos (pulgar y auricular) de la mano usada para escribir. 
# En un mismo gráfico, describir como esta relación cambia con el sexo.
##########################################################################

qplot(NW.Hnd, Height, data = survey, color = Sex, geom = c("point", "smooth"),
      size = I(0.5))


##########################################################################
# 4. Exercise: 
# Describir con un gráfico de dispersión los datos de la base de datos islands
# sobre superficies de islas. Puede ser oportuno recurrir a una escala log.
##########################################################################

View(islands)
islands <- islands
names(islands)
qplot(islands, reorder(names(islands), islands)) + scale_x_log10()



##########################################################################
# 5. Exercise: 
# Utlizando la base de datos del paro guardada en data:
# Realizar un gráfico que muestre la evolución de la tasa de paro según 
# la provincia y el sexo en el periodo 2011-2014
##########################################################################

paro <- read.csv("data/paro.csv", encoding="UTF-8")

ggplot(paro) + 
  aes(x = Año, y = Tasa.paro, col = Sexo) +
  geom_point() + geom_smooth() +
  facet_wrap( ~ Provincia)
  

##########################################################################
# 6. Exercise: 
# Según los datos de la mortalidad en Virginia VADeaths
# Realizar un gráfico que muestre la tasa de mortalidad según edad y condición
# Realizar el gráfico con puntos unidos por líneas
##########################################################################

mortalidad <- VADeaths

# Cómo los datos no están alargados es mejor transformar el DF para que ggplot
# trabaje mejor

mortalidad <- mortalidad %>%
  as.tibble() %>% # Change the matrices to Data Frames
  mutate(edad = row.names(VADeaths))

mortalidad <- gather(mortalidad, grupo, tasa, -edad)
# Con la función gather hacemos lo siguiente:
# Cogemos el DF mortalidad
# Creamos una variable llamada grupo que tendrá el valor de las columnas del DF
# Creamos una variable llamada tasa que tendrá el valor que tenían las columnas del DF
# Le decimos que haga esto con todas las columnas del DF menos con la variable edad.

ggplot(mortalidad) + 
  aes(x = edad, y = tasa, col = grupo, group = grupo) + 
  geom_point() + geom_line()
# Hay que poner group = grupo para que luego el geom_line() sea capaz de entender
# los puntos que tiene que unir con líneas



##########################################################################
# 7. Exercise: 
# Realizar el mismo ejercicio que antes pero hacer un gráfico por barras
# Las barras deben estar graficadas unas al lado de otras y en orden
# de menor a mayor
##########################################################################

ggplot(mortalidad) +
  aes(x = edad, y = tasa, fill = grupo) + 
  geom_bar(stat = "identity", position = "dodge")

# Para que las barras estén ordenadas de menor a mayor voy a crear una
# nueva variable "condición" y la voy a ordenar como yo quiero.

mortalidad <- mortalidad %>% 
  mutate(condicion = factor(grupo, levels = c("Urban Female", "Rural Female", 
                                              "Rural Male", "Urban Male")))

ggplot(mortalidad) +
  aes(x = edad, y = tasa, fill = condicion) + 
  geom_bar(stat = "identity", position = "dodge")



##########################################################################
# 8. Exercise: 
# Realizar un gráfico mostrando la evolución del paro de españa entre los años
# 2011 y 2014 con líneas y se muestre:
# diferenciación según el color para el trimestre
# diferenciación del tipo de línea según sexo
##########################################################################

ggplot(paro) + 
  aes(x = Año, y = Tasa.paro, col = Trimestre, linetype = Sexo) +
  geom_smooth(se = FALSE)



##########################################################################
# 9. Exercise: 
# Sobre los datos de mortalidad en Virginia:
# Realizar un gráfico de barras pero subdividido por sexo y grupo
# Poner con número los datos de cada barra
# Poner etiquetas, tema y exportarlo en pdf
##########################################################################

# Primero tengo que crear una nueva variable llamada sexo

mortalidad <- mortalidad %>% 
  separate(grupo, c("grupo", "Sexo"))

ggplot(mortalidad) +
  aes(x = edad, y = tasa, fill = grupo, label = tasa) +
  geom_bar(stat = "identity", alpha = 0.5) + 
  geom_text(nudge_y = -5) +
  ggtitle("Tasa de Mortalidad en Virginia") +
  labs(x = "Edad", y = "Tasa de Mortalidad", fill = "Grupo") +
  facet_grid(grupo ~ Sexo) +
  theme_bw()

ggsave("Mortalidad.pdf", width = 40, height = 20, units = "cm")

##########################################################################
# 10. Exercise: 
# Realizar un gráfico sobre el paro de las provincias de Aragón:
# Que se vea la evolución por trimestre y que cada año tenga su propio gráfico
# Diferenciación de color por sexo
# Cada sexo debe tener también su gráfico
# Gráfico en forma de boxplot
##########################################################################

paro_HZT <- paro %>% 
  filter(Provincia %in% c("Zaragoza", "Huesca", "Teruel"))

ggplot(paro_HZT) + 
  aes(x = Trimestre, y = Tasa.paro, col = Sexo) + 
  geom_boxplot() + 
  facet_grid(Sexo ~ Año)
  

##########################################################################
# 11. Exercise: 
# Realizar un gráfico sobre el paro de las provincias de Aragón:
# Que se vea la evolución por año y que cada trimestre tenga su propio gráfico
# Diferenciación de color por sexo
# Cada provincia debe tener también su gráfico
# Gráfico en forma de puntos unidos por líneas
##########################################################################

ggplot(paro_HZT) + 
  aes(x = Año, y = Tasa.paro, col = Sexo) + 
  geom_point() + geom_line() + 
  facet_grid(Provincia ~ Trimestre)


##########################################################################
# 12. Exercise: 
# Realizar un gráfico sobre los datos de obesidad del csv:
# Que se vea la relación del IMC por Renta
# Diferenciación de colo0 y gráfico por región
# Que aparezcan etiquetas para título, ejes y color
# Añadirle un tema y exportarlo a pdf
##########################################################################

ggplot(obesidad) + 
  aes(x = renta, y = imc, col = region) + 
  geom_point(size = 0.1, alpha = 0.5) + geom_smooth(method = "lm") + 
  ggtitle("Relación entre IMC y Renta") + 
  labs(x = "Renta(M$/Año)", y = "IMC(kg/m2)", color = "Continente") + 
  scale_x_log10() + scale_y_continuous(breaks=seq(10,50,10),trans="log") +
  facet_wrap( ~ region) + 
  theme_bw()

ggsave("Obesidad.pdf", width = 40, height = 20, units = "cm")
