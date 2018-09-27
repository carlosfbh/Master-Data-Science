# install.packages("tidyverse")
# install.packages("ggmap")
library(tidyverse) #incluye ggplot2
library(ggmap)

# Primera parte. Gráficos para ver como son los datos.
# Segunda parte. Gráficos de dispersión para ver relación entre dos variables. 
# Última parte. Dedicada a proporcionar herramientas para que el gráfico sea dinámico.

## Primera parte. Principales gráficos.

# qplot. Viene bien para representar gráficos rápidamente
x <- runif(1000) # Distribución uniforme
y <- runif(1000)
qplot(x,y)

# Vamos a generar unos sueldos con una distribución normal
sueldos <- rnorm(5000, 2000, 200) # 1er el tamaño muestra, 2do es la media, 3er desviación
summary(sueldos) # Para ver el resumen de los datos

# Vamos a ver una función para ver la distribuión
stem(sueldos)
hist(sueldos)
qplot(sueldos)

# Imaginemos que queremos explicar la diferencia de sueldos.
# Suponemos que la edad tiene un efecto directo en el sueldo.
edades <- rpois(5000, sueldos/40) # Creamos unos valores de edad con distribución de poisson. La media la creamos dependiendo del sueldo
summary(edades)

# Hacemos un gráfico de edad vs sueldo. Eje x edad, eje y sueldo
qplot(edades, sueldos)
qplot(edades, sueldos, geom = c("point", "smooth")) # Le decimos que nos cree una curva que simule la nube de puntos

# Si queremos representar todos los puntos, aunque estén solapados.
# Dos maneras:

# 1. Generando un poco de ruido
qplot(edades, sueldos, geom = "jitter")

# 2. Poner transparencia a los puntos
qplot(edades, sueldos, alpha = I(0.5), size = I(3))

# Para calcular la frecuencia de cada valor usamos la función table
table(edades)
qplot(factor(edades), sueldos, geom = "boxplot") # Si ponemos las edades como factor, nos saca el boxplot para cada edad
# Si queremos girar el gráfico
qplot(factor(edades), sueldos, geom = "boxplot") + coord_flip()
# Podríamos categorizar la variable edad para hacerlo por grupos
gr.edades <- cut(edades, breaks = seq(15, 90, 5))
qplot(gr.edades, sueldos, geom = "boxplot") + coord_flip()

# Vamos a inroducir sexo a nuestros trabajadores
sample(letters, 50, replace = TRUE)
sexos <- sample(c("hombre", "mujer"), 5000, replace = TRUE)

# Creamos data frame con todos los datos que hemos creado
datos <- data.frame(sueldo = sueldos, sexo = sexos, edad = edades, gr.edad = gr.edades)

# Graficamos datos de nuestro DF
qplot(gr.edad, sueldo, data = datos, fill = I("lightblue"), geom = "boxplot") + coord_flip()
qplot(gr.edad, sueldo, data = datos, fill = sexo, geom = "boxplot") + coord_flip()
# Con fill coloreamos el contenido de las cajas

# EJERCICIO. Representar la distribución de la altura de los alumnos de la universidad 
# de Adelaide y su variación con el sexo, a partir de la base de datos survey del 
# paquete MASS.

require(MASS)
?survey
survey <- as.tibble(survey) %>% 
  filter(!is.na(Sex))

qplot(Sex, Height, data = survey, fill = I("lightblue"), geom = "boxplot")

# Voy a representar por grupo de edades
survey$gr.edades = cut(survey$Age, breaks = seq(15, 75, 5))
qplot(gr.edades, Height, data = survey, fill = Sex, geom = "boxplot")
qplot(gr.edades, Pulse, data = survey, geom = "boxplot")

# Ancho de la mano en función de la altura
qplot(Height, Wr.Hnd, data = survey, geom = c("point", "smooth"))

# Representar la distribución de los hábitos con el tabaco según el sexo, 
# en la muestra de alumnos de la universidad de Adelaide. Averiguar cual podría 
# ser la mejor representación (¡hay tantos chicos como chicas!).
qplot(Smoke, fill = Sex, data = survey)
qplot(factor(Sex), fill = Smoke, data = survey)


# Utilizando los base de datos survey, describir la relación entre la altura del alumno
# y la distancia entre sus dedos (pulgar y auricular) de la mano usada para escribir. 
# En un mismo gráfico, describir como esta relación cambia con el sexo.
qplot(Height, Wr.Hnd, color = Sex, size = 1, data = survey, geom = c("point", "smooth"), alpha = I(0.2))


# EJERCICIO
# Describir con un gráfico similar al anterior (apuntes), los datos de la base de datos 
# islands sobre superficies de islas. Puede ser oportuno recurrir a una escala log.
islands <- islands
?islands
summary(islands)
qplot(islands, reorder(names(islands), islands), log = "x")


## Segunda parte. Gráficos avanzados
paro <- read_csv("data/paro.csv")
summary(paro)

ggplot(paro) + # Cargamos la base para hacer el gráfico
  aes(x = Año, y = Tasa.paro, col = Sexo) + # Eligimos las coordenadas y diferenciamos por sexo
  geom_point() + geom_smooth(alpha = 0.5, se = FALSE) + # Decimos que elegimos puntos y curva para dibujar
  facet_wrap( ~ Provincia) # Aquí le decimos los subgráficos, hacerlo de acuerdo a la variable provincia

# Vamos a hacer este ejercicio con los datos de Survey
require(MASS)
?survey
survey <- as.tibble(survey) %>% 
  filter(!is.na(Sex))

p <- ggplot(survey)
summary(p)

p <- p + aes(x = Height, y = Wr.Hnd, col = Sex)
p

p <- p + geom_point() + geom_smooth()
p

# Quitamos el intervalo de confianza
p <- p + geom_point() + geom_smooth(se = FALSE, method = "lm") # Method lm es para que dibuje una recta y no una curva
p
p + facet_wrap(~Smoke)

# Mismo ejercicio con estos datos. Datos de mortalidad en Virginia
VADeaths
# Vamos a alargar este Data Frame. Porque ggplot funciona mejor con datos alargados
temp <- VADeaths %>% 
  as.tibble() %>% 
  mutate(edad = row.names(VADeaths))

mortalidad <- gather(temp,grupo,tasa,-edad)
p <- ggplot(mortalidad)
p <- ggplot(mortalidad, aes(x = edad, y = tasa, colour = grupo, group = grupo, shape = grupo, linetype = grupo))
p <- p + geom_point(alpha = 0.2) + geom_line()
p

# Lo mismo pero diagrama en barras
p <- ggplot(mortalidad, aes(x = edad, y = tasa, fill = grupo))
p + geom_bar(stat = "identity", position = "dodge") # con position = dodge lo que hacemos es que en lugar de poner encima las barras las pone al lado

# Vamos a mejorar este gráfico con ggplot
qplot(class, data=mpg, fill=drv, color=I("gray60"))

ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar(col = "gray60", position = "dodge")

# Vamos a ordenar por categoría los plot de survey
ggplot(survey, aes(x = Sex, fill = Smoke)) +
  geom_bar(col = "gray60", position ="dodge")

# Vamos a crear una variable llamado tabaco que será como Smoke pero ordenada como yo quiero
survey$Tabaco <- factor(survey$Smoke, levels = c("Never", "Occas", "Regul", "Heavy"))
survey$Tabaco

ggplot(survey, aes(x = Sex, fill = Tabaco)) +
  geom_bar(col = "gray60", position = "dodge")

# EJERCICIO
# Elaborar el siguientes gráfico sobre la evolución del paro en España. 
# Utilizar la capa geom_smooth para suavizar la tendencia y la estética 
# linetype para distintos tipos de curvas.

ggplot(paro) + 
  aes(x = Año, y = Tasa.paro, col = Trimestre, linetype = Sexo) +
  geom_smooth(se = FALSE)


# Facetas
ggplot(mortalidad, aes(x = edad, y = tasa)) + 
  geom_bar(stat="identity") +
  facet_grid(~grupo)

# Separamos la columna grupo y la dividimos en dos: zone y sex
mortalidad2 <- mortalidad %>% 
  separate(grupo, c("zone", "sex"), " ")

ggplot(mortalidad2, aes(x = edad, y = tasa)) + 
  geom_bar(stat="identity") +
  facet_grid(zone ~ sex)

# EJERCICIO.
# Elaborar los siguientes gráficos sobre la evolución del paro. 
# Para el segundo gráfico, se puede filtrar la base original con el comando:
# paro.ZHT <- paro %>% filter(Provincia %in% c(“Zaragoza”, “Huesca”, “Teruel”))

paro.ZHT <- paro %>% filter(Provincia %in% c("Zaragoza", "Huesca", "Teruel"))

ggplot(paro, aes(x = Trimestre, y = Tasa.paro, col = Sexo)) +
  geom_boxplot() +
  facet_grid(Sexo ~ Año)

ggplot(paro.ZHT, aes(x = Año, y = Tasa.paro, col = Sexo)) +
  geom_point() + geom_line() +
  facet_grid(Provincia ~ Trimestre)

# Etiquetas
obesidad<-read_csv("data/obesidad.csv")
obesidad
p<-ggplot(obesidad,aes(x=renta,y=imc,color=region))+geom_smooth(method="lm")
p + ggtitle("Relación entre Indice de Masa Corporal (IMC) y renta") + 
  labs(x = "Renta (en miles de $ por año)", y = "IMC (kg/m2)", color = "Continente")

p<-ggplot(obesidad,aes(x=renta,y=imc))+geom_point(size=.1)+geom_smooth(method="lm")
p 

# Vemos que hay un problema de escala:
p + scale_x_log10() + scale_y_continuous(breaks=seq(10,50,10),trans="log")

# Temas
p <- ggplot(obesidad,aes(x=renta,y=imc,color=region))+geom_smooth(method="lm")
p + facet_wrap(~region)
p + facet_wrap(~region) + theme_bw()
p + facet_wrap(~region) + theme_classic()

ggplot(mortalidad, aes(x=edad, y=tasa, label=tasa, fill = grupo)) + 
  geom_bar(stat="identity", alpha = 0.5) + geom_text(nudge_y = 1) +
  facet_grid(~grupo)

# Exportar
ggsave("mortalidad.pdf")
ggsave("mortalidad.pdf", width = 40, height = 20, units = "cm")
