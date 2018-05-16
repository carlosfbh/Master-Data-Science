# Ejercicios en base al csv de Titanic sacado de Kaggle
# https://www.kaggle.com/c/titanic/data


# 1. Leer archivo csv y guardarlo en una variable
titanic <- read.csv("train.csv")


# 2. Cuál es la edad del sujeto numero 7
titanic[7, "Age"]


# 3. Obtener el sujeto #7
titanic[7,]


# 4. Qué edad tiene la persona más joven
min(titanic$Age, na.rm = TRUE)


# 5. Calcula la media y máximo de la edad
mean(titanic$Age, na.rm = TRUE)
max(titanic$Age, na.rm = TRUE)


## 6. Selecciona las filas de menores de edad y
# guardalo en un nuevo dataframe llamado menores
menores <- titanic[titanic$Age < 18,]


## 7. Ejercico mejorado. Vamos a quitar los NA que aparecen en menores
menores <- titanic[titanic$Age < 18 & !is.na(titanic$Age),]
mean(menores$Survived)
mean(titanic$Survived)


## 8. Filtra aquellas mujeres de primera clase y extrae
# las columnas Fare y Survived
mujeres1clase <- titanic[titanic$Pclass == 1 & titanic$Sex == "female", c("Fare", "Survived")]


# 9. Porcentaje de supervivencia en este grupo.
mean(mujeres1clase$Survived)


# 10. Media de edad de hombres que sobrevivieron
mean(titanic[titanic$Sex == "male" & titanic$Survived, "Age"], na.rm = TRUE)


# 11. Cuantas personas sobrevivieron
nrow(titanic[titanic$Survived == 1,])
sum(titanic$Survived)


# 12. Cuantas personas fallecieron
nrow(titanic[titanic$Survived == 0,])
sum(!titanic$Survived)


# 13. Cuantas personas viajaron en el titanic
nrow(titanic)
nrow(titanic[titanic$Embarked != "",])
sum(titanic$Embarked != "")


# 14. Ratio Entre personas de primera clase y tercera
sum(titanic$Pclass == 1)
sum(titanic$Pclass == 3)
sum(titanic$Pclass == 3) / sum(titanic$Pclass == 1)


# 15. Seleccionar columna Age y Sex para personas de primera clase
titanic[titanic$Pclass == 1, c("Age", "Sex")]


# 16.  Calcula la máscara para seleccionar los supervivientes
##   de tercera clase o los hombres de primera que fallecieron
View(titanic[(titanic$Survived == 1 & titanic$Pclass == 3) |
               (titanic$Survived == 0 & titanic$Pclass == 1) & 
               titanic$Sex == "male",])

# 17. Correlación entre edad y fare para cada sexo
cor(titanic$Age, titanic$Fare, use = "comple")

## 18. Crea una nueva columna en "titanic" que se llame riesgo que valga
# para cada grupo el siguiente valor:
#            Mujer    Hombre
# 1a clase   1        2
# 2a clase   2        3
# 3a clase   3        4
riesgo <- vector(mode = "numeric", length = nrow(titanic))
titanic <- data.frame(titanic, riesgo)

titanic$riesgo[titanic$Sex == "female" & titanic$Pclass == 1] <- 1
titanic$riesgo[titanic$Sex == "female" & titanic$Pclass == 2] <- 2
titanic$riesgo[titanic$Sex == "female" & titanic$Pclass == 3] <- 3
titanic$riesgo[titanic$Sex == "male" & titanic$Pclass == 1] <- 2
titanic$riesgo[titanic$Sex == "male" & titanic$Pclass == 2] <- 3
titanic$riesgo[titanic$Sex == "male" & titanic$Pclass == 3] <- 4

## 19. Crea otra columna desviacionFare con la diferencia entre Fare y la media
# de todos los "Fare" del dataset.
# Calcula el mínimo y máximo de esta nueva variable para cada combinación
# de sexo y clase.
desviacionFare <- vector(mode = "numeric", length = nrow(titanic))
titanic <- data.frame(titanic, desviacionFare)

titanic$desviacionFare <- titanic$Fare - mean(titanic$Fare)

summary(titanic[titanic$Sex == "male" & titanic$Pclass == 1, "desviacionFare"])
summary(titanic[titanic$Sex == "male" & titanic$Pclass == 2, "desviacionFare"])
summary(titanic[titanic$Sex == "male" & titanic$Pclass == 3, "desviacionFare"])
summary(titanic[titanic$Sex == "female" & titanic$Pclass == 1, "desviacionFare"])
summary(titanic[titanic$Sex == "female" & titanic$Pclass == 2, "desviacionFare"])
summary(titanic[titanic$Sex == "female" & titanic$Pclass == 3, "desviacionFare"])



## 20. Mira la ayuda de xtabs y presenta los mínimos de otra manera con
# esta función

xtabs(desviacionFare ~ Pclass + Sex, aggregate(desviacionFare ~ Pclass + Sex, titanic, min))

