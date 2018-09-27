require(tidyverse)
set.seed(1234) # Con esto establecemos la semilla por defecto para las muestras
# Entonces cada vez que saquemos el sample nos sale lo mismo

require(rvest)
url="https://es.wikipedia.org/wiki/Provincia_de_España"
provincias<-read_html(url) %>% html_nodes("table") %>% .[[2]] %>% html_table(header=TRUE,fill=TRUE) %>% .[[1]]

#####################################
######## Muestreo simple ############
#####################################


sample(provincias,5) # Se seleccionan al azar 5 provincias

########
NN=10000
poblacion=rnorm(NN,108,5)
summary(poblacion)
nn=250 #tamaño muestral
muestra=sample(poblacion,nn)
mean(muestra)
#t.test(muestra)

#####################################
### Muestreo simple con reposición ##
#####################################

muestra=sample(provincias,nn,replace=TRUE)
table(muestra)

muestra=sample(poblacion,nn,replace=TRUE)
mean(muestra)
#t.test(muestra)


#####################################
######## Muestreo estratificado #####
#####################################
View(diamonds)
NN=nrow(diamonds) #tamaño de la poblacion
nn= 250 #tamaño muestral


######## Preambulo
diamonds %>% sample_n(nn) #muestreo simple de la base de datos "diamonds"
diamonds %>% sample_n(nn) %>% summarise(mean(price))
diamonds %>% summarise(mean(price))
diamonds %>%  group_by(cut) %>% summarise(media=mean(price), n = n()) #peso y media de los estratos: calidad del diamante (cut)

######## Muestreo estratificado con afijación fija

muestra <- diamonds %>% group_by(cut) %>% sample_n(nn/5) 
muestra %>% summarise(media=mean(price), n = n())
# Sale con diferencia respecto al original porque hay tipos de diamantes con 
# mucha menos cantidad que otros
# Esta muestra coge 50 de cada tipo

######## Muestreo estratificado con afijación proporcional

muestra <- diamonds %>% group_by(cut) %>% sample_frac(nn/NN) 
muestra %>% summarise(media=mean(price),n = n())

####### Muestreo estratificado por calidad y color del diamante

muestra <- diamonds %>% group_by(cut,color) %>% sample_frac(nn/NN) 
muestra %>% summarise(media=mean(price),n = n())


#####################################
#####Muestreo por conglomerado ######
#####################################

diamonds$origen=sample(provincias,NN,replace=TRUE) #se asigna de manera artificial un estado (americano) a cada diamante
diamonds %>%  group_by(origen) %>% summarise(media=mean(price), n = n())

mm=10 
muestraI <- diamonds %>%  group_by(origen) %>% summarise(N = n()) %>% sample_n(mm,weight=N) #Primero se muestrea 10 estados
muestraI

frameI <- diamonds %>% filter(origen %in% muestraI$origen)  # nos quedamos con los 10 estados seleccionados
NNI=nrow(frameI)
muestraII <- frameI %>%	group_by(origen) %>% sample_frac(nn/NNI) #Muestreo dentro de la base resultante
muestraII %>% summarise(media=mean(price),n = n())

