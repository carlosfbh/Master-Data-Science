## -------------------------------------------------------------------------
## SCRIPT: Recomendador de Productos.R
## CURSO: Soluciones de Negocio para Cientificos de Datos y Analistas
## FECHA: 08/04/2017
## Paquetes Necesarios: tm
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####

if(!require("tm")){
  install.packages("tm")
  library("tm")
}

## -------------------------------------------------------------------------

##### 2. Bloque de parametros iniciales #####

setwd("D:/Documentos, Trabajos y Demás/Formación/Kschool/201703 Curso de Soluciones de Negocio/6 - E-commerce - Recomendador de Productos")

## -------------------------------------------------------------------------

##### 3. Bloque de carga de informacion #####

Juguetes=read.csv2("0 datos/dataset Juguetes.csv", stringsAsFactors = FALSE)

## -------------------------------------------------------------------------

##### 4. Bloque de analisis del dataset #####

str(Juguetes)
head(Juguetes)
tail(Juguetes)
summary(Juguetes)

ListadoJuguetes=Juguetes$Titulo

## -------------------------------------------------------------------------

##### 5. Bloque de tratamiento de la información #####

dim(Juguetes)[1]
Muestra=1000
# Muestra=5597

corpus<-Corpus(VectorSource(Juguetes$Descripcion[1:Muestra]))
corpus[[1]]$content
corpus[[1]]$meta

corpus2<-tm_map(corpus, PlainTextDocument)
corpus2[[1]]$content

corpus2<-tm_map(corpus2, content_transformer(tolower))
corpus2[[1]]$content

#stopwords("spanish")
corpus2<-tm_map(corpus2, removeWords, stopwords("spanish"))
corpus2[[1]]$content

corpus2<-tm_map(corpus2, removeWords, c("juguete","juguetes","edad","máxima","recomendada","incluye"))
corpus2[[1]]$content

corpus2<-tm_map(corpus2, removePunctuation)
corpus2[[1]]$content

corpus2<-tm_map(corpus2, removeNumbers)
corpus2[[1]]$content

corpus2<-tm_map(corpus2, stripWhitespace)
corpus2[[1]]$content

## -------------------------------------------------------------------------

##### 6. Bloque de creacion de matrices de documentos y terminos #####

#TermDocumentMatrix(corpus2) 
tdm<-TermDocumentMatrix(corpus2, control=list(wordLengths=c(1,Inf)))
tdm$dimnames$Docs=Juguetes$Titulo[1:Muestra]
dtm<-DocumentTermMatrix(corpus2, control=list(wordLengths=c(1,Inf)))
dtm$dimnames$Docs=Juguetes$Titulo[1:Muestra]
inspect(tdm)
inspect(dtm)

tdm2<-TermDocumentMatrix(corpus2, control=list(wordLengths=c(1,Inf),weighting=function(x) weightTfIdf(x,normalize=TRUE)))
tdm2$dimnames$Docs=Juguetes$Titulo[1:Muestra]
dtm2<-DocumentTermMatrix(corpus2, control=list(wordLengths=c(1,Inf),weighting=function(x) weightTfIdf(x,normalize=TRUE)))
dtm2$dimnames$Docs=Juguetes$Titulo[1:Muestra]
inspect(tdm2)
inspect(dtm2)

## -------------------------------------------------------------------------

##### 7. Bloque de inspeccion de matriz de documentos y terminos #####

dim(tdm2)
inspect(tdm2[,1:5])
inspect(tdm2[,1])

juguete= 7 # numero o nombre completo
indices=which(inspect(tdm2[,juguete])>0)
inspect(tdm2[indices,juguete])

juguete= 20 # numero o nombre completo
indices=which(inspect(tdm2[,juguete])>0)
inspect(tdm2[indices,juguete])

termino="toalla" # numero o termino
indices=which(inspect(tdm2[termino,])>0)
inspect(tdm2[termino,indices])

termino="azul"
indices=which(inspect(tdm2[termino,])>0)
inspect(tdm2[termino,indices])

## -------------------------------------------------------------------------

##### 8. Bloque de calculo de similitudes de objetos #####

inicio <- proc.time()
N=tdm2$ncol
dim(tdm2)
dim(dtm2)

ProductosEscalares=as.matrix(dtm2)%*%as.matrix(tdm2)
proc.time()-inicio 
## Para N=5597 
## user  system elapsed 
## 428.81    1.97  432.54 
normas=sqrt(diag(ProductosEscalares))
Simil=(1/normas)*t(ProductosEscalares*(1/normas))

# Grabamos los objetos con la informacion
#save(ProductosEscalares,file="./1 matrices productos/ProductosEscalares.RData")
#save(normas,file="./1 matrices productos/normas.RData")
#save(Simil,file="./1 matrices productos/Similitudes.RData")

# cargamos los objetos
load(file="./1 matrices productos/ProductosEscalares.RData")
load(file="./1 matrices productos/normas.RData")
load(file="./1 matrices productos/Similitudes.RData")

## -------------------------------------------------------------------------

##### 9. Bloque de similitud de objetos #####

Top=10
Num_juguete=25  #15, 25, 45,88, 76, 3745, 1325, 874, 13
colnames(Simil)[Num_juguete]
Simil[order(Simil[,Num_juguete],decreasing=TRUE)[2:(Top+1)],Num_juguete]

## -------------------------------------------------------------------------

##### 10. Bloque de filtrado colaborativo item-item #####

ValoracionIndividual=read.csv2("./3 valoraciones individuales/Valoraciones individual.csv",stringsAsFactors = FALSE)

str(ValoracionIndividual)
summary(ValoracionIndividual)
JuguetesValorados=which(!is.na(ValoracionIndividual$Valoracion))

MediaIndividual=mean(ValoracionIndividual$Valoracion,na.rm=TRUE)
ValoracionIndividual$ValoracionNorm=ValoracionIndividual$Valoracion-MediaIndividual
ValoracionIndividual$ValoracionNorm[is.na(ValoracionIndividual$ValoracionNorm)]=0

PuntuacionesIndividuales=Simil %*% ValoracionIndividual$ValoracionNorm
PrioridadIndividual=order(PuntuacionesIndividuales, decreasing=TRUE)
PrioridadFiltrada=PrioridadIndividual[!(PrioridadIndividual %in% JuguetesValorados)]

RecomendacionesIndividuales=ListadoJuguetes[PrioridadFiltrada[1:20]]

RecomendacionesIndividuales

write.csv2(RecomendacionesIndividuales,"./4 recomendacion content-based/recomendaciones individuales.csv",row.names = FALSE)

## -------------------------------------------------------------------------

##### 11. Bloque de filtrado colaborativo user-user #####

ValoracionesTotales=read.csv2("./5 valoraciones poblacion/Valoraciones Totales.csv",stringsAsFactors = FALSE)

Medias=apply(ValoracionesTotales[,3:dim(ValoracionesTotales)[2]],2,mean,na.rm=TRUE)
Desviaciones=apply(ValoracionesTotales[,3:dim(ValoracionesTotales)[2]],2,sd,na.rm=TRUE)

ValoracionesTotales[1:10,3:dim(ValoracionesTotales)[2]]
Medias
Matriz=t(t(ValoracionesTotales[,3:dim(ValoracionesTotales)[2]])-Medias)
Matriz[is.na(Matriz)]=0
dim(Matriz)
CovarianzasUsuarios=t(Matriz)%*% Matriz
normasUsuarios=sqrt(diag(CovarianzasUsuarios))

SimilUsuarios=(1/normasUsuarios)*t(CovarianzasUsuarios*(1/normasUsuarios))
#SimilUsuarios=cor(Matriz)
SimilUsuarios

MatrizNorm=t(t(Matriz)/Desviaciones)

Numerador=MatrizNorm %*% SimilUsuarios
MatrizNorm[1:10,]
Numerador[1:10,]

Denominador=(!MatrizNorm==0) %*% abs(SimilUsuarios)
Denominador[1:10,]

Cociente=Numerador/Denominador
Cociente[1:10,]

Usuario=4
Orden=order(Cociente[,Usuario],decreasing = TRUE)
RecomendacionesUserUser=Orden[Orden %in% which(Matriz[,Usuario]==0) & !is.na(Cociente[Orden])]

ListadoJuguetes[RecomendacionesUserUser]

write.csv2(ListadoJuguetes[RecomendacionesUserUser],paste("./6 recomendaciones user-user/Recomendaciones Individuales Usuario ",Usuario,".csv",sep=""),row.names = FALSE)

## -------------------------------------------------------------------------