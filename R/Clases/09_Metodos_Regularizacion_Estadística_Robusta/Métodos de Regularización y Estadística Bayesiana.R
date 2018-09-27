## -------------------------------------------------------------------------
## SCRIPT: Métodos de Regularización y Estadística Bayesiana.R
## CURSO: Master en Data Science
## SESION: Métodos de Regularización y Estadística Bayesiana
## PROFESOR: Antonio Pita Lozano
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####

if (!require("e1071")){
  install.packages("e1071")
  library("e1071")
}

if (!require("ROCR")){
  install.packages("ROCR")
  library("ROCR")
}

if (!require("glmnet")){
  install.packages("glmnet") 
  library("glmnet")
}

if (!require("caTools")){
  install.packages("caTools") 
  library(caTools)
}

setwd("D:/Documentos, Trabajos y Demás/Formación/Kschool/201805 Clase VII Master Data Science/4 Métodos de Regularización")

## -------------------------------------------------------------------------
##       PARTE 1: Métodos de Regularización
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 2. Bloque de carga de datos #####

files <- (Sys.glob("./Viviendas/*.csv"))

if (exists("Viviendas")){rm(Viviendas)}

for (file in files){
  print(paste("Procesando fichero: ",file,sep=""))
  data=read.csv2(file,stringsAsFactors = FALSE)
  #data=read.csv2(file,stringsAsFactors = FALSE, fileEncoding = "windows-1252", sep=";")
  if (exists("Viviendas")){
    Viviendas=rbind(Viviendas,data)
  }else{
    Viviendas=data
  }
}

## -------------------------------------------------------------------------

##### 3. Bloque de revisión basica del dataset #####

str(Viviendas)
head(Viviendas)
tail(Viviendas)
summary(Viviendas)

## -------------------------------------------------------------------------

##### 4. Bloque de formateo de variables #####

Viviendas$Longitud=as.numeric(Viviendas$Longitud)
Viviendas$Latitud=as.numeric(Viviendas$Latitud)

## -------------------------------------------------------------------------

##### 5. Bloque de tratamiento de información #####

table(Viviendas$Planta)
Viviendas$Planta[grepl("Hipoteca",Viviendas$Planta)]=""
table(Viviendas$Planta)
Viviendas$Planta[grepl("Ha bajado",Viviendas$Planta)]=""
table(Viviendas$Planta)

## -------------------------------------------------------------------------

##### 6. Bloque de filtrado de registros #####

# eliminamos los registros sin precio
Viviendas[is.na(Viviendas$Precio),]
Viviendas=Viviendas[!is.na(Viviendas$Precio),]

## -------------------------------------------------------------------------

##### 7. Bloque de filtrado de registros #####

table(Viviendas$Habitaciones,useNA="always")
#analizar los nulos de habitaciones
Nulos=Viviendas[is.na(Viviendas$Habitaciones),]
Nulos

Viviendas$Habitaciones[is.na(Viviendas$Habitaciones) & grepl("Estudio",Viviendas$Direccion)]=1

Nulos=Viviendas[is.na(Viviendas$Habitaciones),]
Nulos

# Eliminamos los registros sin Habitaciones
Viviendas=Viviendas[!is.na(Viviendas$Habitaciones),]


## -------------------------------------------------------------------------

##### 8. Bloque de análisis gráfico #####

hist(Viviendas$Precio)
hist(log(Viviendas$Precio))
boxplot(Viviendas$Precio)
boxplot(Viviendas$Precio[Viviendas$Precio<300000])

hist(Viviendas$Superficie)
hist(log(Viviendas$Superficie))
boxplot(Viviendas$Superficie)
boxplot(Viviendas$Superficie[Viviendas$Superficie<200])

## -------------------------------------------------------------------------

##### 9. Bloque de creación de variables auxiliares #####

str(Viviendas)

Viviendas$madrid=as.numeric(grepl("Madrid",Viviendas$Info))
sum(Viviendas$madrid)
Viviendas$madrid=as.numeric(grepl("madrid",Viviendas$Info))
sum(Viviendas$madrid)
Viviendas$madrid=as.numeric(grepl("[Mm][Aa][Dd][Rr][Ii][Dd]",Viviendas$Info))
sum(Viviendas$madrid)

Viviendas$piscina=as.numeric(grepl("[Pp][Ii][Ss][Cc][Ii][Nn][Aa]",Viviendas$Info))
Viviendas$garage=as.numeric(grepl("[Gg][Aa][Rr][Aa][Gg][Ee]",Viviendas$Info))
Viviendas$ascensor=as.numeric(grepl("[Aa][Ss][Cc][Ee][Nn][Ss][Oo][Rr]",Viviendas$Info))
Viviendas$terraza=as.numeric(grepl("[Tt][Ee][Rr][Rr][Aa][Zz][Aa]",Viviendas$Info))
Viviendas$amueblado=as.numeric(grepl("[Aa][Mm][Uu][Ee][Bb][Ll][Aa][Dd][OoAa]",Viviendas$Info))

Viviendas$piso=as.numeric(grepl("[Pp][Ii][Ss][Oo]",Viviendas$Direccion))
Viviendas$duplex=as.numeric(grepl("[Dd][UuÚú][Pp][Ll][Ee][Xx]",Viviendas$Direccion))
Viviendas$chalet=as.numeric(grepl("[Cc][Hh][Aa][Ll][Ee][Tt]",Viviendas$Direccion))
Viviendas$estudio=as.numeric(grepl("[Ee][Ss][Tt][Uu][Dd][Ii][Oo]",Viviendas$Direccion))
Viviendas$atico=as.numeric(grepl("[AaÁá][Tt][Ii][Cc][Oo]",Viviendas$Direccion))

head(Viviendas)
summary(Viviendas)

table(Viviendas$piso,Viviendas$duplex)
table(Viviendas$chalet,Viviendas$duplex)
table(Viviendas$chalet,Viviendas$piso)
table(Viviendas$chalet,Viviendas$estudio)
table(Viviendas$piso,Viviendas$estudio)
table(Viviendas$duplex,Viviendas$estudio)

table(Viviendas$piso + Viviendas$duplex + Viviendas$chalet + Viviendas$estudio + Viviendas$atico)

Casos=Viviendas[Viviendas$piso + Viviendas$duplex + Viviendas$chalet + Viviendas$estudio + Viviendas$atico==2,]
Casos
Viviendas$atico[Viviendas$piso + Viviendas$duplex + Viviendas$chalet + Viviendas$estudio + Viviendas$atico==2]=0

Casos=Viviendas[Viviendas$piso + Viviendas$duplex + Viviendas$chalet + Viviendas$estudio + Viviendas$atico==0,]
Casos

Viviendas$casarural=as.numeric(grepl("Casa rural",Viviendas$Direccion))
Viviendas$casapueblo=as.numeric(grepl("Casa de pueblo",Viviendas$Direccion))
Viviendas$fincarustica=as.numeric(grepl("Finca rústica",Viviendas$Direccion))

table(Viviendas$piso + Viviendas$duplex + Viviendas$chalet + Viviendas$estudio + Viviendas$atico + Viviendas$casarural + Viviendas$casapueblo + Viviendas$fincarustica)

Casos=Viviendas[Viviendas$piso + Viviendas$duplex + Viviendas$chalet + Viviendas$estudio + Viviendas$atico + Viviendas$casarural + Viviendas$casapueblo + Viviendas$fincarustica==0,]
Casos

## -------------------------------------------------------------------------

##### 10. Bloque de creación de conjuntos de entrenamiento y test #####

set.seed(12345) 
SAMPLE = sample.split(Viviendas$Precio, SplitRatio = 0.75)
Train = subset(Viviendas, SAMPLE == TRUE)
Test = subset(Viviendas, SAMPLE == FALSE)

## -------------------------------------------------------------------------

##### 11. Bloque de parámetros básicos Regularización #####

variables=c("Superficie","madrid","piscina","garage","ascensor","terraza","amueblado","piso","duplex","chalet","estudio","atico","casarural","casapueblo","fincarustica")
Lambda=50000
Pruebas=1500

## -------------------------------------------------------------------------

##### 12. Bloque de regression Ridge #####

alphaSeleccionado=0

coeficientes=matrix(0,nrow=Pruebas,ncol=length(variables)+1)
coeficientes=as.data.frame(coeficientes)
colnames(coeficientes)=c("termino_independiente",variables)

metricas=data.frame(sceTrain=rep(0,Pruebas),sceTest=rep(0,Pruebas))

for (i in 1:Pruebas){
  modelo_glmnet=glmnet(x=as.matrix(Train[,variables]),y=Train$Precio,lambda=Lambda*(i-1)/Pruebas,alpha=alphaSeleccionado)
  coeficientes[i,]=c(modelo_glmnet$a0,as.vector(modelo_glmnet$beta))
  
  prediccionesTrain=predict(modelo_glmnet,newx = as.matrix(Train[,variables]))
  metricas$sceTrain[i]=sum((Train$Precio-prediccionesTrain)^2)
  
  prediccionesTest=predict(modelo_glmnet,newx = as.matrix(Test[,variables]))
  metricas$sceTest[i]=sum((Test$Precio-prediccionesTest)^2)
}

# Gráfico con evolución de los coeficientes
colores=rainbow(length(variables))
plot(coeficientes[,1],type="l",col="white",ylim=c(-300000,300000))
for (i in 1:length(variables)){
  lines(coeficientes[,i+1],type="l",col=colores[i])
}

# Gráfico con evolución de los errores
par(mar = c(5,5,2,5)) #cambiamos la configuración de la parte gráfica
plot(metricas$sceTrain,col="red",type="l",ylab="Error Train",xlab="Prueba")
par(new = T)
plot(metricas$sceTest,col="blue",type="l",axes=FALSE,xlab=NA, ylab=NA)
axis(side = 4)
mtext(side = 4, line = 3, 'Error Test')
par(mar = c(5.1,4.1,4.1,2.1)) #volvemos a la configuración inicial

# Selección del Lambda Óptimo
min(metricas$sceTest)
which(metricas$sceTest==min(metricas$sceTest))
Caso=1246

# Modelo y Parámetro
metricasRidge=metricas[Caso,]
lambdaRidge=Lambda*(Caso-1)/Pruebas
coeficientesRidge=coeficientes[Caso,]
modeloRidge=glmnet(x=as.matrix(Train[,variables]),y=Train$Precio,lambda=Lambda*(Caso-1)/Pruebas,alpha=alphaSeleccionado)
modeloRidge$beta

## -------------------------------------------------------------------------

##### 13. Bloque de regression Lasso #####

alphaSeleccionado=1

coeficientes=matrix(0,nrow=Pruebas,ncol=length(variables)+1)
coeficientes=as.data.frame(coeficientes)
colnames(coeficientes)=c("termino_independiente",variables)

metricas=data.frame(sceTrain=rep(0,Pruebas),sceTest=rep(0,Pruebas))

for (i in 1:Pruebas){
  modelo_glmnet=glmnet(x=as.matrix(Train[,variables]),y=Train$Precio,lambda=Lambda*(i-1)/Pruebas,alpha=alphaSeleccionado)
  coeficientes[i,]=c(modelo_glmnet$a0,as.vector(modelo_glmnet$beta))
  
  prediccionesTrain=predict(modelo_glmnet,newx = as.matrix(Train[,variables]))
  metricas$sceTrain[i]=sum((Train$Precio-prediccionesTrain)^2)
  
  prediccionesTest=predict(modelo_glmnet,newx = as.matrix(Test[,variables]))
  metricas$sceTest[i]=sum((Test$Precio-prediccionesTest)^2)
}

# Gráfico con evolución de los coeficientes
colores=rainbow(length(variables))
plot(coeficientes[,1],type="l",col="white",ylim=c(-300000,300000))
for (i in 1:length(variables)){
  lines(coeficientes[,i+1],type="l",col=colores[i])
}

# Gráfico con evolución de los errores
par(mar = c(5,5,2,5)) #cambiamos la configuración de la parte gráfica
plot(metricas$sceTrain,col="red",type="l",ylab="Error Train",xlab="Prueba")
par(new = T)
plot(metricas$sceTest,col="blue",type="l",axes=FALSE,xlab=NA, ylab=NA)
axis(side = 4)
mtext(side = 4, line = 3, 'Error Test')
par(mar = c(5.1,4.1,4.1,2.1)) #volvemos a la configuración inicial

# Selección del Lambda Óptimo
min(metricas$sceTest)
which(metricas$sceTest==min(metricas$sceTest))
Caso=708

# Modelo y Parámetro
metricasLasso=metricas[Caso,]
lambdaLasso=Lambda*(Caso-1)/Pruebas
coeficientesLasso=coeficientes[Caso,]
modeloLasso=glmnet(x=as.matrix(Train[,variables]),y=Train$Precio,lambda=Lambda*(Caso-1)/Pruebas,alpha=alphaSeleccionado)
modeloLasso$beta

## -------------------------------------------------------------------------

##### 14. Bloque de regression Elastic Net #####

alphaSeleccionado=0.5

coeficientes=matrix(0,nrow=Pruebas,ncol=length(variables)+1)
coeficientes=as.data.frame(coeficientes)
colnames(coeficientes)=c("termino_independiente",variables)

metricas=data.frame(sceTrain=rep(0,Pruebas),sceTest=rep(0,Pruebas))

for (i in 1:Pruebas){
  modelo_glmnet=glmnet(x=as.matrix(Train[,variables]),y=Train$Precio,lambda=Lambda*(i-1)/Pruebas,alpha=alphaSeleccionado)
  coeficientes[i,]=c(modelo_glmnet$a0,as.vector(modelo_glmnet$beta))
  
  prediccionesTrain=predict(modelo_glmnet,newx = as.matrix(Train[,variables]))
  metricas$sceTrain[i]=sum((Train$Precio-prediccionesTrain)^2)
  
  prediccionesTest=predict(modelo_glmnet,newx = as.matrix(Test[,variables]))
  metricas$sceTest[i]=sum((Test$Precio-prediccionesTest)^2)
}

# Gráfico con evolución de los coeficientes
colores=rainbow(length(variables))
plot(coeficientes[,1],type="l",col="white",ylim=c(-200000,300000))
for (i in 1:length(variables)){
  lines(coeficientes[,i+1],type="l",col=colores[i])
}

# Gráfico con evolución de los errores
par(mar = c(5,5,2,5)) #cambiamos la configuración de la parte gráfica
plot(metricas$sceTrain,col="red",type="l",ylab="Error Train",xlab="Prueba")
par(new = T)
plot(metricas$sceTest,col="blue",type="l",axes=FALSE,xlab=NA, ylab=NA)
axis(side = 4)
mtext(side = 4, line = 3, 'Error Test')
par(mar = c(5.1,4.1,4.1,2.1)) #volvemos a la configuración inicial

# Selección del Lambda Óptimo
min(metricas$sceTest)
which(metricas$sceTest==min(metricas$sceTest))
Caso=1156

# Modelo y Parámetro
metricasElasticNet=metricas[Caso,]
lambdaElasticNet=Lambda*(Caso-1)/Pruebas
coeficientesElasticNet=coeficientes[Caso,]
modeloElasticNet=glmnet(x=as.matrix(Train[,variables]),y=Train$Precio,lambda=Lambda*(Caso-1)/Pruebas,alpha=alphaSeleccionado)
modeloElasticNet$beta

## -------------------------------------------------------------------------

##### 15. Bloque de comparativa de Modelos #####

metricasRidge
metricasLasso
metricasElasticNet

## -------------------------------------------------------------------------
##       PARTE 2: Estadística Bayesiana: Naive Bayes
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 16. Bloque de carga de datos #####

bank=read.csv2("data/bank-full.csv")
##### datos extraidos de https://archive.ics.uci.edu/ml/datasets/bank+Marketing

## -------------------------------------------------------------------------

##### 17. Bloque de revisión basica del dataset #####

str(bank)
head(bank)
summary(bank)

## -------------------------------------------------------------------------

##### 18. Bloque de formateo de variables #####

bank$day=as.factor(bank$day)
bank$campaign=as.factor(bank$campaign)
bank$IndPrevio=as.factor(as.numeric(bank$pdays!=-1))

str(bank)
head(bank)
summary(bank)

## -------------------------------------------------------------------------

##### 19. Bloque de creación de conjuntos de entrenamiento y test #####

set.seed(1234) 
SAMPLE = sample.split(bank$y, SplitRatio = .75)
bankTrain = subset(bank, SAMPLE == TRUE)
bankTest = subset(bank, SAMPLE == FALSE)

## -------------------------------------------------------------------------

##### 20. Bloque de modelo Naive Bayes #####

modeloBayesTrain=naiveBayes(y~job+marital+education+default+balance+housing+loan+contact+month+poutcome, data=bankTrain,family=binomial(link="Bayes"))

modeloBayesTrain

## -------------------------------------------------------------------------

##### 21. Bloque de evaluación de los modelos #####

bankTrain$predDefecto <- predict(modeloBayesTrain,bankTrain)
bankTrain$prediccion <- predict(modeloBayesTrain,bankTrain,type="raw")[,2]
Predauxiliar= prediction(bankTrain$prediccion, bankTrain$y, label.ordering = NULL)
auc.tmp = performance(Predauxiliar, "auc");
aucModeloBayestrain = as.numeric(auc.tmp@y.values)
aucModeloBayestrain

CurvaRocModeloBayesTrain <- performance(Predauxiliar,"tpr","fpr")
plot(CurvaRocModeloBayesTrain,colorize=TRUE)
abline(a=0,b=1)

## Indice de GINI
GINItrain=2*aucModeloBayestrain-1


bankTest$predDefecto <- predict(modeloBayesTrain,bankTest)
bankTest$prediccion <- predict(modeloBayesTrain,bankTest,type="raw")[,2]
Predauxiliar = prediction(bankTest$prediccion, bankTest$y, label.ordering = NULL)
auc.tmp = performance(Predauxiliar, "auc");
aucModeloBayestest = as.numeric(auc.tmp@y.values)
aucModeloBayestest

CurvaRocModeloBayesTest <- performance(Predauxiliar,"tpr","fpr")
plot(CurvaRocModeloBayesTest,colorize=TRUE)
abline(a=0,b=1)

## Indice de GINI
GINItest=2*aucModeloBayestest-1

## Capacidad del Modelo
mean(as.numeric(bankTest$y)-1)
aggregate(bankTest$prediccion~bankTest$y,FUN=mean)

## -------------------------------------------------------------------------
##       PARTE 3: MODELO DE CLASIFICACIÓN: PUESTA EN VALOR DEL MODELO
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 22. Bloque de puesta en valor de un modelo: Fijación del Threshold #####

ALPHA=0.5
ConfusionTest=table(bankTest$y,bankTest$prediccion>=ALPHA)
AccuracyTest= (sum(bankTest$y=="yes" & bankTest$prediccion>=ALPHA)+sum(bankTest$y=="no" & bankTest$prediccion<ALPHA))/length(bankTest$y)
PrecisionTest=sum(bankTest$y=="yes" & bankTest$prediccion>=ALPHA)/sum(bankTest$prediccion>=ALPHA)
CoberturaTest=sum(bankTest$y=="yes" & bankTest$prediccion>=ALPHA)/sum(bankTest$y=="yes")
ConfusionTest
AccuracyTest
PrecisionTest
CoberturaTest

ALPHA=0.2
ConfusionTest=table(bankTest$y,bankTest$prediccion>=ALPHA)
AccuracyTest= (sum(bankTest$y=="yes" & bankTest$prediccion>=ALPHA)+sum(bankTest$y=="no" & bankTest$prediccion<ALPHA))/length(bankTest$y)
PrecisionTest=sum(bankTest$y=="yes" & bankTest$prediccion>=ALPHA)/sum(bankTest$prediccion>=ALPHA)
CoberturaTest=sum(bankTest$y=="yes" & bankTest$prediccion>=ALPHA)/sum(bankTest$y=="yes")
ConfusionTest
AccuracyTest
PrecisionTest
CoberturaTest

ALPHA=0.8
ConfusionTest=table(bankTest$y,bankTest$prediccion>=ALPHA)
AccuracyTest= (sum(bankTest$y=="yes" & bankTest$prediccion>=ALPHA)+sum(bankTest$y=="no" & bankTest$prediccion<ALPHA))/length(bankTest$y)
PrecisionTest=sum(bankTest$y=="yes" & bankTest$prediccion>=ALPHA)/sum(bankTest$prediccion>=ALPHA)
CoberturaTest=sum(bankTest$y=="yes" & bankTest$prediccion>=ALPHA)/sum(bankTest$y=="yes")
ConfusionTest
AccuracyTest
PrecisionTest
CoberturaTest

## -------------------------------------------------------------------------

##### 23. Bloque de puesta en valor de un modelo: KS y punto de máxima separación #####

bankKS=bankTest[order(bankTest$prediccion, decreasing=TRUE),c("y","prediccion")]
bankKS$N=1:length(bankKS$y)
bankKS$EXITOSACUM=cumsum(as.numeric(bankKS$y)-1)
bankKS$FRACASOSACUM=bankKS$N-bankKS$EXITOSACUM
bankKS$EXITOSTOT=sum(bankKS$y=="yes")
bankKS$FRACASOSTOT=sum(bankKS$y=="no")
bankKS$TOTAL=bankKS$EXITOSTOT+bankKS$FRACASOSTOT
bankKS$TPR=bankKS$EXITOSACUM/bankKS$EXITOSTOT
bankKS$FPR=bankKS$FRACASOSACUM/bankKS$FRACASOSTOT
bankKS$DIFF=bankKS$TPR-bankKS$FPR
plot(bankKS$DIFF)
max(bankKS$DIFF)
which(bankKS$DIFF==max(bankKS$DIFF))
bankKS[2764,]

plot(bankKS$prediccion*1000,1-bankKS$TPR,xlab="SCORE",ylab="Porcentaje acumulado",main="Distribuciones por Score (rojo malos, azul buenos)",type="l",col="blue")
lines(bankKS$prediccion*1000,1-bankKS$FPR,col="red")

## -------------------------------------------------------------------------

##### 24. Bloque de puesta en valor de un modelo: F1Score y punto óptimo estadístico #####

bankKS$Accuracy=(bankKS$EXITOSACUM+bankKS$FRACASOSTOT-bankKS$FRACASOSACUM)/bankKS$TOTAL
bankKS$Precision=bankKS$EXITOSACUM/bankKS$N
bankKS$Cobertura=bankKS$EXITOSACUM/bankKS$EXITOSTOT
bankKS$F1Score=2*(bankKS$Precision*bankKS$Cobertura)/(bankKS$Precision+bankKS$Cobertura)
plot(bankKS$F1Score)
max(bankKS$F1Score,na.rm=TRUE)
which(bankKS$F1Score==max(bankKS$F1Score,na.rm=TRUE))
bankKS[1475,]

ALPHAS=seq(0,1,0.05)
Accuracy=c()
Precision=c()
Cobertura=c()
F1Score=c()
for (i in 1:length(ALPHAS)){
  ALPHA=ALPHAS[i]
  Confusion=table(bankKS$y,bankKS$prediccion>=ALPHA)
  Accuracy=c(Accuracy,(sum(bankKS$y=="yes" & bankKS$prediccion>=ALPHA)+sum(bankKS$y=="no" & bankKS$prediccion<ALPHA))/length(bankKS$y))
  Precision=c(Precision,sum(bankKS$y=="yes" & bankKS$prediccion>=ALPHA)/sum(bankKS$prediccion>=ALPHA))
  Cobertura=c(Cobertura,sum(bankKS$y=="yes" & bankKS$prediccion>=ALPHA)/sum(bankKS$y=="yes"))
}
F1Score=2*(Precision*Cobertura)/(Precision+Cobertura)
DFF1=data.frame(ALPHAS,Accuracy,Precision,Cobertura,F1Score)

DFF1

## -------------------------------------------------------------------------

##### 25. Bloque de puesta en valor de un modelo: Beneficio y punto óptimo financiero #####

costeLlamada=10
beneficioVenta=100

bankKS$BeneficioTP=beneficioVenta-costeLlamada
bankKS$BeneficioTN=0
bankKS$PerdidaFP=-costeLlamada
bankKS$PerdidaFN=-beneficioVenta

bankKS$BeneficioFinan=bankKS$EXITOSACUM*bankKS$BeneficioTP+
  bankKS$FRACASOSACUM*bankKS$PerdidaFP

bankKS$Oportunidad=bankKS$EXITOSACUM*bankKS$BeneficioTP+
  (bankKS$EXITOSTOT-bankKS$EXITOSACUM)*bankKS$PerdidaFN+
  bankKS$FRACASOSACUM*bankKS$PerdidaFP+
  (bankKS$FRACASOSTOT-bankKS$FRACASOSACUM)*bankKS$BeneficioTN

plot(bankKS$BeneficioFinan)
max(bankKS$BeneficioFinan)
which(bankKS$BeneficioFinan==max(bankKS$BeneficioFinan))
bankKS[3646,]

plot(bankKS$Oportunidad)
max(bankKS$Oportunidad)
which(bankKS$Oportunidad==max(bankKS$Oportunidad))
bankKS[8755,]

## -------------------------------------------------------------------------