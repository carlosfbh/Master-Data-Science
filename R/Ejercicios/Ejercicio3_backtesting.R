#####
# 3. Crea una función que haga el backtesting aceptando estrategias con 3 stocks.

# La estrategia a seguir sería la siguiente:
#   
# Si de un día para otro (día A y día A+1) la bolsa sube en los tres stocks 
# entonces se compra de aquel en el cual haya subido más.
# 
# Si hay empate se compra el que quieras (sólo 1)

## Solución

library(quantmod)
library(dplyr)

# Este es el código para descargarse valores reales de bolsa

amzn <- getSymbols('AMZN', from = "2014-01-01",auto.assign = F)
googl <- getSymbols('GOOGL', from = "2014-01-01",auto.assign = F)
aapl <- getSymbols('AAPL', from = "2014-01-01",auto.assign = F)
bolsa <- cbind(amzn, googl, aapl)


# Esto son ejemplos para ver si el código funciona
test_1 = c(1,3,5,7,9)
test_2 = c(1,2,5,6,7)
test_3 = c(1,2,3,6,7)


# Primero creo una función que me da un vector que me dice lo que compro ese día
# Si pone 1 entonces compro el primer stock, 2 el segundo, etc...
# Si pone 0 quiere decir que ese día no compro.

puntosDeCompra <- function(precioA, precioB, precioC) {
  
  resultado <- integer(length(precioA))
  
  for (i in 1:(length(precioA)-1)) {
    deltaA <- precioA[[i+1]] - precioA[[i]]
    deltaB <- precioB[[i+1]] - precioB[[i]]
    deltaC <- precioC[[i+1]] - precioC[[i]]
    
    if (deltaA > 0 && deltaB > 0 && deltaC > 0) {
      if (deltaA > deltaB && deltaA > deltaC) 
        resultado[i+1] <- 1
      else if (deltaB > deltaA && deltaB > deltaC)
        resultado[i+1] <- 2
      else
        resultado[i+1] <- 3
    }
  }
  resultado
}

# Segundo hago una función backtesting que me dará el beneficio total.
# El beneficio será el precio del último día del stock menos el promedio
# de las compras y luego multiplico por el total de compras que he hecho de ese stock

backtesting <- function(precioA, precioB, precioC, puntosDeCompra) {
  
  comprasA <- c()
  comprasB <- c()
  comprasC <- c()
  
  for (i in 1:length(precioA)) {
    if (puntosDeCompra[i] == 1)
      comprasA <- c(comprasA, precioA[[i]])
    else if (puntosDeCompra[i] == 2)
      comprasB <- c(comprasB, precioB[[i]])
    else if (puntosDeCompra[i] == 3)
      comprasC <- c(comprasC, precioC[[i]])
  }
  
  # Calculo el beneficio
  precioFinalA <- precioA[[length(precioA)]]
  precioFinalB <- precioB[[length(precioB)]]
  precioFinalC <- precioC[[length(precioC)]]
  
  (precioFinalA - mean(comprasA)) * length(comprasA) + 
    (precioFinalB - mean(comprasB)) * length(comprasB) + 
    (precioFinalC - mean(comprasC)) * length(comprasC)
}

backtesting(bolsa$AMZN.Open, bolsa$GOOGL.Open, bolsa$AAPL.Open,
            puntosDeCompra(bolsa$AMZN.Open, bolsa$GOOGL.Open, bolsa$AAPL.Open))


# Ahora voy a tratar de mejorar las funciones vectorizando

puntosDeCompra <- function(precioA, precioB, precioC) {
  
  deltaA <- diff(as.numeric(precioA)) # Al hacer la función diff el vector resultante
  deltaB <- diff(as.numeric(precioB)) # tiene la misma longitud menos 1
  deltaC <- diff(as.numeric(precioC))
  
  resultado <- (deltaA > 0 & deltaB > 0 & deltaC > 0) +
    (((deltaA > 0 & deltaB > 0 & deltaC > 0) * (deltaB > deltaA & deltaB > deltaC)) * 1) +
    (((deltaA > 0 & deltaB > 0 & deltaC > 0) * (deltaC > deltaA & deltaC > deltaB)) * 2)
  
  c(0, resultado) # Le pongo primero un 0 por la función diff (ver arriba)
}

backtesting <- function(precioA, precioB, precioC, puntosDeCompra) {

  comprasA <- precioA[puntosDeCompra == 1]
  comprasB <- precioB[puntosDeCompra == 2]
  comprasC <- precioC[puntosDeCompra == 3]
  
  # Calculo el beneficio
  precioFinalA <- precioA[[length(precioA)]]
  precioFinalB <- precioB[[length(precioB)]]
  precioFinalC <- precioC[[length(precioC)]]
  
  (precioFinalA - mean(comprasA)) * length(comprasA) + 
    (precioFinalB - mean(comprasB)) * length(comprasB) + 
    (precioFinalC - mean(comprasC)) * length(comprasC)
}

backtesting(bolsa$AMZN.Open, bolsa$GOOGL.Open, bolsa$AAPL.Open,
            puntosDeCompra(bolsa$AMZN.Open, bolsa$GOOGL.Open, bolsa$AAPL.Open))


