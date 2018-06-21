require(ggplot2)
require(tidyverse)
require(ggmap)
library(shiny)

# Datos de vuelos
# flights <- readr::read_csv('C:/Users/carlo/Desktop/Máster/RStudio/Ejercicios/data/flights/2008.csv')
# airlines <- readr::read_csv('C:/Users/carlo/Desktop/Máster/RStudio/Ejercicios/data/airlines.csv')
# airports <- readr::read_csv('C:/Users/carlo/Desktop/Máster/RStudio/Ejercicios/data/airports.csv')
# flights <- inner_join(flights, airlines, by = c("UniqueCarrier" = "Code")) %>%
#  rename(NameCarrier = Description)

retraso_salida <- function(DF) {
  p <- DF %>% 
    filter(!is.na(DepDelay)) %>% 
    group_by(NameCarrier, Month) %>% 
    summarise(avg = mean(DepDelay > 0))
  
  ggplot(p) +
    aes(x = Month, y = avg, col = NameCarrier, group = NameCarrier) +
    geom_point(size = 1) + 
    geom_line(size = 1.5, alpha = 0.5) +
    scale_x_continuous(breaks = seq(0, 12, 1)) +
    scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent) + ylim(0, 0.7) +
    ggtitle("Vuelos retrasados a la salida") +
    labs(x = "Mes", y = "% Vuelos retrasados", color = "Compañía Aérea") +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10, face = "bold"),
          plot.title = element_text(size = 18, face = "bold"))
}

retraso_llegada <- function(DF) {
  p <- DF %>% 
    filter(!is.na(ArrDelay)) %>% 
    group_by(NameCarrier, Month) %>% 
    summarise(avg = mean(ArrDelay > 0))
  
  ggplot(p) +
    aes(x = Month, y = avg, col = NameCarrier, group = NameCarrier) +
    geom_point(size = 1) + 
    geom_line(size = 1.5, alpha = 0.5) +
    scale_x_continuous(breaks = seq(0, 12, 1)) +
    scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent) + ylim(0, 0.7) +
    ggtitle("Vuelos retrasados a la llegada") +
    labs(x = "Mes", y = "% Vuelos retrasados", color = "Compañía Aérea") +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10, face = "bold"),
          plot.title = element_text(size = 18, face = "bold"))
}
