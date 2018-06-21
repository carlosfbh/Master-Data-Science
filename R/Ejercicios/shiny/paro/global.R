require(ggplot2)
require(tidyverse)
require(ggmap)

# Datos del paro
paro <- read.csv("data/paro.csv", encoding = "UTF-8")

# Datos para dibujar la península
shape <- getData("GADM", country= "Spain", level = 2)
peninsula <- subset(shape, !NAME_1 == "Islas Canarias")


tasa_paro <- function(DF) {
  
  peninsula.df <- fortify(peninsula, region="CCA_2")
  
  peninsula.df <- peninsula.df %>% 
    mutate(id = as.numeric(id))
  
  peninsula_paro = inner_join(peninsula.df, DF, by = c("id" = "Prov.id"))
  
  ggplot(peninsula_paro) + 
    aes(x = long, y = lat, group = group, fill = Tasa.paro) + 
    geom_polygon(colour = "grey80", size = 0.1) + 
    facet_grid(Sexo ~ Trimestre) + 
    scale_fill_gradient(low = "aliceblue", high = "steelblue4") +
    coord_quickmap() + 
    theme_bw()
}