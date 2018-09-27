# install.packages("nycflights13")
library(nycflights13)

View(flights)
View(airports)

# Merge (inner join = sin fila extras o huérfanas)

cruce <- merge(flights, airports, by.x = "origin",
      by.y = "faa")


