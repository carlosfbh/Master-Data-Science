require(ggplot2)
require(gapminder)
require(tidyverse)

dispersion <- function(DF, label = "1992") {
  ggplot(DF, aes(x = gdpPercap, y = lifeExp, col = continent, size = pop)) +
    geom_text(x = 22000, y = 50, label = label, size = 40, color = "grey50") +
    geom_point() + ylim(20, 85) + xlim(0, 45000) +
    theme_bw()
}
