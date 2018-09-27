## Rmarkdown. Hacer un ejercicio por cuenta propia en plan ensayo.

## Shiny

ui <- fluidPage(titlePanel("Una pagina"))
server <- function(input, output){}

shinyApp(ui, server)


ui <- fluidPage(titlePanel("Una pagina"), sidebarPanel("Parametros"))
shinyApp(ui, server)
