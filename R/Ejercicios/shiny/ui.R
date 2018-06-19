# Definición del UI

shinyUI(fluidPage(
  
  titlePanel("Tasa del Paro en España"), # Título de la aplicación
  
  # Panel para escoger opciones
  sidebarPanel(width = 3,
               selectInput("year", "Año:", choices = c(2011, 2012, 2013, 2014), selected = 2011),
               selectInput("sexo", "Sexo:", choices = c("Ambos", "Mujeres", "Hombres"), selected = "Ambos"),
               selectInput("trimestre", "Trimestre:", choices = c("Todos", "I", "II", "III", "IV"), selected = "Todos")
               ),
  
  # Panel donde se visualizan los gráficos / datos
  mainPanel(width = 9,
            tabsetPanel(id = "Menu",
                        tabPanel("Gráfico",
                                 br(),
                                 column(10, plotOutput("grafico")),
                                 column(2, downloadButton("guardarGrafico", "Guardar (.pdf)"))
                                 ),
                        tabPanel("Datos",
                                 br(),
                                 fluidRow(column(2, offset = 10, downloadButton("guardarTabla", "Guardar (.csv)"))),
                                 br(),
                                 fluidRow(column(12, dataTableOutput("datos")))
                                 )
                        )
            )
))
