# Definición del UI

shinyUI(fluidPage(
  
  titlePanel("Vuelos Estados Unidos"), # Título
  
  # Panel para escoger opciones
  sidebarPanel(width = 2,
               selectInput("year", "Año:", choices = c(2008), selected = 2008),
               checkboxGroupInput("carrier", "Compañía Aérea", 
                                  choices = list("AirTran Airways Corporation",
                                              "Alaska Airlines Inc.",
                                              "Aloha Airlines Inc.",
                                              "American Airlines Inc.",
                                              "Continental Air Lines Inc.",
                                              "Delta Air Lines Inc.",
                                              "Endeavor Air Inc.",
                                              "Envoy Air",
                                              "ExpressJet Airlines Inc.",
                                              "ExpressJet Airlines Inc. (1)",
                                              "Frontier Airlines Inc.",
                                              "Hawaiian Airlines Inc.",
                                              "JetBlue Airways",
                                              "Mesa Airlines Inc.",
                                              "Northwest Airlines Inc.",
                                              "PSA Airlines Inc.",
                                              "SkyWest Airlines Inc.",
                                              "Southwest Airlines Co.",
                                              "United Air Lines Inc.",
                                              "US Airways Inc."),
                                  selected = "AirTran Airways Corporation")
               ),
  
  # Panel donde se visualizan los gráficos / datos
  
  mainPanel(width = 10,
            tabsetPanel(id = "Menu",
                        tabPanel("Gráfico",
                                 br(),
                                 column(12, plotOutput("GraficoSalida")),
                                 br(),
                                 br(),
                                 column(12, plotOutput("GraficoLlegada"))
                        ),
                        tabPanel("Datos",
                                 br(),
                                 fluidRow(column(12, dataTableOutput("datos")))
                        )
            )
  )
                        
))

