# Definción del UI
shinyUI(fluidPage(
  
# Titulo
titlePanel("Evolución de la esperanza de vida"),
  
sidebarPanel(width = 3,
		sliderInput("year", "Año:", value = 1992, min = 1952, max = 2007, step = 5, sep="", 
		            animate = TRUE)
	),
  
mainPanel(width = 9,
    tabsetPanel(id = "Menu",
      tabPanel("Tendencia",
      br(),
      column(10, plotOutput("tendencia")),
      column(2,downloadButton("guardarTendencia", "Guardar (.pdf)"))
    ),
    tabPanel("Datos",
      br(),
      fluidRow(column(2,offset=10,downloadButton("guardarTabla", "Guardar (.csv)"))),
      br(),
      fluidRow(column(12,dataTableOutput("datos")))
      )
    )
  )
))
