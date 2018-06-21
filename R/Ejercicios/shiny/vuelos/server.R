# Definición de la parte server

shinyServer(function(input, output) {
  
  # Son los datos para hacer el gráfico y la tabla
  
  datos <- reactive({
    year = input$year
    carrier = input$carrier
    subset(flights, Year %in% year & NameCarrier %in% carrier)
  })
  
  output$GraficoSalida <- renderPlot({ retraso_salida(datos()) })
  output$GraficoLlegada <- renderPlot({ retraso_llegada(datos()) })
  output$datos <- renderDataTable(datos(), options = list(pageLength = 10))
  
})