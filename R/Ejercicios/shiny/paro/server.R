# Definición de la parte server

shinyServer(function(input, output) {
  
  # Son los datos para hacer el gráfico y la tabla
  
  datos <- reactive({
    year = input$year
    trimestres = input$trimestre
    sexo = input$sexo
    if (sexo == "Ambos" ) sexo = c("Mujeres", "Hombres")
    if (trimestres == "Todos") trimestres = c("I", "II", "III", "IV")
    subset(paro, Año %in% year & Sexo %in% sexo & Trimestre %in% trimestres)
  })
  
  output$grafico <- renderPlot({ tasa_paro(datos()) })
  
  output$datos <- renderDataTable(datos(), options = list(pageLength = 10))
  
  output$guardarGrafico <- downloadHandler(
    filename = function() { paste0("grafico", Sys.Date(), ".pdf") },
    content = function(file) {
      p <- tasa_paro(datos())
      ggsave(file, p, width = 40, height = 20, units = "cm")
    })
  
  output$guardarTabla <- downloadHandler(
    filename = function() { paste0("datos_paro", ".csv") },
    content = function(file) {
      write_excel_csv(datos(), file)
    })	
  
})