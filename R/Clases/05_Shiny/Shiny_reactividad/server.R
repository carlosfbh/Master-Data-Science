# Definición de la parte server
shinyServer(function(input, output) {
  
  datos <- reactive({
    gapminder %>%
      filter(year == input$year)
  }) 
  

  output$tendencia <- renderPlot({ 
    dispersion(datos(), label = input$year)
	})
	
  output$datos <-  renderDataTable(datos(),options = list(pageLength = 10))
  
  output$guardarTendencia <- downloadHandler(
    filename = function() { paste0("tendencia",Sys.Date(), '.pdf') },
    content = function(file) {
      p<-dispersion(datos(), label = input$year)
      ggsave(file,p,width=12,height=8)
    })		
  
  output$guardarTabla <- downloadHandler(
    filename = function() { paste0("datos_paro", '.csv') },
    content = function(file) {
      write_excel_csv(datos(),file)
    })	

})


