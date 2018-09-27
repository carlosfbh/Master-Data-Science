# Definición de la parte server
shinyServer(function(input, output) {
  
  datos <- reactive({ 
						subset(gapminder,continent==input$continent)						
					}) 
  
  # color = input$color. Esto da error. No puedes gestionarlo fuera del output
  
  # Si queremos definir el color fuera de los outputs sería algo así
  # y luego llamamos en el output a colorin()
  colorin <- reactive({ input$color  })
  
  output$evolucion <- renderPlot({ 
    #color=isolate(input$color) # Con esto aislamos el color cuando lo cambiamos en la app
	ggplot(datos(),aes(x=year,y=lifeExp,group=country)) + 
		geom_line(stat="smooth",method="loess",alpha=.2,color=colorin())
	})
	
  output$bigotes <- renderPlot({ 
	#fill=input$color
	ggplot(datos(),aes(x=factor(year),y=lifeExp)) + 
		geom_boxplot(fill=colorin())
	}) 	
})


