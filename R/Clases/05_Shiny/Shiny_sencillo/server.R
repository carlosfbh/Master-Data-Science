# Definición de la parte server
shinyServer(function(input, output) {
  
  output$grafico <- renderPlot({
    ggplot(cars,aes(x=dist,y=speed))+
		geom_point(size=input$grosor,color="orange3",alpha=.3) +
		geom_smooth(method='lm')
  })
})
