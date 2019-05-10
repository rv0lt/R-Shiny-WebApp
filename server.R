library(pxR)
library(shiny)
library(dplyr)
library(ggplot2)
data_1 <- read.px("http://www.ine.es/jaxiT3/files/t/es/px/6061.px?nocab=1") %>% as.data.frame()



shinyServer(
  
  function(input, output) {
    
      
      Filtrar <- reactive ({
        
        #busqueda contiene un dataFrame con todos los datos filtrados excepto el Periodo
        busqueda = filter(data_1, data_1$Comunidades.y.Ciudades.Autónomas %in% input$Comunidades &
                            data_1$Sectores.de.actividad.CNAE.2009 == input$Sectores &
                            data_1$Componentes.del.coste == input$Componentes ) 
        
        #Vector con todas las posiciones que contengan el año y trimestre desde el que comienzo a visualizar
        vectorPosInicio =  grep(Inicio(), busqueda$Periodo) 
        
        #Vector con todas las posiciones que contengan el año y trimestre desde el que Termino la visualización
        vectorPosFin = grep(Fin(), busqueda$Periodo)
        
        #Filtra en base al rango en Años y Trimestres desde el que se pretende visualizar 
        aux = slice(busqueda, vectorPosFin[1]:vectorPosInicio[1] )
        for(i in 1:length(vectorPosInicio)){
          aux2 = slice(busqueda, vectorPosFin[i]:vectorPosInicio[i] )
          aux = full_join(aux, aux2)
        }#for
        aux = select(aux, Periodo, Comunidades.y.Ciudades.Autónomas, value)
        aux
      })#Filtrar
      
      #Une el Año y trimestre inicial seleccionado
      Inicio <- reactive({
        paste(as.character(format(input$año[1],format="%Y")),input$TC, sep="")
      })
      #Une el año y trimestre final seleccionado
      Fin <- reactive({
        paste(as.character(format(input$año[2],format="%Y")),input$TF, sep="")
      })
      output$grafico <- renderPlot({
        query <- Filtrar()
        
        ggplot(query, aes(x =Periodo , y = value, group=1 ,colour=Comunidades.y.Ciudades.Autónomas )) + geom_line() + facet_grid(Comunidades.y.Ciudades.Autónomas ~ .)
        
      })#Grafica
      output$datos <-  renderTable({ Filtrar() 
        })# Filtrado de todos los datos
      
      output$varSel <- renderText({
        paste("Visualización del ", input$Componentes, "en el sector ", input$Sectores, "Desde el ", Inicio(), "hasta el ", Fin(), 
              "Para la/s Comunidades Autónomas seleccionadas")
      })
    
    
    
    
  }
)