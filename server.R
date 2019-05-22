library(pxR)
library(shiny)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
#http://www.xavigimenez.net/blog/2012/09/visualizing-data-with-r/
#https://gadm.org/download_country_v3.html

library(sp)
data_1 <- read.px("http://www.ine.es/jaxiT3/files/t/es/px/6061.px?nocab=1", encoding = "latin1") %>% as.data.frame()

spain = readRDS("gadm36_ESP_1_sp.rds")

par <- list(
  c("01 Andalucía","Andalucía"),
  c("02 Aragón", "Aragón"),
  c("06 Cantabria","Cantabria"),
  c("08 Castilla - La Mancha", "Castilla-La Mancha"),
  c("07 Castilla y León","Castilla y León"),
  c("09 Cataluña","Cataluña"),
  c("nop","Ceuta y Melilla"),
  c("13 Madrid, Comunidad de", "Comunidad de Madrid"), 
  c("15 Navarra, Comunidad Foral de", "Comunidad Foral de Navarra"), 
  c("10 Comunitat Valenciana", "Comunidad Valenciana"),
  c("11 Extremadura","Extremadura"),
  c("12 Galicia","Galicia"),
  c("04 Balears, Illes", "Islas Baleares"), 
  c("05 Canarias", "Islas Canarias"), 
  c("17 Rioja, La", "La Rioja"),
  c("16 País Vasco","País Vasco"),
  c("03 Asturias, Principado de", "Principado de Asturias"), 
  c("14 Murcia, Región de", "Región de Murcia")
)

paleta <- brewer.pal(9, "Reds")[1:9]
colores <- c('#F4F1A2','#F4F1A2','#E6EAA2','#E6EAA2',
             '#CFE3A2','#CFE3A2','#9AD0A3','#9AD0A3',
             '#7FC9A4','#7FC9A4','#32B9A3','#32B9A3',
             '#00A7A2','#00667E','#00667E','#1D4F73')

colores2 <- c('#FFF5F0','#FFF5F0','#FEE0D2','#FEE0D2',
              '#FCBBA1','#FCBBA1','#FC9272','#FC9272',
              '#FB6A4A','#FB6A4A','#EF3B2C','#EF3B2C',
              '#CB181D','#A50F15','#A50F15','#67000D') 









shinyServer(
  
  function(input, output) {
    
    #Variables seleccionadas
    output$varSel <- renderText({
      if(input$elecc=="mc"){
        paste("Visualización del mapa de calor del componente: ", input$Componentes, " en el sector:  ", input$Sectores, "y durante el año", 
              paste(as.character(format(input$añoMc,format="%Y")),input$Tmc, sep=" "))
      }#if
      else{
      paste("Visualización del componente: ", input$Componentes, " en el sector:  ", input$Sectores, "desde el ", Inicio(), "hasta el ", Fin(), 
            "Para la/s Comunidades Autónomas seleccionadas")
      }#else
    })
    
    
    
    ####MÉTODO QUE FILTRA DE MANERA REACTIVA#########
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
      aux = select(aux, Periodo, Comunidades.y.Ciudades.Autónomas, value) %>% arrange(Periodo)
      aux
    })#####FIN DE FILTRAR#######
    
    
    #Une el Año y trimestre inicial seleccionado
    Inicio <- reactive({
      
      paste(as.character(format(input$año[1],format="%Y")),input$TC, sep="")
    })
    #Une el año y trimestre final seleccionado
    Fin <- reactive({
      paste(as.character(format(input$año[2],format="%Y")),input$TF, sep="")
    })
    
    
    output$grafico1 <- renderPlot({
      query = Filtrar()
      ggplot(query, aes(x =Periodo , y = value, group=1, colour=Comunidades.y.Ciudades.Autónomas )) + geom_line() + facet_grid(Comunidades.y.Ciudades.Autónomas ~ .)
    })#Grafica1
    output$grafico2 <- renderPlot ({
      query = Filtrar()
      ggplot(query, aes(x=Periodo, y=value, fill=Comunidades.y.Ciudades.Autónomas)) + geom_bar(stat="identity", position=position_dodge())
    })#Grafico2
    output$datos <-  renderTable({ Filtrar() 
    })#Volvado de los datos
    
    output$mapita <- renderPlot({
      query = filter(data_1, Componentes.del.coste==input$Componentes, Sectores.de.actividad.CNAE.2009==input$Sectores, Periodo==paste(as.character(format(input$añoMc,format="%Y")),input$Tmc, sep=""), Comunidades.y.Ciudades.Autónomas!="Total Nacional" )
      aux = select(query,  value, Comunidades.y.Ciudades.Autónomas)
      
      colLevel <- vector(length = length(par))
      for(i in 1:length(colLevel)){
        l = par[[i]][2]
        if(l!="Ceuta y Melilla"){
          q = filter(aux, Comunidades.y.Ciudades.Autónomas==par[[i]][1])
          q = select(q, value)
          #z = (q %/% 10) +1
          colLevel[i] = q
        }#if
      }
      #y <- as.factor(unlist(colLevel))
      y <- unlist(colLevel)
      y[7] = y[6]
      spain@data$mc=y
      spplot(spain, 'mc', col.regions=colores2 )
      
    })#representacion del mapa
    
    output$prueba <- renderText({
    })
      
    
    
  }
)
