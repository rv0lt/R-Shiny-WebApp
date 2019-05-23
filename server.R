library(pxR)
library(shiny)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
#http://www.xavigimenez.net/blog/2012/09/visualizing-data-with-r/
#https://gadm.org/download_country_v3.html

library(sp)
#Coste laboral por trabajador, comunidad aut?noma, sectores de actividad
data_1 <- read.px("http://www.ine.es/jaxiT3/files/t/es/px/6061.px?nocab=1", encoding = "latin1") %>% as.data.frame()


#Coste laboral por hora efectiva, comunidad aut?noma, sectores de actividad
data_2 <- read.px("http://www.ine.es/jaxiT3/files/t/es/px/6062.px?nocab=1", encoding = "latin1") %>% as.data.frame()

#N?mero de vacantes comunidad aut?noma
data_3 <- read.px("http://www.ine.es/jaxiT3/files/t/es/px/6064.px?nocab=1", encoding = "latin1") %>% as.data.frame()

#Motivos por los que no existen vacantes por comunidad aut?noma
data_4 <- read.px("http://www.ine.es/jaxiT3/files/t/es/px/6066.px?nocab=1", encoding = "latin1") %>% as.data.frame()


#spain = readRDS("gadm36_ESP_1_sp.rds")
spain <- readRDS(gzcon(url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_ESP_1_sp.rds")))


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
coloresAzules <- c('#F4F1A2','#F4F1A2','#E6EAA2','#E6EAA2',
             '#CFE3A2','#CFE3A2','#9AD0A3','#9AD0A3',
             '#7FC9A4','#7FC9A4','#32B9A3','#32B9A3',
             '#00A7A2','#00667E','#00667E','#1D4F73')

coloresRojos <- c('#FFF5F0','#FFF5F0','#FEE0D2','#FEE0D2',
              '#FCBBA1','#FCBBA1','#FC9272','#FC9272',
              '#FB6A4A','#FB6A4A','#EF3B2C','#EF3B2C',
              '#CB181D','#A50F15','#A50F15','#67000D') 

coloresVerdes <- c("#F7FCF5","#F7FCF5","#E5F5E0","#E5F5E0",
                   "#C7E9C0","#C7E9C0","#A1D99B","#A1D99B",
                   "#74C476","#74C476","#41AB5D","#41AB5D",
                   "#238B45","#006D2C","#006D2C","#00441B") 

         








shinyServer(
  
  function(input, output) {

    ######VARIABLES SELECCIONADAS########
    output$varSel_1 <- renderText({
      if(input$elecc_1=="mc"){
        paste("Visualización del mapa de calor del componente: ", input$Componentes_1, " en el sector:  ", input$Sectores_1, "y durante el año", 
              paste(as.character(format(input$añoMc_1,format="%Y")),input$Tmc_1, sep=" "))
      }#if
      else{
      paste("Visualización del componente: ", input$Componentes_1, " en el sector:  ", input$Sectores_1, "desde el ", Inicio_1(), "hasta el ", Fin_1(), 
            "Para la/s Comunidades Autónomas seleccionadas")
      }#else
    })#VarSel_1
    output$varSel_2 <- renderText({
      if(input$elecc_2=="mc"){
        paste("Visualización del mapa de calor del componente: ", input$Componentes_2, " en el sector:  ", input$Sectores_2, "y durante el año", 
              paste(as.character(format(input$añoMc_2,format="%Y")),input$Tmc_2, sep=" "))
      }#if
      else{
        paste("Visualización del componente: ", input$Componentes_2, " en el sector:  ", input$Sectores_2, "desde el ", Inicio_2(), "hasta el ", Fin_2(), 
              "Para la/s Comunidades Autónomas seleccionadas")
      }#else
    })#VarSel_2
    output$varSel_3 <- renderText({
      if(input$elecc_3=="mc"){
        paste("Visualización del mapa de calor durante el año: ",paste(as.character(format(input$añoMc_3,format="%Y")),input$Tmc_3, sep=" "))
      }#if
      else{
        paste("Visualización de los datos desde el ", Inicio_3(), "hasta el ", Fin_3(), 
              "Para la/s Comunidades Autónomas seleccionadas")
      }#else
    })#VarSel_3
    output$varSel_4 <- renderText({
      if(input$elecc_4=="mc"){
        paste("Visualización del mapa de calor del Motivo: ", input$Motivos_4, "Durante el año ", paste(as.character(format(input$añoMc_4,format="%Y")),input$Tmc_4, sep=" "))
      }#if
      else{
        paste("Visualización de los datos del Motivo: ", input$Motivos_4 ,"desde el ", Inicio_4(), "hasta el ", Fin_4(), 
              "Para la/s Comunidades Autónomas seleccionadas")
      }#else
    })#VarSel_4
    
    
    
    ####MÉTODOS QUE FILTRAN DE MANERA REACTIVA#########
    Filtrar_1 <- reactive ({
      
      #busqueda contiene un dataFrame con todos los datos filtrados excepto el Periodo
      busqueda = filter(data_1, data_1$Comunidades.y.Ciudades.Autónomas %in% input$Comunidades_1 &
                          data_1$Sectores.de.actividad.CNAE.2009 == input$Sectores_1 &
                          data_1$Componentes.del.coste == input$Componentes_1 ) 
      
      #Vector con todas las posiciones que contengan el año y trimestre desde el que comienzo a visualizar
      vectorPosInicio =  grep(Inicio_1(), busqueda$Periodo) 
      
      #Vector con todas las posiciones que contengan el año y trimestre desde el que Termino la visualización
      vectorPosFin = grep(Fin_1(), busqueda$Periodo)
      
      #Filtra en base al rango en Años y Trimestres desde el que se pretende visualizar 
      aux = slice(busqueda, vectorPosFin[1]:vectorPosInicio[1] )
      for(i in 1:length(vectorPosInicio)){
        aux2 = slice(busqueda, vectorPosFin[i]:vectorPosInicio[i] )
        aux = full_join(aux, aux2)
      }#for
      aux = select(aux, Periodo, Comunidades.y.Ciudades.Autónomas, value) %>% arrange(Periodo)
      aux
    })#Filtrar1
    
    Filtrar_2 <- reactive ({
      
      #busqueda contiene un dataFrame con todos los datos filtrados excepto el Periodo
      busqueda = filter(data_2, data_2$Comunidades.y.Ciudades.Autónomas %in% input$Comunidades_2 &
                          data_2$Sectores.de.actividad.CNAE.2009 == input$Sectores_2 &
                          data_2$Componentes.del.coste == input$Componentes_2 ) 
      
      #Vector con todas las posiciones que contengan el año y trimestre desde el que comienzo a visualizar
      vectorPosInicio =  grep(Inicio_2(), busqueda$Periodo) 
      
      #Vector con todas las posiciones que contengan el año y trimestre desde el que Termino la visualización
      vectorPosFin = grep(Fin_2(), busqueda$Periodo)
      
      #Filtra en base al rango en Años y Trimestres desde el que se pretende visualizar 
      aux = slice(busqueda, vectorPosFin[1]:vectorPosInicio[1] )
      for(i in 1:length(vectorPosInicio)){
        aux2 = slice(busqueda, vectorPosFin[i]:vectorPosInicio[i] )
        aux = full_join(aux, aux2)
      }#for
      aux = select(aux, Periodo, Comunidades.y.Ciudades.Autónomas, value) %>% arrange(Periodo)
      aux
    })#Filtrar_2
    
    Filtrar_3 <- reactive ({
      
      #busqueda contiene un dataFrame con todos los datos filtrados excepto el Periodo
      busqueda = filter(data_3, data_3$Comunidad.autónoma %in% input$Comunidades_3 ) 
      
      #Vector con todas las posiciones que contengan el año y trimestre desde el que comienzo a visualizar
      vectorPosInicio =  grep(Inicio_3(), busqueda$Periodo) 
      
      #Vector con todas las posiciones que contengan el año y trimestre desde el que Termino la visualización
      vectorPosFin = grep(Fin_3(), busqueda$Periodo)
      
      #Filtra en base al rango en Años y Trimestres desde el que se pretende visualizar 
      aux = slice(busqueda, vectorPosFin[1]:vectorPosInicio[1] )
      for(i in 1:length(vectorPosInicio)){
        aux2 = slice(busqueda, vectorPosFin[i]:vectorPosInicio[i] )
        aux = full_join(aux, aux2)
      }#for
      aux = select(aux, Periodo, Comunidad.autónoma, value) %>% arrange(Periodo)
      aux
    })#Filtrar_3
    Filtrar_4 <- reactive ({
      
      #busqueda contiene un dataFrame con todos los datos filtrados excepto el Periodo
      busqueda = filter(data_4, data_4$Comunidad.autónoma %in% input$Comunidades_4 &
                          data_4$Motivos.por.los.que.no.exiten.puestos.de.trabajo.vacantes == input$Motivos_4 ) 
      
      #Vector con todas las posiciones que contengan el año y trimestre desde el que comienzo a visualizar
      vectorPosInicio =  grep(Inicio_4(), busqueda$Periodo) 
      
      #Vector con todas las posiciones que contengan el año y trimestre desde el que Termino la visualización
      vectorPosFin = grep(Fin_4(), busqueda$Periodo)
      
      #Filtra en base al rango en Años y Trimestres desde el que se pretende visualizar 
      aux = slice(busqueda, vectorPosFin[1]:vectorPosInicio[1] )
      for(i in 1:length(vectorPosInicio)){
        aux2 = slice(busqueda, vectorPosFin[i]:vectorPosInicio[i] )
        aux = full_join(aux, aux2)
      }#for
      aux = select(aux, Periodo, Comunidad.autónoma, value) %>% arrange(Periodo)
      aux
    })#Filtrar_4
    #####FIN DE LOS METODOS DE FILTRAR#######
    
    
    
    #Une el Año y trimestre inicial seleccionado
    Inicio_1 <- reactive({
      
      paste(as.character(format(input$año_1[1],format="%Y")),input$TC_1, sep="")
    })
    Inicio_2 <- reactive({
      
      paste(as.character(format(input$año_2[1],format="%Y")),input$TC_2, sep="")
    })
    Inicio_3 <- reactive({
      
      paste(as.character(format(input$año_3[1],format="%Y")),input$TC_3, sep="")
    })
    Inicio_4 <- reactive({
      
      paste(as.character(format(input$año_4[1],format="%Y")),input$TC_4, sep="")
    })
    
    
    #Une el año y trimestre final seleccionado
    Fin_1 <- reactive({
      paste(as.character(format(input$año_1[2],format="%Y")),input$TF_1, sep="")
    })
    Fin_2 <- reactive({
      paste(as.character(format(input$año_2[2],format="%Y")),input$TF_2, sep="")
    })
    Fin_3 <- reactive({
      paste(as.character(format(input$año_3[2],format="%Y")),input$TF_3, sep="")
    })
    Fin_4 <- reactive({
      paste(as.character(format(input$año_4[2],format="%Y")),input$TF_4, sep="")
    })
    
    output$grafico1_1 <- renderPlot({
      query = Filtrar_1()
      ggplot(query, aes(x =Periodo , y = value, group=1, colour=Comunidades.y.Ciudades.Autónomas )) + geom_line() + facet_grid(Comunidades.y.Ciudades.Autónomas ~ .)
    })#Grafica1_1
    output$grafico1_2 <- renderPlot({
      query = Filtrar_2()
      ggplot(query, aes(x =Periodo , y = value, group=1, colour=Comunidades.y.Ciudades.Autónomas )) + geom_line() + facet_grid(Comunidades.y.Ciudades.Autónomas ~ .)
    })#Grafica1_2
    output$grafico1_3 <- renderPlot({
      query = Filtrar_3()
      ggplot(query, aes(x =Periodo , y = value, group=1, colour=Comunidad.autónoma )) + geom_line() + facet_grid(Comunidad.autónoma ~ .)
    })#Grafica1_3
    output$grafico1_4 <- renderPlot({
      query = Filtrar_4()
      ggplot(query, aes(x =Periodo , y = value, group=1, colour=Comunidad.autónoma )) + geom_line() + facet_grid(Comunidad.autónoma ~ .)
    })#Grafica1_4
    
    output$grafico2_1 <- renderPlot ({
      query = Filtrar_1()
      ggplot(query, aes(x=Periodo, y=value, fill=Comunidades.y.Ciudades.Autónomas)) + geom_bar(stat="identity", position=position_dodge())
    })#Grafico2_1
    output$grafico2_2 <- renderPlot ({
      query = Filtrar_2()
      ggplot(query, aes(x=Periodo, y=value, fill=Comunidades.y.Ciudades.Autónomas)) + geom_bar(stat="identity", position=position_dodge())
    })#Grafico2_2
    output$grafico2_3 <- renderPlot ({
      query = Filtrar_3()
      ggplot(query, aes(x=Periodo, y=value, fill=Comunidad.autónoma)) + geom_bar(stat="identity", position=position_dodge())
    })#Grafico2_3
    output$grafico2_4 <- renderPlot ({
      query = Filtrar_4()
      ggplot(query, aes(x=Periodo, y=value, fill=Comunidad.autónoma)) + geom_bar(stat="identity", position=position_dodge())
    })#Grafico2_4
    
    output$datos_1 <-  renderTable({ Filtrar_1() 
    })#Volvado de los datos
    output$datos_2 <-  renderTable({ Filtrar_2() 
    })#Volvado de los datos
    output$datos_3 <-  renderTable({ Filtrar_3() 
    })#Volvado de los datos
    output$datos_4 <-  renderTable({ Filtrar_4() 
    })#Volvado de los datos
    
  
      
    output$mapita_1 <- renderPlot({
      query = filter(data_1, Componentes.del.coste==input$Componentes_1, Sectores.de.actividad.CNAE.2009==input$Sectores_1, Periodo==paste(as.character(format(input$añoMc_1,format="%Y")),input$Tmc_1, sep=""), Comunidades.y.Ciudades.Autónomas!="Total Nacional" )
      aux = select(query,  value, Comunidades.y.Ciudades.Autónomas)
      
      colLevel <- vector(length = length(par))
      for(i in 1:length(colLevel)){
        l = par[[i]][2]
        if(l!="Ceuta y Melilla"){
          q = filter(aux, Comunidades.y.Ciudades.Autónomas==par[[i]][1])
          q = select(q, value)
          colLevel[i] = q
        }#if
      }
      y <- unlist(colLevel)
      y[7] = y[6]
      spain@data$mc=y
      spplot(spain, 'mc', col.regions=coloresRojos )
      
    })#representacion del mapa_1
    output$mapita_2 <- renderPlot({
      query = filter(data_2, Componentes.del.coste==input$Componentes_2, Sectores.de.actividad.CNAE.2009==input$Sectores_2, Periodo==paste(as.character(format(input$añoMc_2,format="%Y")),input$Tmc_2, sep=""), Comunidades.y.Ciudades.Autónomas!="Total Nacional" )
      aux = select(query,  value, Comunidades.y.Ciudades.Autónomas)
      
      colLevel <- vector(length = length(par))
      for(i in 1:length(colLevel)){
        l = par[[i]][2]
        if(l!="Ceuta y Melilla"){
          q = filter(aux, Comunidades.y.Ciudades.Autónomas==par[[i]][1])
          q = select(q, value)
          colLevel[i] = q
        }#if
      }
      y <- unlist(colLevel)
      y[7] = y[6]
      spain@data$mc=y
      spplot(spain, 'mc', col.regions=coloresRojos )
      
    })#representacion del mapa_2
    
    output$mapita_3 <- renderPlot({
      query = filter(data_3, Periodo==paste(as.character(format(input$añoMc_3,format="%Y")),input$Tmc_3, sep=""),Comunidad.autónoma!="Total Nacional" )
      aux = select(query,  value, Comunidad.autónoma)
      
      colLevel <- vector(length = length(par))
      for(i in 1:length(colLevel)){
        l = par[[i]][2]
        if(l!="Ceuta y Melilla"){
          q = filter(aux, Comunidad.autónoma==par[[i]][1])
          q = select(q, value)
          colLevel[i] = q
        }#if
      }
      y <- unlist(colLevel)
      y[7] = y[6]
      spain@data$mc=y
      spplot(spain, 'mc', col.regions=coloresRojos)
      
    })#representacion del mapa_3
    
    output$mapita_4 <- renderPlot({
      query = filter(data_4, Motivos.por.los.que.no.exiten.puestos.de.trabajo.vacantes == input$Motivos_4, Periodo==paste(as.character(format(input$añoMc_4,format="%Y")),input$Tmc_4, sep=""), Comunidad.autónoma!="Total Nacional" )
      aux = select(query,  value, Comunidad.autónoma)
      
      colLevel <- vector(length = length(par))
      for(i in 1:length(colLevel)){
        l = par[[i]][2]
        if(l!="Ceuta y Melilla"){
          q = filter(aux, Comunidad.autónoma==par[[i]][1])
          q = select(q, value)
          colLevel[i] = q
        }#if
      }
      y <- unlist(colLevel)
      y[7] = y[6]
      spain@data$mc=y
      spplot(spain, 'mc', col.regions=coloresRojos )
      
    })#representacion del mapa_2

  }#function(input,output)
)#ShinyServer
