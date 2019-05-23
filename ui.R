
library(shiny)
library(dplyr)
library(pxR)
library(ggplot2)
# 
library(RColorBrewer)
#http://www.xavigimenez.net/blog/2012/09/visualizing-data-with-r/
#https://gadm.org/download_country_v3.html
library(sp)


dateInput2 <- function(inputId, label, minview = "years", maxview = "decades", ...) {
  d <- shiny::dateInput(inputId, label, ...)
  d$children[[2L]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$attribs[["data-date-max-view-mode"]] <- maxview
  d
}

dateRangeInput2 <- function(inputId, label, minview = "years", maxview = "decades", ...) {
  d <- shiny::dateRangeInput(inputId, label, ...)
  d$children[[2L]]$children[[1]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$children[[3]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$children[[1]]$attribs[["data-date-max-view-mode"]] <- maxview
  d$children[[2L]]$children[[3]]$attribs[["data-date-max-view-mode"]] <- maxview
  d
}
# Define UI for the shiny application here
shinyUI(fluidPage(
  headerPanel("Aplicación Web Shiny para visualizar distintas variables relacionadas con el Mercado Laboral y, en concreto, con el Coste Laboral"),
  tabsetPanel(id="data",
              
              tabPanel('Coste laboral(en euros) por trabajador, comunidad autónoma, sectores de actividad', value="1",
                       sidebarPanel("Seleccione",
                                    radioButtons(
                                      'elecc_1', label='Eliga que desea visualizar', choices = c("Gráficas"= "gr", "Datos"= "dt", "Mapa de Calor" = "mc"),
                                      selected="gr"
                                    ),#elección de Gráfica, Datos o mapa de calor
                                    conditionalPanel(condition="input.elecc_1 != 'mc'",
                                                     selectInput('Comunidades_1', 
                                                                 label = 'Comunidades autonómas que desea consultar (Puede seleccionar varias)', 
                                                                 choices = c("Total Nacional"="Total Nacional",
                                                                             "Andalucía"="01 Andalucía",
                                                                             "Aragon" = "02 Aragón",
                                                                             "Asturias" = "03 Asturias, Principado de",
                                                                             "Islas Baleares" = "04 Balears, Illes",
                                                                             "Islas canarias" = "05 Canarias",
                                                                             "Cantabria" = "06 Cantabria",
                                                                             "Castilla y León" = "07 Castilla y León",
                                                                             "Castilla La Mancha" = "08 Castilla - La Mancha",
                                                                             "Cataluña" = "09 Cataluña",
                                                                             "Valencia" = "10 Comunitat Valenciana",
                                                                             "Extremadura" = "11 Extremadura",
                                                                             "Galicia" = "12 Galicia",
                                                                             "Madrid" = "13 Madrid, Comunidad de",
                                                                             "Murcia" = "14 Murcia, Región de",
                                                                             "Navarra" = "15 Navarra, Comunidad Foral de",
                                                                             "País Vasco" = "16 País Vasco",
                                                                             "La Rioja" = "17 Rioja, La"
                                                                 ),#choices
                                                                 selected = "Total Nacional",
                                                                 multiple = TRUE
                                                     ),#COMUNIDADES_SelectInput
                                                     p("Selección del Periodo en el que quiere generar la gráfica. Las fechas están divididas en años y trimestres"),
                                                     dateRangeInput2("año_1", "Año", 
                                                                     startview = "year",
                                                                     min = "2008-01-01", max = "2018-01-01",
                                                                     minview = "years", maxview = "decades", 
                                                                     format ="yyyy" , start = "2008-01-01",
                                                                     
                                                                     end ="2009-01-01"
                                                     ),#AÑO_RangeInput
                                                     selectInput('TC_1',
                                                                 label = 'Trimeste Inicial',
                                                                 choices = c("Primer Trimestre" ="T1",
                                                                             "Segundo Trimestre" = "T2",
                                                                             "Tercer Trimestre" ="T3",
                                                                             "Cuarto Trimestre" = "T4"
                                                                 )#choices
                                                     ),#TC_SelectInput
                                                     selectInput('TF_1',
                                                                 label = 'Trimeste Final',
                                                                 choices = c("Primer Trimestre" ="T1",
                                                                             "Segundo Trimestre" = "T2",
                                                                             "Tercer Trimestre" ="T3",
                                                                             "Cuarto Trimestre" = "T4"
                                                                 )#choices
                                                     )#TF_SelectInput
                                    ),#ConditionalPanel != mc
                                    selectInput('Sectores_1',
                                                label = 'Sector que desea consultar',
                                                choices = c("Industria" = "Industria",
                                                            "Servicios" = "Servicios",
                                                            "Construcción" = "Construcción",
                                                            "B_S Industria, construcción y servicios (excepto actividades de los hogares como empleadores y de organizaciones y organismos extraterritoriales)" = "B_S Industria, construcción y servicios (excepto actividades de los hogares como empleadores y de organizaciones y organismos extraterritoriales)"
                                                )#choices
                                    ),#SECTORES_SelectInput
                                    selectInput('Componentes_1',
                                                label = 'Componentes del Coste',
                                                choices = c("Coste laboral total" ="Coste laboral total",
                                                            "Coste salarial total" = "Coste salarial total",
                                                            "Coste salarial ordinario" ="Coste salarial ordinario",
                                                            "Otros costes" = "Otros costes",
                                                            "Coste por percepciones no salariales" ="Coste por percepciones no salariales",
                                                            "Coste por cotizaciones obligatorias" = "Coste por cotizaciones obligatorias",
                                                            "Subvenciones y bonificaciones de la S.Social" = "Subvenciones y bonificaciones de la S.Social"
                                                )#choices
                                    ),#COMPONENTES_SelectInput
                                    
                                    conditionalPanel(condition="input.elecc_1 == 'mc'",
                                                     dateInput2('añoMc_1', "Año que desea representar", 
                                                                startview = "year",
                                                                min = "2008-01-01", max = "2018-01-01",
                                                                minview = "years", maxview = "decades", 
                                                                format ="yyyy", value="2008-01-01" ),#dateInput
                                                     
                                                     selectInput('Tmc_1',
                                                                 label = 'Trimeste',
                                                                 choices = c("Primer Trimestre" ="T1",
                                                                             "Segundo Trimestre" = "T2",
                                                                             "Tercer Trimestre" ="T3",
                                                                             "Cuarto Trimestre" = "T4"
                                                                 )#choices
                                                     )#selectInput
                                    )#conditionalPanel ==mc
                                    
                       ),#SidebarPanel
  mainPanel(strong("Datos extraídos del INE"),
            p(textOutput('varSel_1')),
            conditionalPanel(condition="input.elecc_1 == 'gr'",
                             plotOutput('grafico1_1'),
                             plotOutput('grafico2_1') ),
            conditionalPanel(condition="input.elecc_1 == 'dt'",
                             tableOutput('datos_1') ),
            conditionalPanel(condition="input.elecc_1 == 'mc'",
                             plotOutput('mapita_1'))
            
  )#mainPanel

                      
              ),#tabPanel_1
  tabPanel('Coste laboral(en euros) por hora efectiva, comunidad autónoma, sectores de actividad', value="2",
           sidebarPanel("Seleccione",
                        radioButtons(
                          'elecc_2', label='Eliga que desea visualizar', choices = c("Gráficas"= "gr", "Datos"= "dt", "Mapa de Calor" = "mc"),
                          selected="gr"
                        ),#elección de Gráfica, Datos o mapa de calor
                        conditionalPanel(condition="input.elecc_2 != 'mc'",
                                         selectInput('Comunidades_2', 
                                                     label = 'Comunidades autonómas que desea consultar (Puede seleccionar varias)', 
                                                     choices = c("Total Nacional"="Total Nacional",
                                                                 "Andalucía"="01 Andalucía",
                                                                 "Aragon" = "02 Aragón",
                                                                 "Asturias" = "03 Asturias, Principado de",
                                                                 "Islas Baleares" = "04 Balears, Illes",
                                                                 "Islas canarias" = "05 Canarias",
                                                                 "Cantabria" = "06 Cantabria",
                                                                 "Castilla y León" = "07 Castilla y León",
                                                                 "Castilla La Mancha" = "08 Castilla - La Mancha",
                                                                 "Cataluña" = "09 Cataluña",
                                                                 "Valencia" = "10 Comunitat Valenciana",
                                                                 "Extremadura" = "11 Extremadura",
                                                                 "Galicia" = "12 Galicia",
                                                                 "Madrid" = "13 Madrid, Comunidad de",
                                                                 "Murcia" = "14 Murcia, Región de",
                                                                 "Navarra" = "15 Navarra, Comunidad Foral de",
                                                                 "País Vasco" = "16 País Vasco",
                                                                 "La Rioja" = "17 Rioja, La"
                                                     ),#choices
                                                     selected = "Total Nacional",
                                                     multiple = TRUE
                                         ),#COMUNIDADES_SelectInput
                                         p("Selección del Periodo en el que quiere generar la gráfica. Las fechas están divididas en años y trimestres"),
                                         dateRangeInput2("año_2", "Año", 
                                                         startview = "year",
                                                         min = "2008-01-01", max = "2018-01-01",
                                                         minview = "years", maxview = "decades", 
                                                         format ="yyyy" , start = "2008-01-01",
                                                         
                                                         end ="2009-01-01"
                                         ),#AÑO_RangeInput
                                         selectInput('TC_2',
                                                     label = 'Trimeste Inicial',
                                                     choices = c("Primer Trimestre" ="T1",
                                                                 "Segundo Trimestre" = "T2",
                                                                 "Tercer Trimestre" ="T3",
                                                                 "Cuarto Trimestre" = "T4"
                                                     )#choices
                                         ),#TC_SelectInput
                                         selectInput('TF_2',
                                                     label = 'Trimeste Final',
                                                     choices = c("Primer Trimestre" ="T1",
                                                                 "Segundo Trimestre" = "T2",
                                                                 "Tercer Trimestre" ="T3",
                                                                 "Cuarto Trimestre" = "T4"
                                                     )#choices
                                         )#TF_SelectInput
                        ),#ConditionalPanel != mc
                        selectInput('Sectores_2',
                                    label = 'Sector que desea consultar',
                                    choices = c("Industria" = "Industria",
                                                "Servicios" = "Servicios",
                                                "Construcción" = "Construcción",
                                                "B_S Industria, construcción y servicios (excepto actividades de los hogares como empleadores y de organizaciones y organismos extraterritoriales)" = "B_S Industria, construcción y servicios (excepto actividades de los hogares como empleadores y de organizaciones y organismos extraterritoriales)"
                                    )#choices
                        ),#SECTORES_SelectInput
                        selectInput('Componentes_2',
                                    label = 'Componentes del Coste',
                                    choices = c("Coste laboral total por hora" ="Coste laboral total por hora",
                                                "Coste salarial total por hora" = "Coste salarial total por hora",
                                                "Coste salarial ordinario por hora" ="Coste salarial ordinario por hora",
                                                "Otros costes por hora" = "Otros costes por hora",
                                                "Coste por percepciones no salariales por hora" ="Coste por percepciones no salariales por hora",
                                                "Coste por cotizaciones obligatorias por hora" = "Coste por cotizaciones obligatorias por hora",
                                                "Subvenciones y bonificaciones de la S.Social por hora" = "Subvenciones y bonificaciones de la S.Social por hora"
                                    )#choices
                        ),#COMPONENTES_SelectInput
                        
                        conditionalPanel(condition="input.elecc_2 == 'mc'",
                                         dateInput2('añoMc_2', "Año que desea representar", 
                                                    startview = "year",
                                                    min = "2008-01-01", max = "2018-01-01",
                                                    minview = "years", maxview = "decades", 
                                                    format ="yyyy", value="2008-01-01" ),#dateInput
                                         
                                         selectInput('Tmc_2',
                                                     label = 'Trimeste',
                                                     choices = c("Primer Trimestre" ="T1",
                                                                 "Segundo Trimestre" = "T2",
                                                                 "Tercer Trimestre" ="T3",
                                                                 "Cuarto Trimestre" = "T4"
                                                     )#choices
                                         )#selectInput
                        )#conditionalPanel ==mc
                        
           ),#SidebarPanel
           mainPanel(strong("Datos extraídos del INE"),
                     p(textOutput('varSel_2')),
                     conditionalPanel(condition="input.elecc_2 == 'gr'",
                                      plotOutput('grafico1_2'),
                                      plotOutput('grafico2_2') ),
                     conditionalPanel(condition="input.elecc_2 == 'dt'",
                                      tableOutput('datos_2') ),
                     conditionalPanel(condition="input.elecc_2 == 'mc'",
                                      plotOutput('mapita_2'))
                     
           )#mainPanel
  ),#tabPanel_2
  
  tabPanel('Número de vacantes comunidad autónoma', value="3",
           sidebarPanel("Seleccione",
                        radioButtons(
                          'elecc_3', label='Eliga que desea visualizar', choices = c("Gráficas"= "gr", "Datos"= "dt", "Mapa de Calor" = "mc"),
                          selected="gr"
                        ),#elección de Gráfica, Datos o mapa de calor
                        conditionalPanel(condition="input.elecc_3 != 'mc'",
                                         selectInput('Comunidades_3', 
                                                     label = 'Comunidades autonómas que desea consultar (Puede seleccionar varias)', 
                                                     choices = c("Total Nacional"="Total Nacional",
                                                                 "Andalucía"="01 Andalucía",
                                                                 "Aragon" = "02 Aragón",
                                                                 "Asturias" = "03 Asturias, Principado de",
                                                                 "Islas Baleares" = "04 Balears, Illes",
                                                                 "Islas canarias" = "05 Canarias",
                                                                 "Cantabria" = "06 Cantabria",
                                                                 "Castilla y León" = "07 Castilla y León",
                                                                 "Castilla La Mancha" = "08 Castilla - La Mancha",
                                                                 "Cataluña" = "09 Cataluña",
                                                                 "Valencia" = "10 Comunitat Valenciana",
                                                                 "Extremadura" = "11 Extremadura",
                                                                 "Galicia" = "12 Galicia",
                                                                 "Madrid" = "13 Madrid, Comunidad de",
                                                                 "Murcia" = "14 Murcia, Región de",
                                                                 "Navarra" = "15 Navarra, Comunidad Foral de",
                                                                 "País Vasco" = "16 País Vasco",
                                                                 "La Rioja" = "17 Rioja, La"
                                                     ),#choices
                                                     selected = "Total Nacional",
                                                     multiple = TRUE
                                         ),#COMUNIDADES_SelectInput
                                         p("Selección del Periodo en el que quiere generar la gráfica. Las fechas están divididas en años y trimestres"),
                                         dateRangeInput2("año_3", "Año", 
                                                         startview = "year",
                                                         min = "2014-01-01", max = "2018-01-01",
                                                         minview = "years", maxview = "decades", 
                                                         format ="yyyy" , start = "2014-01-01",
                                                         
                                                         end ="2015-01-01"
                                         ),#AÑO_RangeInput
                                         selectInput('TC_3',
                                                     label = 'Trimeste Inicial',
                                                     choices = c("Primer Trimestre" ="T1",
                                                                 "Segundo Trimestre" = "T2",
                                                                 "Tercer Trimestre" ="T3",
                                                                 "Cuarto Trimestre" = "T4"
                                                     )#choices
                                         ),#TC_SelectInput
                                         selectInput('TF_3',
                                                     label = 'Trimeste Final',
                                                     choices = c("Primer Trimestre" ="T1",
                                                                 "Segundo Trimestre" = "T2",
                                                                 "Tercer Trimestre" ="T3",
                                                                 "Cuarto Trimestre" = "T4"
                                                     )#choices
                                         )#TF_SelectInput
                        ),#ConditionalPanel != mc
                        
                        
                        conditionalPanel(condition="input.elecc_3 == 'mc'",
                                         dateInput2('añoMc_3', "Año que desea representar", 
                                                    startview = "year",
                                                    min = "2014-01-01", max = "2018-01-01",
                                                    minview = "years", maxview = "decades", 
                                                    format ="yyyy", value="2014-01-01" ),#dateInput
                                         
                                         selectInput('Tmc_3',
                                                     label = 'Trimeste',
                                                     choices = c("Primer Trimestre" ="T1",
                                                                 "Segundo Trimestre" = "T2",
                                                                 "Tercer Trimestre" ="T3",
                                                                 "Cuarto Trimestre" = "T4"
                                                     )#choices
                                         )#selectInput
                        )#conditionalPanel ==mc
                        
           ),#SidebarPanel
           mainPanel(strong("Datos extraídos del INE"),
                     p(textOutput('varSel_3')),
                     conditionalPanel(condition="input.elecc_3 == 'gr'",
                                      plotOutput('grafico1_3'),
                                      plotOutput('grafico2_3') ),
                     conditionalPanel(condition="input.elecc_3 == 'dt'",
                                      tableOutput('datos_3') ),
                     conditionalPanel(condition="input.elecc_3 == 'mc'",
                                      plotOutput('mapita_3'))
                     
           )#mainPanel
  ),#tabPanel_3
  
  tabPanel('Motivos por los que no existen vacantes por comunidad autónoma', value="4",
           sidebarPanel("Seleccione",
                        radioButtons(
                          'elecc_4', label='Eliga que desea visualizar', choices = c("Gráficas"= "gr", "Datos"= "dt", "Mapa de Calor" = "mc"),
                          selected="gr"
                        ),#elección de Gráfica, Datos o mapa de calor
                        conditionalPanel(condition="input.elecc_4 != 'mc'",
                                         selectInput('Comunidades_4', 
                                                     label = 'Comunidades autonómas que desea consultar (Puede seleccionar varias)', 
                                                     choices = c("Total Nacional"="Total Nacional",
                                                                 "Andalucía"="01 Andalucía",
                                                                 "Aragon" = "02 Aragón",
                                                                 "Asturias" = "03 Asturias, Principado de",
                                                                 "Islas Baleares" = "04 Balears, Illes",
                                                                 "Islas canarias" = "05 Canarias",
                                                                 "Cantabria" = "06 Cantabria",
                                                                 "Castilla y León" = "07 Castilla y León",
                                                                 "Castilla La Mancha" = "08 Castilla - La Mancha",
                                                                 "Cataluña" = "09 Cataluña",
                                                                 "Valencia" = "10 Comunitat Valenciana",
                                                                 "Extremadura" = "11 Extremadura",
                                                                 "Galicia" = "12 Galicia",
                                                                 "Madrid" = "13 Madrid, Comunidad de",
                                                                 "Murcia" = "14 Murcia, Región de",
                                                                 "Navarra" = "15 Navarra, Comunidad Foral de",
                                                                 "País Vasco" = "16 País Vasco",
                                                                 "La Rioja" = "17 Rioja, La"
                                                     ),#choices
                                                     selected = "Total Nacional",
                                                     multiple = TRUE
                                         ),#COMUNIDADES_SelectInput
                                         p("Selección del Periodo en el que quiere generar la gráfica. Las fechas están divididas en años y trimestres"),
                                         dateRangeInput2("año_4", "Año", 
                                                         startview = "year",
                                                         min = "2014-01-01", max = "2018-01-01",
                                                         minview = "years", maxview = "decades", 
                                                         format ="yyyy" , start = "2014-01-01",
                                                         
                                                         end ="2015-01-01"
                                         ),#AÑO_RangeInput
                                         selectInput('TC_4',
                                                     label = 'Trimeste Inicial',
                                                     choices = c("Primer Trimestre" ="T1",
                                                                 "Segundo Trimestre" = "T2",
                                                                 "Tercer Trimestre" ="T3",
                                                                 "Cuarto Trimestre" = "T4"
                                                     )#choices
                                         ),#TC_SelectInput
                                         selectInput('TF_4',
                                                     label = 'Trimeste Final',
                                                     choices = c("Primer Trimestre" ="T1",
                                                                 "Segundo Trimestre" = "T2",
                                                                 "Tercer Trimestre" ="T3",
                                                                 "Cuarto Trimestre" = "T4"
                                                     )#choices
                                         )#TF_SelectInput
                        ),#ConditionalPanel != mc
                        
                        selectInput('Motivos_4',
                                    label = 'Motivos de las vacantes',
                                    choices = c("No se necesita ningún trabajador" ="No se necesita ningún trabajador",
                                                "Elevado coste de contratación" = "Elevado coste de contratación",
                                                "Otros" ="Otros",
                                                "Total" = "Total"
                                    )#choices
                        ),#MOTIVOS_selectInput
                        
                        conditionalPanel(condition="input.elecc_4 == 'mc'",
                                         dateInput2('añoMc_4', "Año que desea representar", 
                                                    startview = "year",
                                                    min = "2014-01-01", max = "2018-01-01",
                                                    minview = "years", maxview = "decades", 
                                                    format ="yyyy", value="2014-01-01" ),#dateInput
                                         
                                         selectInput('Tmc_4',
                                                     label = 'Trimeste',
                                                     choices = c("Primer Trimestre" ="T1",
                                                                 "Segundo Trimestre" = "T2",
                                                                 "Tercer Trimestre" ="T3",
                                                                 "Cuarto Trimestre" = "T4"
                                                     )#choices
                                         )#selectInput
                        )#conditionalPanel ==mc
                        
           ),#SidebarPanel
           mainPanel(strong("Datos extraídos del INE"),
                     p(textOutput('varSel_4')),
                     conditionalPanel(condition="input.elecc_4 == 'gr'",
                                      plotOutput('grafico1_4'),
                                      plotOutput('grafico2_4') ),
                     conditionalPanel(condition="input.elecc_4 == 'dt'",
                                      tableOutput('datos_4') ),
                     conditionalPanel(condition="input.elecc_4 == 'mc'",
                                      plotOutput('mapita_4'))
                     
           )#mainPanel
  )#tabPanel_4
  
  
  )#tabset



) )