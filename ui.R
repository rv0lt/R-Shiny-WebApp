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
  headerPanel("Coste (en euros) por trabajador, comunidad autónoma y sectores de actividad"),
  sidebarPanel("Seleccione",
               radioButtons(
                 'elecc', label='Eliga que desea visualizar', choices = c("Gráficas"= "gr", "Datos"= "dt", "Mapa de Calor" = "mc"),
                 selected="gr"
               ),#elección de Gráfica, Datos o mapa de calor
               conditionalPanel(condition="input.elecc != 'mc'",
                                selectInput('Comunidades', 
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
                                dateRangeInput2("año", "Año", 
                                                startview = "year",
                                                min = "2008-01-01", max = "2018-01-01",
                                                minview = "years", maxview = "decades", 
                                                format ="yyyy" , start = "2008-01-01",
                                                
                                                end ="2009-01-01"
                                ),#AÑO_RangeInput
                                selectInput('TC',
                                            label = 'Trimeste Inicial',
                                            choices = c("Primer Trimestre" ="T1",
                                                        "Segundo Trimestre" = "T2",
                                                        "Tercer Trimestre" ="T3",
                                                        "Cuarto Trimestre" = "T4"
                                            )#choices
                                ),#TC_SelectInput
                                selectInput('TF',
                                            label = 'Trimeste Final',
                                            choices = c("Primer Trimestre" ="T1",
                                                        "Segundo Trimestre" = "T2",
                                                        "Tercer Trimestre" ="T3",
                                                        "Cuarto Trimestre" = "T4"
                                            )#choices
                                )#TF_SelectInput
               ),#ConditionalPanel != mc
               selectInput('Sectores',
                           label = 'Sector que desea consultar',
                           choices = c("Industria" = "Industria",
                                       "Servicios" = "Servicios",
                                       "Construcción" = "Construcción",
                                       "B_S Industria, construcción y servicios (excepto actividades de los hogares como empleadores y de organizaciones y organismos extraterritoriales)" = "B_S Industria, construcción y servicios (excepto actividades de los hogares como empleadores y de organizaciones y organismos extraterritoriales)"
                           )#choices
               ),#SECTORES_SelectInput
               selectInput('Componentes',
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
               
               conditionalPanel(condition="input.elecc == 'mc'",
                                dateInput2('añoMc', "Año que desea representar", 
                                           startview = "year",
                                           min = "2008-01-01", max = "2018-01-01",
                                           minview = "years", maxview = "decades", 
                                           format ="yyyy", value="2008-01-01" ),#dateInput
                                
                                selectInput('Tmc',
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
            p(textOutput('varSel')),
            #h1('A continuación se muestran los datos.'),
            conditionalPanel(condition="input.elecc == 'gr'",
                             plotOutput('grafico1'),
                             plotOutput('grafico2') ),
            conditionalPanel(condition="input.elecc == 'dt'",
                             tableOutput('datos') ),
            conditionalPanel(condition="input.elecc == 'mc'",
                             plotOutput('mapita'))
            #textOutput('prueba')
  )#main_Panel
  
)#Fluid_Page


)