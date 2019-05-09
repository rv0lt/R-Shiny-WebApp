library(pxR)
library(shiny)
library(dplyr)
#Con el simbolo %>% se puede escribir en una linea los comados que realizan la lectura del PC-Axis
# y la transformacion a datos que entienda R

#Coste laboral por trabajador, comunidad aut?noma, sectores de actividad
data_1 <- read.px("http://www.ine.es/jaxiT3/files/t/es/px/6061.px?nocab=1") %>% as.data.frame()

#Coste laboral por hora efectiva, comunidad aut?noma, sectores de actividad
data_2 <- read.px("http://www.ine.es/jaxiT3/files/t/es/px/6062.px?nocab=1") %>% as.data.frame()

#Tiempo de trabajo por trabajador y mes por comunidad aut?noma, tipo de jornada, sectores de actividad
data_3 <- read.px("http://www.ine.es/jaxiT3/files/t/es/px/6063.px?nocab=1") %>% as.data.frame()

#N?mero de vacantes comunidad aut?noma
data_4 <- read.px("http://www.ine.es/jaxiT3/files/t/es/px/6064.px?nocab=1") %>% as.data.frame()

#Motivos por los que no existen vacantes por comunidad aut?noma
data_5 <- read.px("http://www.ine.es/jaxiT3/files/t/es/px/6066.px?nocab=1") %>% as.data.frame()

View(data_1)
View(data_2)
View(data_3)
View(data_4)
View(data_5)

query = filter(data_1, data_1$Componentes.del.coste =="Coste laboral total" 
               & data_1$Sectores.de.actividad.CNAE.2009=="Servicios" & 
                 (data_1$Comunidades.y.Ciudades.Autónomas=="Total Nacional" ))
View(query)
