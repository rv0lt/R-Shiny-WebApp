library(pxR)

#Con el simbolo %>% se puede escribir en una linea los comados que realizan la lectura del PC-Axis
# y la transformacion a datos que entienda R

#Coste laboral por trabajador, comunidad aut?noma, sectores de actividad
data_1 <- read.px("http://www.ine.es/jaxiT3/files/t/es/px/6061.px?nocab=1") %>% as.data.frame()

#Coste laboral por hora efectiva, comunidad aut?noma, sectores de actividad
data_2 <- read.px("http://www.ine.es/jaxiT3/files/t/es/px/6062.px?nocab=1") %>% as.data.frame()

#N?mero de vacantes comunidad aut?noma
data_3 <- read.px("http://www.ine.es/jaxiT3/files/t/es/px/6064.px?nocab=1") %>% as.data.frame()

#Motivos por los que no existen vacantes por comunidad aut?noma
data_4 <- read.px("http://www.ine.es/jaxiT3/files/t/es/px/6066.px?nocab=1") %>% as.data.frame()

View(data_1)
View(data_2)
View(data_3)
View(data_4)






