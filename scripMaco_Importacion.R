library(pxR)
library(sp)
setwd("/home/arevuelta/Escritorio/yo/FI/3/macro/TrabajoMacro")

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


#spain = readRDS("gadm36_ESP_1_sp.rds")
con <- url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_ESP_1_sp.rds","rb")

con <- gzfile("https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_ESP_1_sp.rds","rb")
spain = readRDS(con)
close(con)

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

query = filter(data_1, Componentes.del.coste=="Coste laboral total", Sectores.de.actividad.CNAE.2009=="Servicios", Periodo=="2010T2", Comunidades.y.Ciudades.Autónomas!="Total Nacional" )
View(query)
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
spplot(spain, 'mc', col.regions=colores )





