#Trabajo para Macroeconomía avanzada

Aplicación que recoge datos del INE (en formato PC-AXIS) y los filtra en base a diferentes parametros. Tiene una opción para desplegar un mapa de España que permite visualizar los datos de manera más gráfica.

El repositorio contiene:
-Carpeta Archive con el codigo comprimido en un .Tar
-Un documento pdf con el resumén del desarrollo de la aplicación y un pequeño analisis de los datos
-Archivos Server.R y UI.R de la aplicación Shiny
-Un Scrip que solamente recoge la importación de los datos desde el INE
-El código adaptado a una plantilla Rmd

Cualquier persona con R instalado puede ejecutar la aplicación web con estos dos comandos desde una consola de R
` library(Shiny)`
` runGitHub('rv0lt',TrabajoMacro)`

