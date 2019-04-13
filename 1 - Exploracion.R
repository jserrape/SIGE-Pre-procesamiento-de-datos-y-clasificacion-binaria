## ---------------------------------------------------------------
## Exploracion

library(tidyverse)
library(funModeling)

#Cambio el directorio de trabajo
setwd("C:/Users/juanca/Desktop/SIGE")
options(max.print=999999)
set.seed(1)

#Cargo el conjunto de datos
datos <- read_csv('train.csv', na = c('NA', 'n/a', '', ' '))

#Reduzco el conjunto a la mitad por limitaciones del equipo
reducido <- sample_frac(datos, .1)
write_csv(reducido, 'reducido.csv')
datos <- reducido

#Obtengo estadisticas como el numero de ceros o de N/A
status <- df_status(datos)

#Obtengo la estructura basica para ver como están formados los datos
str(datos)

#Comparo los datos de la columna target
prop.table(table(datos$target))

#Creo un grafico de barras para comparar los datos de target
barplot(prop.table(table(datos$target)),col=c("red","blue"),main="Target", ylab ="Frecuencias Relativas")
