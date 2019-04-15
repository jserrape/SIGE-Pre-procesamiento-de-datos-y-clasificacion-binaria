## ---------------------------------------------------------------
## Exploracion

library(tidyverse)
library(funModeling)
library(ROSE)

#Cambio el directorio de trabajo
setwd("C:/Users/juanca/Desktop/SIGE")
options(max.print=999999)
set.seed(1234)

#Cargo el conjunto de datos
datos <- read_csv('train.csv', na = c('NA', 'n/a', '', ' '))

#Balanceo los datos
table(datos$target)
datos <- ovun.sample(target ~., data=datos, p=0.5, seed=1, method="over")$data
table(datos$target)

#Reduzco el conjunto a la mitad por limitaciones del equipo
write_csv(datos, 'balanceado.csv')

#Obtengo estadisticas como el numero de ceros o de N/A
status <- df_status(datos)

#Obtengo la estructura basica para ver como están formados los datos
str(datos)

#Comparo los datos de la columna target
prop.table(table(datos$target))

#Creo un grafico de barras para comparar los datos de target
barplot(prop.table(table(datos$target)),col=c("red","blue"),main="Target", ylab ="Frecuencias Relativas")
