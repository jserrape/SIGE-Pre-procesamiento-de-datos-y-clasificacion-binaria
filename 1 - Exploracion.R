## ---------------------------------------------------------------
## Exploracion

library(tidyverse)
library(funModeling)
library(ROSE)

#Cambio el directorio de trabajo
setwd("C:/Users/juanca/Desktop/SIGE")
options(max.print = 999999)
set.seed(1234)

#Cargo el conjunto de datos
datos <- read_csv('train.csv', na = c('NA', 'n/a', '', ' '))

#Obtengo un resumen de los datos
summary(datos)

#Obtengo estadisticas como el numero de ceros o de N/A
status <- df_status(datos)


#Balanceo los datos
table(datos$target)
datos <-
  ovun.sample(
    target ~ .,
    data = datos,
    p = 0.5,
    seed = 1,
    method = "under"
  )$data
table(datos$target)

#Guardo los datos balanceados
write_csv(datos, 'balanceado.csv')


#Obtengo la estructura basica para ver como están formados los datos
str(datos)

#Comparo los datos de la columna target
prop.table(table(datos$target))

#Creo un grafico de barras para comparar los datos de target
barplot(
  prop.table(table(datos$target)),
  col = c("red", "blue"),
  main = "Target",
  ylab = "Frecuencias Relativas"
)
