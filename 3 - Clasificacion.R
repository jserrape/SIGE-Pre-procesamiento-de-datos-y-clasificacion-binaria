## ---------------------------------------------------------------
## Clasificacion

library(tidyverse)
library(funModeling)
library(rpart)
library(rattle)
library(randomForest)

#Cambio el directorio de trabajo
setwd("C:/Users/juanca/Desktop/SIGE")
options(max.print=999999)
set.seed(1)

#Cargo los conjuntos de datos de entrenamiento y test
train <- read_csv('datos_entrenamiento.csv', na = c('NA', 'n/a', '', ' '))
test <- read_csv('datos_prueba.csv', na = c('NA', 'n/a', '', ' '))


# https://www.r-bloggers.com/how-to-implement-random-forests-in-r/

# Fine tuning parameters of Random Forest model
model2 <- randomForest(target ~ ., data = train, ntree = 50, mtry = 6, importance = TRUE)
model2
plot(model2)


# Predicting on Validation set
predValid <- predict(model2, test, type = "class")

# Checking classification accuracy
mean(predValid == test$target)                    
table(predValid,test$target)
  
