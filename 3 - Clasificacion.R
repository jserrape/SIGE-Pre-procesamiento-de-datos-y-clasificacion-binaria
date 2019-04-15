## ---------------------------------------------------------------
## Clasificacion

library(tidyverse)
library(funModeling)
library(rpart)
library(rattle)
library(randomForest)
library(caret)
library(partykit)
library(rattle)
library(pROC)
library(mice)
library(lubridate)

h1 <- now()

#Cambio el directorio de trabajo
setwd("C:/Users/juanca/Desktop/SIGE")
options(max.print=999999)
set.seed(0)

#Cargo los conjuntos de datos de entrenamiento y test
#train <- read_csv('datos_entrenamiento.csv', na = c('NA', 'n/a', '', ' '))
#test <- read_csv('datos_prueba.csv', na = c('NA', 'n/a', '', ' '))

datos <- read_csv('datos_pre_proc.csv', na = c('NA', 'n/a', '', ' '))

# Datos con imputacion de valores perdidos
data_preproc <-
  datos %>%
  mutate(target = as.factor(ifelse(target == 1, 'Yes', 'No'))) %>%
  select(target, var_81, var_99, var_190, var_6, var_80, var_1, var_146)
imputation <-
  mice(data_preproc, method = c("", "", "", "cart", "", "", "", "cart"), printFlag = F)
datos <- complete(imputation) %>%
  na.exclude()

# Parámetros
rpartCtrl <- trainControl(verboseIter = F, classProbs = TRUE, summaryFunction = twoClassSummary)
rpartParametersGrid <- expand.grid(.cp = c(0.01, 0.05))

# Conjuntos de entrenamiento y validación
trainIndex <- createDataPartition(datos$target, p = .8, list = FALSE, times = 1)
train <- datos[trainIndex, ] 
val   <- datos[-trainIndex, ]

# Entrenamiento del modelo
rpartModel <- train(target ~ ., data = train, method = "rpart", metric = "ROC", trControl = rpartCtrl, tuneGrid = rpartParametersGrid)


# Visualización del modelo
rpartModel_party <- as.party(rpartModel$finalModel)
plot(rpartModel_party)


# Cálculo de error
prediction <- predict(rpartModel, val, type = "raw") 
cm_train <- confusionMatrix(prediction, val[["target"]])
cm_train





# Entrenamiento utilizando otra técnica
rfModel <- train(target ~ ., data = train, method = "rf", metric = "ROC", trControl = rpartCtrl)

# Visualización del modelo
rpartModel_party <- as.party(rfModel$finalModel)
plot(rpartModel_party)

# Cálculo de error
prediction <- predict(rfModel, val, type = "raw") 
cm_train <- confusionMatrix(prediction, val[["target"]])
cm_train

h2 <- now()
h2 - h1
