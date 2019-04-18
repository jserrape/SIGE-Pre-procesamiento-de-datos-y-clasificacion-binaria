## ---------------------------------------------------------------
## Clasificacion

library(tidyverse)
library(funModeling)
library(rpart)
library(caret)
library(partykit)
library(rattle)
library(mice)
library(lubridate)

library(pROC)
library(MLmetrics)
library(rattle)
library(randomForest)


#Cambio el directorio de trabajo
setwd("C:/Users/juanca/Desktop/SIGE")
options(max.print = 999999)
set.seed(0)

datos <-
  read_csv('datos_pre_proc.csv', na = c('NA', 'n/a', '', ' '))

#Reduzco el tamaño para hacer pruebas
#datos <- datos[createDataPartition(datos$target, p = .3, list = FALSE, times = 1), ]

# Datos con imputacion de valores perdidos
data_preproc <-
  datos %>%
  mutate(target = as.factor(ifelse(target == 1, 'Yes', 'No'))) %>%
  select(
    target,
    var_6,
    var_166,
    var_110,
    var_76,
    var_174,
    var_81,
    var_22,
    var_12,
    var_139,
    var_146,
    var_26,
    var_53
  )
imputation <-
  mice(
    data_preproc,
    method = c("", "", "", "", "", "", "", "", "", "", "", "", ""),
    printFlag = F
  )
datos <- complete(imputation) %>%
  na.exclude()

# Parámetros
rpartCtrl <-
  trainControl(
    verboseIter = F,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
rpartParametersGrid <- expand.grid(.cp = c(0.01, 0.05))

# Conjuntos de entrenamiento y validación
trainIndex <-
  createDataPartition(datos$target,
                      p = .8,
                      list = FALSE,
                      times = 1)
train <- datos[trainIndex, ]
val   <- datos[-trainIndex, ]



## ---------------------------------------------------------------
## Accuracy : 0.5883  Tiempo: 11.83264 secs

h1 <- now()

# Entrenamiento del modelo rpart
rpartModel <-
  train(
    target ~ .,
    data = train,
    method = "rpart",
    metric = "ROC",
    trControl = rpartCtrl,
    tuneGrid = rpartParametersGrid
  )

# Visualización del modelo
rpartModel_party <- as.party(rpartModel$finalModel)
plot(rpartModel_party)

# Cálculo de error
prediction <- predict(rpartModel, val, type = "raw")
cm_train <- confusionMatrix(prediction, val[["target"]])
cm_train

h2 <- now()
h2 - h1


## ---------------------------------------------------------------
## Accuracy : Accuracy : 0.6632   Tiempo: 43.11453 mins 

h1 <- now()

# Entrenamiento del modelo rf
rfModel <-
  train(
    target ~ .,
    data = train,
    method = "rf",
    metric = "ROC",
    trControl = rpartCtrl
  )

# Visualización del modelo
rpartModel_party <- as.party(rfModel$finalModel)
plot(rpartModel_party)

# Cálculo de error
prediction <- predict(rfModel, val, type = "raw")
cm_train <- confusionMatrix(prediction, val[["target"]])
cm_train
h2 <- now()
h2 - h1


## ---------------------------------------------------------------
## Accuracy : Accuracy : 0.6969  Time difference of 1.024036 hours

h1 <- now()

# Entrenamiento del modelo svm
svmCtrl <-
  trainControl(
    verboseIter = F,
    classProbs = TRUE,
    method = "repeatedcv",
    number = 10,
    repeats = 1,
    summaryFunction = twoClassSummary
  )

# Visualización del modelo
svmModel <-
  train(
    target ~ .,
    data = train,
    method = "svmRadial",
    metric = "ROC",
    trControl = svmCtrl,
    tuneLength = 10
  )
print(svmModel)
plot(svmModel)

# Cálculo de error
prediction <- predict(svmModel, val, type = "raw")
cm_train <- confusionMatrix(prediction, val[["target"]])
cm_train

h2 <- now()
h2 - h1

