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
library(MLmetrics)



## -------------------------------------------------------------------------------------
## -------------------------------------------------------------------------------------
## Funciones ##

my_roc <-
  function(data,
           predictionProb,
           target_var,
           positive_class) {
    auc <-
      roc(data[[target_var]], predictionProb[[positive_class]], levels = unique(data[[target_var]]))
    roc <-
      plot.roc(
        auc,
        ylim = c(0, 1),
        type = "S" ,
        print.thres = T,
        main = paste('AUC:', round(auc$auc[[1]], 2))
      )
    return(list("auc" = auc, "roc" = roc))
  }


trainRF <-
  function(train_data,
           rfCtrl = NULL,
           rfParametersGrid = NULL) {
    if (is.null(rfCtrl)) {
      rfCtrl <- trainControl(
        verboseIter = F,
        classProbs = TRUE,
        method = "repeatedcv",
        number = 10,
        repeats = 1,
        summaryFunction = twoClassSummary
      )
    }
    if (is.null(rfParametersGrid)) {
      rfParametersGrid <- expand.grid(.mtry = c(sqrt(ncol(train))))
    }
    
    rfModel <- train(
      target ~ .,
      data = train_data,
      method = "rf",
      metric = "ROC",
      trControl = rfCtrl,
      tuneGrid = rfParametersGrid
    )
    
    return(rfModel)
  }
## -------------------------------------------------------------------------------------
## -------------------------------------------------------------------------------------



#Cambio el directorio de trabajo
setwd("C:/Users/juanca/Desktop/SIGE")
options(max.print = 999999)
set.seed(0)

#Cargo los conjuntos de datos de entrenamiento y test
#train <- read_csv('datos_entrenamiento.csv', na = c('NA', 'n/a', '', ' '))
#test <- read_csv('datos_prueba.csv', na = c('NA', 'n/a', '', ' '))

datos <-
  read_csv('datos_pre_proc.csv', na = c('NA', 'n/a', '', ' '))

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
train <- datos[trainIndex,]
val   <- datos[-trainIndex,]



## ---------------------------------------------------------------
## Accuracy : 0.5883  Tiempo: 15.7612 secs

h1 <- now()

# Entrenamiento del modelo
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
## Accuracy : 0.5883   Tiempo: 20 años de ejecucion despues, sigo queriendo morirme

h1 <- now()

# Entrenamiento utilizando otra técnica
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




## https://github.com/jgromero/sige2019/blob/master/pr%C3%A1cticas/03.%20An%C3%A1lisis%20predictivo/loans-unbalanced.R linea 82
## ---------------------------------------------------------------
## Accuracy : 0,66066  Tiempo: 5.4646 mins

h1 <- now()

rfModel <- trainRF(train)
saveRDS(rfModel, file = "model1.rds")
rfModel <- readRDS("model1.rds")
orig_fit <- rfModel
print(rfModel)

prediction_p <- predict(rfModel, val, type = "prob")
prediction_r <- predict(rfModel, val, type = "raw")
result <- my_roc(val, prediction_p, "target", "Yes")

plotdata <- val %>%
  select(target) %>%
  bind_cols(prediction_p) %>%
  bind_cols(Prediction = prediction_r)
table(plotdata$target, plotdata$Prediction)  # columnas son predicciones
ggplot(plotdata) +
  geom_bar(aes(x = target, fill = Prediction), position = position_fill())

# --> validacion real con un subconjunto del conjunto original
test <- datos %>%
  sample_n(30000)

prediction_p <- predict(rfModel, test, type = "prob")
prediction_r <- predict(rfModel, test, type = "raw")

result <- my_roc(test, prediction_p, "target", "Yes")

plotdata <- test %>%
  select(target) %>%
  bind_cols(prediction_p) %>%
  bind_cols(Prediction = prediction_r)

table(plotdata$target, plotdata$Prediction)

h2 <- now()
h2 - h1
