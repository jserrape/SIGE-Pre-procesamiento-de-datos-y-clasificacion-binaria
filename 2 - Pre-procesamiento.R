## ---------------------------------------------------------------
## Pre-procesamiento

library(tidyverse)
library(funModeling)
library(corrplot)

remove_all_outliers <- function(df) {
  df[, sapply(df, is.numeric)] <-
    lapply(df[, sapply(df, is.numeric)], remove_outliers)
  df
}

#Cambio el directorio de trabajo
setwd("C:/Users/juanca/Desktop/SIGE")
options(max.print = 999999)
set.seed(1)

#Cargo el conjunto de datos reducido
datos <- read_csv('balanceado.csv', na = c('NA', 'n/a', '', ' '))

#Elimino la columna ID_code
datos <- select(datos,-ID_code)

#Elimino las entradas con valores NA
datos <- na.omit(datos)

## ---------------------------------------------------------------
## Estudio la correlacion de los datos

data_num <- datos %>%
  na.exclude() %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

cor_target <- correlation_table(data_num, target = 'target')
important_vars <- cor_target %>%
  filter(abs(target) >= 0.1)

datos <- datos %>%
  select(one_of(important_vars$Variable))

# Alta correlacion entre si
data_num <- datos %>%
  na.exclude() %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)
rcorr_result <- rcorr(as.matrix(data_num))
cor_matrix <- as.tibble(rcorr_result$r, rownames = "variable")
corrplot(
  rcorr_result$r,
  type = "upper",
  order = "original",
  tl.col = "black",
  tl.srt = 45
)

v <- varclus(as.matrix(data_num), similarity = "pearson")
plot(v)


## ---------------------------------------------------------------
## Estudiar la dispersion de los datos, sin considerar los outlines

boxplot(datos)
datos  <-
  as.data.frame(apply(datos[, 1:13], 2, function(x)
    (x - min(x)) / (max(x) - min(x))))
boxplot(datos, outline = FALSE)

## ---------------------------------------------------------------
## Gestion de ruido (No tengo PC probarlo.....)

library(caret)
library(NoiseFiltersR)

datos <- datos %>%
  mutate(target = as.factor(target)) %>%
  mutate(var_6 = as.factor(var_6))   %>%
  mutate(var_166 = as.factor(var_166))   %>%
  mutate(var_110 = as.factor(var_110))   %>%
  mutate(var_76 = as.factor(var_76))   %>%
  mutate(var_174 = as.factor(var_174))   %>%
  mutate(var_81 = as.factor(var_81))   %>%
  mutate(var_22 = as.factor(var_22))      %>%
  mutate(var_12 = as.factor(var_12))      %>%
  mutate(var_139 = as.factor(var_139))      %>%
  mutate(var_146 = as.factor(var_146))      %>%
  mutate(var_26 = as.factor(var_26))      %>%
  mutate(var_53 = as.factor(var_53))      %>%
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


noise_filter <- AENN(target ~ ., datos)

summary(noise_filter)
identical(noise_filter$cleanData, datos[setdiff(1:nrow(datos), noise_filter$remIdx), ])

## ---------------------------------------------------------------
## Guardo los datos pre-procesados
write_csv(datos, 'datos_pre_proc.csv')
