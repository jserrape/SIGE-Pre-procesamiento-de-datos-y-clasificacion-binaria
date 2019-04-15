## ---------------------------------------------------------------
## Pre-procesamiento

library(tidyverse)
library(funModeling)
library(corrplot)

#Cambio el directorio de trabajo
setwd("C:/Users/juanca/Desktop/SIGE")
options(max.print=999999)
set.seed(1)

#Cargo el conjunto de datos reducido
datos <- read_csv('balanceado.csv', na = c('NA', 'n/a', '', ' '))

#Elimino la columna ID_code
datos <- select(datos, -ID_code)

#Elimino las entradas con valores NA
datos <- na.omit(datos)


## ---------------------------------------------------------------
## Estudio la correlacion de los datos

data_num <- datos %>%
  na.exclude() %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

cor_target <- correlation_table(data_num, target='target')
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
corrplot(rcorr_result$r, type = "upper", order = "original", tl.col = "black", tl.srt = 45)

v <- varclus(as.matrix(data_num), similarity="pearson") 
plot(v)


## ---------------------------------------------------------------
## Gestion de ruido

library(NoiseFiltersR)


## ---------------------------------------------------------------
## Guardo los datos pre-procesados
write_csv(datos, 'datos_pre_proc.csv')
