#Another way to predict the GAM modelling 
library(tidyverse)
library(mgcv)
library(dplyr)
rm(list=ls())  # clear all objects in the environment 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # set directory to the location of the current script

data <- read.csv("~/Downloads/mexico_covid19 2.csv")
filtered_data <- data %>%
  select(-id, -ID_REGISTRO, -DELAY, -ABR_ENT, -SECTOR, -FECHA_DEF, -FECHA_ARCHIVO,-HABLA_LENGUA_INDIG, -MIGRANTE, -PAIS_NACIONALIDAD,-PAIS_ORIGEN ) #exlude some variables 
female_filtered_data <- filtered_data %>% filter(SEXO == 1) 
male_filtered_data <- filtered_data %>% filter(SEXO == 2)
#Working on the gam function 
# people that went to he UCI and which had the highest number 
data <- read.csv("~/Downloads/mexico_covid19 2.csv")
filtered_data$UCI <- as.factor(filtered_data$UCI)
gam_model <- gam(UCI ~ s(EDAD) + OTRO_CASO + OBESIDAD + RENAL_CRONICA + TABAQUISMO + CARDIOVASCULAR + OTRA_COM + HIPERTENSION + ASMA + INMUSUPR + EPOC + DIABETES + EMBARAZO, 
                 data = filtered_data, 
                 family = binomial())
summary(gam_model)

filtered_data$predicted_UCI <- predict(gam_model, newdata = filtered_data, type = "response")

plot(gam_model, select = "s(EDAD)", main="Age and ICU")
plot(gam_model, select = "OBESIDAD", main="Obesity and ICU")
plot(gam_model, select = "RENAL_CRONICA" , main="Chronic Kidney Disease and ICU")
plot(gam_model, select = "EMBARAZO", main="Pregnancy effect on ICU admissions")
#GAMs are more flexible than linear regression and easier than intpret than bagging, boosting, support vector machines 
gam.check(gam_model) #checking  GAM model in order to make sure that it is working 
plot(gam_model$residuals) #plot using base R in order to make sure that our results are accurate




















