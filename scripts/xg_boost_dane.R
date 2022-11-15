#install.packages("xgboost")
#install.packages("tidyverse")
#install.packages("caret")
#install.packages("groupdata2")
#install.packages("mlr")
library(mlr)
library("xgboost")
library("tidyverse")
library("caret")
require("here")
library(groupdata2)
options(scipen=999)
set.seed(6347)


## 1. Cargue de información--------------------------------------------------------

path <- here()
setwd(path)
#train <- read.csv(here("./Data/train.csv"))
#test <- read.csv(here("./Data/test.csv"))
#val<-read.csv(here("./Data/val.csv"))
#test_examen<-read.csv(here("./Data/df_test_examen.csv"))


Train_price <- readRDS(here("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/datos/dataPS3/train.rds"))
Test_price <- readRDS(here("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/datos/dataPS3/test.rds"))
POC_Price=union(Train_price,Test_price)

Train_price <- readRDS(here("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/datos/dataPS3/train.rds"))

Train_bog_jose <- readRDS(here("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/otras_variables/train_original_dscrptin_ftrs_2_2_bog_mnz_imp_noimp.rds"))
Train_Med_jose <- readRDS(here("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/otras_variables/train_original_dscrptin_ftrs_2_2_med_mnz_imp_noimp.rds"))
Train_Cali_jose <- readRDS(here("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/otras_variables/test_original_dscrptin_ftrs_2_2_cali_mnz_imp_noimp.rds"))

Train_jose=union(Train_bog_jose,Train_Med_jose,Train_Cali_jose)

Train_jose=select(Train_jose,-city,-price,)


Ame_Train<-read.csv(here("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/otras_variables/train_original_dscrptin_ftrs_v12_11_222.csv"))

Ame_Train=select(
  Ame_Train, 
  -X,
  -estrato,
  -terraza,
  -balcon,
  -garaje,
  -nueva
)

Ame_Test<-read.csv(here("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/otras_variables/test_with_amenities.csv"))

Amenities<-union(Ame_Train,Ame_Test)
DANE <- readRDS(here("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/POC_ALL.Rds"))
DANE<-select(DANE,-BARRIO)

DANE[is.na(DANE)] <- 0
Amenities[is.na(Amenities)] <- 0

POC_ALL=left_join(Amenities,DANE)
POC_ALL=left_join(POC_ALL,Train_jose)

cols=c("property_type","operation_type","property_type_imp")

dummy_POC_ALL=dummy_cols(POC_ALL[cols], remove_selected_columns=TRUE)


POC_ALL=select(POC_ALL,-COD_DANE,-property_type,-property_type_imp,-operation_type,-title,-description)
POC_ALL=(cbind(POC_ALL,dummy_POC_ALL))

POC_ALL[is.na(POC_ALL)] <- 0

train<- POC_ALL %>% filter(city=="BogotÃ¡ D.C")
train<- train %>% filter(price<800000000)
train<- select(train,-city,-property_id,-lat,-lon)

#train<- select(train,price,surface_total)


Test<- POC_ALL %>% filter(city=="MedellÃ­n")
Test<- select(Test,-city,-property_id,-lat,-lon)

val_fin<- POC_ALL %>% filter(city=="Cali")
val_fin<- select(val_fin,-city,-property_id,-lat,-lon)



#Test<- select(Test,price,surface_total)

#as.data.frame(sapply(POC_ALL,class))
#saveRDS(POC_ALL, file = "C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Base_Completa.rds")


## 2. Entrenamiento XGBoost---------------------------------------------------------

## 2.0 Balance train dataset-------------------------------------


train_xg <- 
  train %>% 
  select(-price) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = train$price)


val_xg <- 
  Test %>% 
  select(-price) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = Test$price)


val_fin_xg <- 
  val_fin %>% 
  select(-price) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = val_fin$price)


## 2.4 Encontrar variables importantes--------------------------------------------

modelo_01=xgboost(data = train_xg, 
        objective = "reg:squarederror",
        nrounds = 100, max.depth = 3, eta = 0.8, nthread = 2,gamma=0)



predict_01 <- predict(modelo_01, val_xg)

compra=as.data.frame(cbind(Test$price,predict_01))
compra$factor=abs(compra$V1-compra$predict_01)

compra$comp=ifelse(compra$factor<=40000000, 1, 0)
a=as.data.frame(table(compra$comp))
a

modelo_01 <- xgboost(data = train_xg, 
                     objective = "reg:tweedie")



predict_01 <- predict(modelo_01, val_fin_xg)
