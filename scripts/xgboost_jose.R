
# LIBRERIAS ---------------------------------------------------------------


library(tidyverse)
library(rio)
library(skimr)
library(viridis)
library(osmdata)
library(ggsn)
library(ggmap)
library(raster)
library(star)
library(sf)
library(leaflet)
library(stringr)
library(stringi)
library(tidytext)
library(janitor)
library(tm)
library(here)
library(caret)



# CARGAR DATOS ------------------------------------------------------------



base_completo <- readRDS("base_completa.Rds")
base_completo_bog <- base_completo %>% filter(city=="BogotÃ¡ D.C") %>% 
  select(
    price,
    surface_covered,
    rooms_imp,
    surface_total_2_imp,
    bathrooms_imp,
    primary,
    police,
    language_school,
    V_MAT_PISO_3,#?
    kindergarten,
    clinic,
    events_venue,
    theatre,
    cinema,
    bus_station,
    PA_HNV_9,#?
    office,
    waste_disposal,
    school,
    hospital,
    university,
    VA1_ESTRATO_6,
    restaurant,
    bar,
    P_QUEHIZO_PPAL_4,
    PA_HNVS_2
    
    
  )

#Filtrar datos de Medellin
base_completo_med <- base_completo %>% filter(city=="MedellÃ­n") %>% 
  select(
    price,
    surface_covered,
    rooms_imp,
    surface_total_2_imp,
    bathrooms_imp,
    primary,
    police,
    language_school,
    V_MAT_PISO_3,#?
    kindergarten,
    clinic,
    events_venue,
    theatre,
    cinema,
    bus_station,
    PA_HNV_9,#?
    office,
    waste_disposal,
    school,
    hospital,
    university,
    VA1_ESTRATO_6,
    restaurant,
    bar,
    P_QUEHIZO_PPAL_4,
    PA_HNVS_2
    
    
  )


# MODELO BOGOTA -----------------------------------------------------------


#train control
train_control_clas <- trainControl(method="cv", 
                                   number=5, 
                                   summaryFunction = pct_propcomp_valprop,#AQUI VA LA FUNCION
                                   allowParallel = T)
#Grilla de hiperparametros de xgboost

grid_xgboost_clas <- expand.grid(  nrounds = 10,
                                   eta = c(0.4, 0.3,0.2),
                                   max_depth = c(2, 4,6),
                                   gamma=c(0, 1,0.5), 
                                   colsample_bytree=0.6, 
                                   min_child_weight=1, 
                                   subsample=0.6
)




#Entrenar xgboost
set.seed(1)
xgboost_clas_bog <- train(
  price ~ 
    surface_covered+
    rooms_imp+
    surface_total_2_imp+
    bathrooms_imp+
    primary+
    police+
    language_school+
    V_MAT_PISO_3+#?
    kindergarten+
    clinic+
    events_venue+
    theatre+
    cinema+
    bus_station+
    PA_HNV_9+#?
    office+
    waste_disposal+
    school+
    hospital+
    university+
    VA1_ESTRATO_6+
    restaurant+
    bar+
    P_QUEHIZO_PPAL_4+
    PA_HNVS_2,
  data=base_completo_bog,
  method = "xgbTree",
  trControl = train_control_clas,
  maximize=FALSE,
  metric = "sum_pct_propcomp_valprop",#AQUI VA EL OUPUT
  tuneGrid = grid_xgboost_clas)




#Predecir Medellín

pred_med <- predict(xgboost_clas_bog,base_completo_med)



pct_propcomp_valprop_dtll(tibble("pred"=pred_med,
                                 "obs"=base_completo_med$price))
