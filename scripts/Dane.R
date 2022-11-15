#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata","caTools","maps","fastDummies"))

library(here)
library(tidyverse)
library(fastDummies)
library(caTools)
library(ggplot2)
library(sf)
library(maps)
library(dplyr)
library(foreign)

set.seed(6347)
memory.limit(size=56000)
rm(list = ls())
# 1. Read Dane data--------------------------------------------------

#1.1. Organizar Información Cundinamarca

#1.1.1. Cargar información-------------------------------------------

#Cund_Viv<-read.csv("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Censo/11Bogota/CNPV2018_1VIV_A2_11.CSV")
#Cund_Hog<-read.csv("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Censo/11Bogota/CNPV2018_2HOG_A2_11.CSV")
#Cund_Per<-read.csv("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Censo/11Bogota/CNPV2018_5PER_A2_11.CSV")
#Cund_MGN<-read.csv("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Censo/11Bogota/CNPV2018_MGN_A2_11.CSV",colClasses=c("COD_DANE_ANM"="character"))

#Ant_Viv<-read.csv("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Censo/05Antioquia/CNPV2018_1VIV_A2_05.CSV")
#Ant_Hog<-read.csv("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Censo/05Antioquia/CNPV2018_2HOG_A2_05.CSV")
#Ant_Per<-read.csv("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Censo/05Antioquia/CNPV2018_5PER_A2_05.CSV")
#Ant_MGN<-read.csv("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Censo/05Antioquia/CNPV2018_MGN_A2_05.CSV",colClasses=c("COD_DANE_ANM"="character"))

#Vall_Viv<-read.csv("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Censo/76ValleDelCauca/CNPV2018_1VIV_A2_76.CSV")
#Vall_Hog<-read.csv("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Censo/76ValleDelCauca/CNPV2018_2HOG_A2_76.CSV")
#Vall_Per<-read.csv("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Censo/76ValleDelCauca/CNPV2018_5PER_A2_76.CSV")
#Vall_MGN<-read.csv("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Censo/76ValleDelCauca/CNPV2018_MGN_A2_76.CSV",colClasses=c("COD_DANE_ANM"="character"))


Cund_Viv<-read.csv(here("./Data/CNPV2018_1VIV_A2_11.CSV"))
Cund_Hog<-read.csv(here("./Data/CNPV2018_2HOG_A2_11.CSV"))
Cund_Per<-read.csv(here("./Data/CNPV2018_5PER_A2_11.CSV"))
Cund_MGN<-read.csv(here("./Data/CNPV2018_MGN_A2_11.CSV",colClasses=c("COD_DANE_ANM"="character")))

Ant_Viv<-read.csv(here("./Data/CNPV2018_1VIV_A2_05.CSV"))
Ant_Hog<-read.csv(here("./Data/CNPV2018_2HOG_A2_05.CSV"))
Ant_Per<-read.csv(here("./Data/CNPV2018_5PER_A2_05.CSV"))
Ant_MGN<-read.csv(here("./Data/CNPV2018_MGN_A2_05.CSV",colClasses=c("COD_DANE_ANM"="character")))

Vall_Viv<-read.csv(here("./Data/CNPV2018_1VIV_A2_76.CSV"))
Vall_Hog<-read.csv(here("./Data/CNPV2018_2HOG_A2_76.CSV"))
Vall_Per<-read.csv(here("./Data/CNPV2018_5PER_A2_76.CSV"))
Vall_MGN<-read.csv(here("./Data/CNPV2018_MGN_A2_76.CSV",colClasses=c("COD_DANE_ANM"="character")))


Cund_Viv<-union(Cund_Viv,Ant_Viv,Vall_Viv)
Cund_Hog<-union(Cund_Hog,Ant_Hog,Vall_Hog)
Cund_Per<-union(Cund_Per,Ant_Per,Vall_Per)
Cund_MGN<-union(Cund_MGN,Ant_MGN,Vall_MGN)

#saveRDS(Cund_Viv, file = "C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Cund_Viv.rds")
#saveRDS(Cund_Hog, file = "C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Cund_Hog.rds")
#saveRDS(Cund_Per, file = "C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Cund_Per.rds")
#saveRDS(Cund_MGN, file = "C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Cund_MGN.rds")


saveRDS(Cund_Viv, file = "./Data/Cund_Viv.rds")
saveRDS(Cund_Hog, file = "./Data/Cund_Hog.rds")
saveRDS(Cund_Per, file = "./Data/Cund_Per.rds")
saveRDS(Cund_MGN, file = "./Data/Cund_MGN.rds")



rm(list = ls())

###Cargar los apartamentos (solo latitud y longitud)
##Cargar lso barios .shp

#Cund_MGN=Cund_MGN %>% filter(U_DPTO=="25")

#View(Cund_Per)
#View(Cund_MGN)

#Cund_MGN[Cund_MGN["COD_ENCUESTAS"]==17769711]

#1.1.2. Crear información agregada----------------------------

#1.1.2.1. Lista de cuadras-------------------------------------

#Cund_MGN <- readRDS(here("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Cund_MGN.rds"))
Cund_MGN <- readRDS(here("./Data/Cund_MGN.rds"))


Cund_MGN=select(
  Cund_MGN,
  COD_ENCUESTAS,
  COD_DANE_ANM
)

names(Cund_MGN)[2]='COD_DANE'


#1.1.2.2. Hogares-------------------------------------

Cund_Hog <- readRDS(here("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Cund_Hog.rds"))

Cund_Hog[is.na(Cund_Hog)] <- 0

cols <- c("H_DONDE_PREPALIM")
dummy_Cund_Hog=dummy_cols(Cund_Hog[cols], remove_selected_columns=TRUE)

Cund_Hog=select(
  Cund_Hog,
  COD_ENCUESTAS,
  H_NRO_CUARTOS, #AVG Y SUM
  H_NRO_DORMIT, #AVG Y SUM
  HA_TOT_PER, #AVG y SUM
)

Cund_df_Hog=(cbind(Cund_Hog,dummy_Cund_Hog))

Cund_df_Hog<-left_join(Cund_df_Hog,Cund_MGN)


Cund_df_Hog=select(
  Cund_df_Hog,
  -COD_ENCUESTAS
)

Cund_df_Hog2<-Cund_df_Hog %>% group_by(COD_DANE) %>% summarize(
              SUM_H_NRO_CUARTOS=sum(H_NRO_CUARTOS),
              SUM_H_NRO_DORMIT=sum(H_NRO_DORMIT),
              SUM_HA_TOT_PER=sum(H_NRO_DORMIT),
              AVG_H_NRO_CUARTOS=mean(H_NRO_CUARTOS),
              AVG_H_NRO_DORMIT=mean(H_NRO_DORMIT),
              AVG_HA_TOT_PER=mean(H_NRO_DORMIT),
              H_DONDE_PREPALIM_0=sum(H_DONDE_PREPALIM_0),
              H_DONDE_PREPALIM_1=sum(H_DONDE_PREPALIM_1),
              H_DONDE_PREPALIM_2=sum(H_DONDE_PREPALIM_2),
              H_DONDE_PREPALIM_3=sum(H_DONDE_PREPALIM_3),
              H_DONDE_PREPALIM_4=sum(H_DONDE_PREPALIM_4),
              H_DONDE_PREPALIM_5=sum(H_DONDE_PREPALIM_5),
              H_DONDE_PREPALIM_6=sum(H_DONDE_PREPALIM_6),
              H_DONDE_PREPALIM_9=sum(H_DONDE_PREPALIM_9)
              ) 


saveRDS(Cund_df_Hog2, file = "C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Dane_Hogar.rds")

rm(Cund_df_Hog2,Cund_df_Hog,Cund_Hog,dummy_Cund_Hog,cols)

#1.1.2.3. Viviendas-------------------------------------

Cund_Viv <- readRDS(here("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Cund_Viv.rds"))

Cund_Viv[is.na(Cund_Viv)] <- 0

cols <- c("UA_CLASE","UVA_ESTATER","UVA1_TIPOTER","UVA_ESTA_AREAPROT","UVA_USO_UNIDAD",
          "V_TIPO_VIV","V_CON_OCUP","V_MAT_PARED","V_MAT_PISO","VA1_ESTRATO") 

cols2 <- c("VB_ACU","VC_ALC","VD_GAS","VE_RECBAS","VE1_QSEM",
          "VF_INTERNET","V_TIPO_SERSA","L_TIPO_INST","L_EXISTEHOG")

Cund_Viv[cols] <- lapply(Cund_Viv[cols], factor)
dummy_Cund_Viv=dummy_cols(Cund_Viv[cols], remove_selected_columns=TRUE)

Cund_Viv[cols2] <- lapply(Cund_Viv[cols2], factor)
dummy_Cund_Viv2=dummy_cols(Cund_Viv[cols2], remove_selected_columns=TRUE)

#View(Cund_Viv)

Cund_Viv=select(
  Cund_Viv,
  COD_ENCUESTAS,
  V_TOT_HOG, #SUM
)

Cund_Viv2=select(
  Cund_Viv,
  COD_ENCUESTAS,
)

Cund_df_Viv=(cbind(Cund_Viv,dummy_Cund_Viv))
Cund_df_Viv<-left_join(Cund_df_Viv,Cund_MGN)

Cund_df_Viv=select(
  Cund_df_Viv,
  -COD_ENCUESTAS
)

Cund_df_Viv2<-Cund_df_Viv %>% group_by(COD_DANE) %>% summarize(across(everything(), sum))


Cund_df_Viv3=(cbind(Cund_Viv2,dummy_Cund_Viv2))
Cund_df_Viv3<-left_join(Cund_df_Viv3,Cund_MGN)

Cund_df_Viv3=select(
  Cund_df_Viv3,
  -COD_ENCUESTAS
)


Cund_df_Viv4<-Cund_df_Viv3 %>% group_by(COD_DANE) %>% summarize(across(everything(), sum))

Cund_df_Viv5<-left_join(Cund_df_Viv2,Cund_df_Viv4)

saveRDS(Cund_df_Viv5, file = "C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Dane_Vivienda.rds")

rm(dummy_Cund_Viv,dummy_Cund_Viv2,Cund_Viv,Cund_Viv2,Cund_df_Viv,Cund_df_Viv2,Cund_df_Viv3,Cund_df_Viv5,Cund_df_Viv4,dummy_Cund_Viv,dummy_Cund_Viv2,cols,cols2)



#1.1.2.4. Personas-------------------------------------

Cund_Per <- readRDS(here("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Cund_Per.rds"))

Cund_Per[is.na(Cund_Per)] <- 0

cols <- c("P_SEXO","P_EDADR","P_PARENTESCOR","PA1_GRP_ETNIC","PA_LUG_NAC","PB1_QOTRAS_LENG",
              "PA_VIVIA_5ANOS","PA_VIVIA_1ANO","P_ENFERMO","P_QUEHIZO_PPAL","PA_LO_ATENDIERON")

cols2 <- c("PA1_CALIDAD_SERV","CONDICION_FISICA","P_ALFABETA","PA_ASISTENCIA","P_NIVEL_ANOSR",
          "P_TRABAJO","P_EST_CIVIL","PA_HNV","PA_HNVS","PA_HFC")
              
Cund_Per[cols] <- lapply(Cund_Per[cols], factor)
dummy_Cund_Per=dummy_cols(Cund_Per[cols], remove_selected_columns=TRUE)

Cund_Per[cols2] <- lapply(Cund_Per[cols2], factor)
dummy_Cund_Per2=dummy_cols(Cund_Per[cols2], remove_selected_columns=TRUE)

#View(Cund_Per)

Cund_Per=select(
  Cund_Per,
  COD_ENCUESTAS,
  P_NRO_PER, #AVG
  PA1_THNV, #SUM
  PA1_THSV, #SUM
  PA1_THFC #SUM
)

Cund_Per2=select(
  Cund_Per,
  COD_ENCUESTAS
)

Cund_df_Per=(cbind(Cund_Per,dummy_Cund_Per))
Cund_df_Per<-left_join(Cund_df_Per,Cund_MGN)

Cund_df_Per=select(
  Cund_df_Per,
  -COD_ENCUESTAS
)

Cund_df_Per2<-Cund_df_Per %>% group_by(COD_DANE) %>% summarize(across(everything(), sum))

Cund_df_Per3=(cbind(Cund_Per2,dummy_Cund_Per2))
Cund_df_Per3<-left_join(Cund_df_Per3,Cund_MGN)

Cund_df_Per3=select(
  Cund_df_Per3,
  -COD_ENCUESTAS
)


Cund_df_Per4<-Cund_df_Per3 %>% group_by(COD_DANE) %>% summarize(across(everything(), sum))


Cund_df_Per5<-left_join(Cund_df_Per2,Cund_df_Per4)


saveRDS(Cund_df_Per5, file = "C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Dane_Personas.rds")

rm(list = ls())


        
