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


#1.1. Leer información de manzanas--------------------------------------

Manzanas_Dane <- st_read("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/MGN_URB_MANZANA.shp")
Barrio_Bta <- st_read("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/SECTOR.shp")
Barrio_Mll <- st_read("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/BarrioVereda_2014.shp")
Barrio_Vall <- st_read("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/mc_barrios.shp")

#Barrio_Bta2=st_transform(Barrio_Bta, 4686)
Barrio_Mll=st_transform(Barrio_Mll, 4686)
Barrio_Vall=st_transform(Barrio_Vall, 4686)

Dane_Hogar <- readRDS(here("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Dane_Hogar.rds"))
Dane_Vivienda <- readRDS(here("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Dane_Vivienda.rds"))
Dane_Personas <- readRDS(here("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/Dane_Personas.rds"))

Train <- readRDS(here("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/datos/dataPS3/train.rds"))
Test <- readRDS(here("C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/datos/dataPS3/test.rds"))

View(Train)
# 1.2. Asiganción de los Barrios de Bogota

sf::sf_use_s2(FALSE)

sites_Bogota=select(
  Train,
  property_id,city,lon,lat
)  %>% filter(city=="Bogotá D.C")

sites=select(
  sites_Bogota,
  lon,lat
)  

ames_sf <- sf::st_as_sf(
  sites,
  coords = c("lon", "lat"),
  crs = 4686
)

Barrio_casa=as.data.frame(st_contains(Barrio_Bta,ames_sf))

Barrio_id=as.data.frame(Barrio_Bta)['SCANOMBRE']
Barrio_id$Barrio_id <- row.names(Barrio_id)  
names(Barrio_casa)[1]='Barrio_id'
names(Barrio_casa)[2]='casa_id'

Barrio_id['Barrio_id'] <- lapply(Barrio_id['Barrio_id'], as.numeric)
Barrio_casa=left_join(Barrio_casa,Barrio_id)


sites_Bogota2=as.data.frame(sites_Bogota['property_id'])
sites_Bogota2$Property_id <- row.names(sites_Bogota2)
names(sites_Bogota2)[2]='casa_id'

sites_Bogota2['casa_id'] <- lapply(sites_Bogota2['casa_id'], as.numeric)

sites_Bogota3=left_join(sites_Bogota2,Barrio_casa)



# 1.3. Asiganción de los Barrios de Medellin

sf::sf_use_s2(FALSE)

sites_Medellin=select(
  Train,
  property_id,city,lon,lat
)  %>% filter(city=="Medellín")

sites=select(
  sites_Medellin,
  lon,lat
)  

ames_sf <- sf::st_as_sf(
  sites,
  coords = c("lon", "lat"),
  crs = 4686
)

Barrio_casa=as.data.frame(st_contains(Barrio_Mll,ames_sf))
Barrio_id=as.data.frame(Barrio_Mll)['SCANOMBRE']
Barrio_id$Barrio_id <- row.names(Barrio_id)  
names(Barrio_casa)[1]='Barrio_id'
names(Barrio_casa)[2]='casa_id'

Barrio_id['Barrio_id'] <- lapply(Barrio_id['Barrio_id'], as.numeric)
Barrio_casa=left_join(Barrio_casa,Barrio_id)


sites_Medellin2=as.data.frame(sites_Medellin['property_id'])
sites_Medellin2$Property_id <- row.names(sites_Medellin2)
names(sites_Medellin2)[2]='casa_id'

sites_Medellin2['casa_id'] <- lapply(sites_Medellin2['casa_id'], as.numeric)

sites_Medellin3=left_join(sites_Medellin2,Barrio_casa)



# 1.4. Asiganción de los Barrios de Cali

sf::sf_use_s2(FALSE)

sites_Cali=select(
  Test,
  property_id,city,lon,lat
)  %>% filter(city=="Cali")

sites=select(
  sites_Cali,
  lon,lat
)  

ames_sf <- sf::st_as_sf(
  sites,
  coords = c("lon", "lat"),
  crs = 4686
)

Barrio_casa=as.data.frame(st_contains(Barrio_Vall,ames_sf))
Barrio_id=as.data.frame(Barrio_Vall)['SCANOMBRE']
Barrio_id$Barrio_id <- row.names(Barrio_id)  
names(Barrio_casa)[1]='Barrio_id'
names(Barrio_casa)[2]='casa_id'

Barrio_id['Barrio_id'] <- lapply(Barrio_id['Barrio_id'], as.numeric)
Barrio_casa=left_join(Barrio_casa,Barrio_id)


sites_Cali2=as.data.frame(sites_Cali['property_id'])
sites_Cali2$Property_id <- row.names(sites_Cali2)
names(sites_Cali2)[2]='casa_id'

sites_Cali2['casa_id'] <- lapply(sites_Cali2['casa_id'], as.numeric)

sites_Cali3=left_join(sites_Cali2,Barrio_casa)

BARIOS=union(sites_Bogota3,sites_Medellin3,sites_Cali3)


# 1.5. Asignación de Censo Bogota

sf::sf_use_s2(FALSE)

sites_Bogota=select(
  Train,
  property_id,city,lon,lat
)  %>% filter(city=="Bogotá D.C")

sites=select(
  sites_Bogota,
  lon,lat
)  

ames_sf <- sf::st_as_sf(
  sites,
  coords = c("lon", "lat"),
  crs = 4686
)

Dane_Id_Bogota=as.data.frame(st_contains(Manzanas_Dane,ames_sf))

sites['lon']=sites['lon']+0.0006
sites['lat']=sites['lat']+0.0006

ames_sf <- sf::st_as_sf(
  sites,
  coords = c("lon", "lat"),
  crs = 4686
)

Dane_Id_Bogota2=as.data.frame(st_contains(Manzanas_Dane,ames_sf))

sites['lon']=sites['lon']-0.0024
sites['lat']=sites['lat']-0.0024

ames_sf <- sf::st_as_sf(
  sites,
  coords = c("lon", "lat"),
  crs = 4686
)

Dane_Id_Bogota3=as.data.frame(st_contains(Manzanas_Dane,ames_sf))


Dane_Id_Bogota2=Dane_Id_Bogota2 %>%
                filter(!(Dane_Id_Bogota2$col.id %in% Dane_Id_Bogota$col.id))

Dane_Id_Bogota=union(Dane_Id_Bogota,Dane_Id_Bogota2)

Dane_Id_Bogota3=Dane_Id_Bogota3 %>%
  filter(!(Dane_Id_Bogota3$col.id %in% Dane_Id_Bogota$col.id))

Barrio_casa=union(Dane_Id_Bogota,Dane_Id_Bogota3)

Barrio_id=as.data.frame(Manzanas_Dane)['COD_DANE']
Barrio_id$Barrio_id <- row.names(Barrio_id)  
names(Barrio_casa)[1]='Barrio_id'
names(Barrio_casa)[2]='casa_id'

Barrio_id['Barrio_id'] <- lapply(Barrio_id['Barrio_id'], as.numeric)
Barrio_casa=left_join(Barrio_casa,Barrio_id)


sites_Bogota2=as.data.frame(sites_Bogota['property_id'])
sites_Bogota2$Property_id <- row.names(sites_Bogota2)
names(sites_Bogota2)[2]='casa_id'

sites_Bogota2['casa_id'] <- lapply(sites_Bogota2['casa_id'], as.numeric)

COD_DANE_Bogota=left_join(sites_Bogota2,Barrio_casa)


# 1.6. Asignación de Censo Medellin

sf::sf_use_s2(FALSE)

sites_Medellin=select(
  Train,
  property_id,city,lon,lat
)  %>% filter(city=="Medellín")

sites=select(
  sites_Medellin,
  lon,lat
)  

ames_sf <- sf::st_as_sf(
  sites,
  coords = c("lon", "lat"),
  crs = 4686
)

Dane_Id_Medellin=as.data.frame(st_contains(Manzanas_Dane,ames_sf))

sites['lon']=sites['lon']+0.0006
sites['lat']=sites['lat']+0.0006

ames_sf <- sf::st_as_sf(
  sites,
  coords = c("lon", "lat"),
  crs = 4686
)

Dane_Id_Medellin2=as.data.frame(st_contains(Manzanas_Dane,ames_sf))

sites['lon']=sites['lon']-0.0018
sites['lat']=sites['lat']-0.0018

ames_sf <- sf::st_as_sf(
  sites,
  coords = c("lon", "lat"),
  crs = 4686
)

Dane_Id_Medellin3=as.data.frame(st_contains(Manzanas_Dane,ames_sf))


Dane_Id_Medellin2=Dane_Id_Medellin2 %>%
  filter(!(Dane_Id_Medellin2$col.id %in% Dane_Id_Medellin$col.id))

Dane_Id_Medellin=union(Dane_Id_Medellin,Dane_Id_Medellin2)

Dane_Id_Medellin3=Dane_Id_Medellin3 %>%
  filter(!(Dane_Id_Medellin3$col.id %in% Dane_Id_Medellin$col.id))

Barrio_casa=union(Dane_Id_Medellin,Dane_Id_Medellin3)

Barrio_id=as.data.frame(Manzanas_Dane)['COD_DANE']
Barrio_id$Barrio_id <- row.names(Barrio_id)  
names(Barrio_casa)[1]='Barrio_id'
names(Barrio_casa)[2]='casa_id'

Barrio_id['Barrio_id'] <- lapply(Barrio_id['Barrio_id'], as.numeric)
Barrio_casa=left_join(Barrio_casa,Barrio_id)

sites_Medellin2=as.data.frame(sites_Medellin['property_id'])
sites_Medellin2$Property_id <- row.names(sites_Medellin2)
names(sites_Medellin2)[2]='casa_id'

sites_Medellin2['casa_id'] <- lapply(sites_Medellin2['casa_id'], as.numeric)

COD_DANE_Medellin=left_join(sites_Medellin2,Barrio_casa)


# 1.7. Asignación de Censo Cali

sf::sf_use_s2(FALSE)

sites_Cali=select(
  Test,
  property_id,city,lon,lat
)  %>% filter(city=="Cali")

sites=select(
  sites_Cali,
  lon,lat
)  

ames_sf <- sf::st_as_sf(
  sites,
  coords = c("lon", "lat"),
  crs = 4686
)

Dane_Id_Cali=as.data.frame(st_contains(Manzanas_Dane,ames_sf))

sites['lon']=sites['lon']+0.0006
sites['lat']=sites['lat']+0.0006

ames_sf <- sf::st_as_sf(
  sites,
  coords = c("lon", "lat"),
  crs = 4686
)

Dane_Id_Cali2=as.data.frame(st_contains(Manzanas_Dane,ames_sf))

sites['lon']=sites['lon']-0.0024
sites['lat']=sites['lat']-0.0024

ames_sf <- sf::st_as_sf(
  sites,
  coords = c("lon", "lat"),
  crs = 4686
)

Dane_Id_Cali3=as.data.frame(st_contains(Manzanas_Dane,ames_sf))


Dane_Id_Cali2=Dane_Id_Cali2 %>%
  filter(!(Dane_Id_Cali2$col.id %in% Dane_Id_Cali$col.id))

Dane_Id_Cali=union(Dane_Id_Cali,Dane_Id_Cali2)

Dane_Id_Cali3=Dane_Id_Cali3 %>%
  filter(!(Dane_Id_Cali3$col.id %in% Dane_Id_Cali$col.id))

Barrio_casa=union(Dane_Id_Cali,Dane_Id_Cali3)

Barrio_id=as.data.frame(Manzanas_Dane)['COD_DANE']
Barrio_id$Barrio_id <- row.names(Barrio_id)  
names(Barrio_casa)[1]='Barrio_id'
names(Barrio_casa)[2]='casa_id'

Barrio_id['Barrio_id'] <- lapply(Barrio_id['Barrio_id'], as.numeric)
Barrio_casa=left_join(Barrio_casa,Barrio_id)

sites_Cali2=as.data.frame(sites_Cali['property_id'])
sites_Cali2$Property_id <- row.names(sites_Cali2)
names(sites_Cali2)[2]='casa_id'

sites_Cali2['casa_id'] <- lapply(sites_Cali2['casa_id'], as.numeric)

COD_DANE_Cali=left_join(sites_Cali2,Barrio_casa)

#1.7 Integración de toda la información

names(COD_DANE_Bogota)


POC_ALL=union(select(COD_DANE_Bogota,property_id,COD_DANE),select(COD_DANE_Medellin,property_id,COD_DANE),select(COD_DANE_Cali,property_id,COD_DANE))

BARIOS=select(BARIOS,property_id,SCANOMBRE)
names(BARIOS)[2]='BARRIO'

POC_ALL=left_join(POC_ALL,Dane_Hogar)
POC_ALL=left_join(POC_ALL,Dane_Vivienda)
POC_ALL=left_join(POC_ALL,Dane_Personas)
POC_ALL=left_join(POC_ALL,BARIOS)

saveRDS(POC_ALL, file = "C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/POC_ALL.rds")
write.csv(POC_ALL,"C:/Users/ABI/Documents/1_Maestria/5_Machine_Learning/R_Andes/Taller_3/POC_ALL.csv", row.names = FALSE)




















#sites2 <- data.frame(longitude = c(-75.542589,-75.542589,-75.5425 ), latitude = c(6.3012,6.30,6.2995))


#ames_sf <- sf::st_as_sf(
#  sites2,
#  coords = c("longitude", "latitude"),
#  crs = 4686
#)

#lon_min <- -74.15
#lon_max <- -74.14
#lat_min <- 4.65
#lat_max <- 4.64

#ggplot()+
#  geom_sf(data=Bogota) +
#  theme_bw() +
#  theme(axis.title =element_blank(),
#        panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
#        axis.text = element_text(size=6)) +
#  geom_point(data = xx, aes(x = lon, y = lat), size = 1, 
#             shape = 23, fill = "darkred") +
#  coord_sf(xlim=c(lon_min, lon_max), ylim=c(lat_min, lat_max))




#ggplot()+
#  geom_sf(data=Barrio_Bta) +
#  theme_bw() +
#  theme(axis.title =element_blank(),
#        panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
#        axis.text = element_text(size=6)) +
#  geom_point(data = xx, aes(x = lon, y = lat), size = 1, 
#             shape = 23, fill = "darkred") +
#  coord_sf(xlim=c(lon_min, lon_max), ylim=c(lat_min, lat_max))




