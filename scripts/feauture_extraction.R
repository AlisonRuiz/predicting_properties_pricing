
# LIBRERIAS ---------------------------------------------------------------


library(tidyverse)
library(stringr)
library(stringi)
library(tidytext)
library(janitor)
library(tm)
library(here)




# CARGAR DATOS ------------------------------------------------------------


train_original <- readRDS(here("stores","train.Rds"))
test_original <- readRDS(here("stores","test.Rds"))

#variables 
glimpse(test_original)
glimpse(train_original)

# EXTRACCION DE VARIABLES DEL TITULO --------------------------------------

#tabla de frecuencias: sirve para identificar palabras
train_original %>% 
  select(title) %>%
  unnest_tokens(.,"tokens",title) %>%
  filter(!tokens%in%stopwords("es")) %>% 
  tabyl(tokens) %>% 
  arrange(desc(n)) %>% view
  


#ubicacion: norte, poblado (barrio lujoso)


#acceso: piso, ascensor

#terraza

#usada o nueva o remodelada

#garaje

#estrato

#Nuevas variables obtenidas de description o title
train_original_dscrptin_ftrs <- train_original %>% 
  mutate(
    
    #Estrato: puede estar en descripcion y titulo
    
    title=str_remove_all(title,"[[:punct:]]") %>% stri_trans_general(.,"latin-ascii") %>% str_squish(.),
    description=str_remove_all(description,"[[:punct:]]") %>% stri_trans_general(.,"latin-ascii") %>% str_squish(.),
    
    
    estrato_title=case_when(  
      str_detect(title,"(?i)estrato")~str_extract(title,"(?i)(?<=estrato)\\s{0,1}.[^\\s]*"),
      str_detect(title,"(?i)estrato\\s*socio")~str_extract(title,"(?i)(?<=estrato\\s{0,1}socioeconomico)\\s{0,1}.[^\\s]*"),
      TRUE~""),
    
    estrato_description=case_when(  
      str_detect(description,"(?i)estrato")~str_extract(description,"(?i)(?<=estrato)\\s{0,1}.[^\\s]*"),
      str_detect(description,"(?i)estrato\\s*socio")~str_extract(description,"(?i)(?<=estrato\\s{0,1}socioeconomico)\\s{0,1}.[^\\s]*"),
      TRUE~""),
    
    estrato=ifelse(estrato_title=="",estrato_description,estrato_title),
    
    #Terraza
    
    terraza_title=ifelse(str_detect(title,"(?i)terraza"),1,0),
    terraza_description=ifelse(str_detect(description,"(?i)terraza"),1,0),
    
    terraza=ifelse(terraza_title==0,terraza_description,terraza_title),
    
    #balcon
    
    balcon_title=ifelse(str_detect(title,"(?i)balc"),1,0),
    balcon_description=ifelse(str_detect(description,"(?i)balc"),1,0),
    
    balcon=ifelse(balcon_title==0,balcon_description,balcon_title),
    
    #garaje
    
    garaje_title=ifelse(str_detect(title,"(?i)garaj|parque[ao]"),1,0),
    garaje_description=ifelse(str_detect(description,"(?i)parque[ao]"),1,0),
    
    garaje=ifelse(garaje_title==0,garaje_description,garaje_title),
    
    #usada o nueva
    
    
    nueva_title=ifelse(str_detect(title,"(?i)\\bnuev|estren"),1,0),
    nueva_description=ifelse(str_detect(title,"(?i)\\bnuev|estren"),1,0),
    nueva=ifelse(nueva_title==0,nueva_description,nueva_title)
    
    
  ) %>% 
  select(-ends_with(c("_title","_description")))
  

# Calculo de distancias mínima de propiedades a amenities
distance_amenity_calculator <- function(amenity_variable){
  # amenities en medellín
  osm_med = opq(bbox = getbb("Medellin")) %>%
    add_osm_feature(key="amenity", value=amenity_variable) 
  class(osm_med)
  # amenities en bogotá
  osm_bog = opq(bbox = getbb("Bogotá")) %>%
    add_osm_feature(key="amenity", value=amenity_variable) 
  class(osm_bog)
  # coordenadas de cada propiedad
  houses <- st_as_sf(x = train_original_dscrptin_ftrs, 
                     coords=c("lon","lat"), 
                     crs=4326) 
  # cálculo de distancia mínima de amenities de medellín con las propiedades
  osm_sf_med = osm_med %>% osmdata_sf()
  amenitie_location_med = osm_sf_med$osm_points %>% select(osm_id,amenity) 
  matrix_dist_med <- st_distance(x=houses , y=amenitie_location_med)
  min_dist_med <- apply(matrix_dist_med , 1 , min)
  # cálculo de distancia mínima de amenities de bogotá con las propiedades
  osm_sf_bog = osm_bog %>% osmdata_sf()
  amenitie_location_bog = osm_sf_bog$osm_points %>% select(osm_id,amenity) 
  matrix_dist_bog <- st_distance(x=houses , y=amenitie_location_bog)
  min_dist_bog <- apply(matrix_dist_bog , 1 , min)
  # creación de data frame con datos de distancia minimos calculados en medellín y bogotá
  global_dist <- data.frame(a=c(min_dist_med), b=c(min_dist_bog))
  # distancia mínima entre las columnas del data frame (medellín y bogotá)
  min_dist <- apply(global_dist,1, min)
  return(min_dist)
}
# amenities
amenities_variables = c("bus_station", "school", "university", "police", "bar", "hospital", "kindergarten", "waste_disposal", "cinema", "prison", "marketplace", "restaurant")
# se llama la función de cálculo de distancia para cada amenity y se almacena en columnas nuevas
for(amenity_variable in amenities_variables){
  min_dist <- distance_amenity_calculator(amenity_variable = amenity_variable)
  train_original_dscrptin_ftrs[[amenity_variable]] = min_dist
}


