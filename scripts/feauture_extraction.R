
# LIBRERIAS ---------------------------------------------------------------


library(tidyverse)
library(stringr)
library(stringi)
library(tidytext)
library(janitor)
library(tm)
library(here)

#options(warn = 1) #en caso de que las advertencias se estén conviertiendo en errores


# CARGAR DATOS ------------------------------------------------------------



train_original <- readRDS(here("stores","train.Rds"))
test_original <- readRDS(here("stores","test.Rds"))

#variables 
glimpse(test_original)
glimpse(train_original)

# EXTRACCION DE VARIABLES DEL TITULO --------------------------------------


# Datos de entrenamiento --------------------------------------------------



#tabla de frecuencias: sirve para identificar palabras
train_original %>% 
  select(title) %>%
  unnest_tokens(.,"tokens",title) %>%
  filter(!tokens%in%stopwords("es")) %>% 
  tabyl(tokens) %>% 
  arrange(desc(n)) %>% view
  


#Nuevas variables obtenidas de description o title
train_original_dscrptin_ftrs <- train_original %>% 
  mutate(
    
    #Estrato: puede estar en descripcion y titulo
    
    title=str_remove_all(title,"[[:punct:]]") %>% stri_trans_general(.,"latin-ascii") %>% str_squish(.),
    description=str_remove_all(description,"[[:punct:]]") %>% stri_trans_general(.,"latin-ascii") %>% str_squish(.),
    
    
    estrato_title=case_when(  
      str_detect(title,"(?i)estrato\\s*socio")~str_extract(title,"(?i)(?<=estrato\\s{0,1}socioeconomico)\\s{0,1}.[^\\s]*"),
      str_detect(title,"(?i)estrato")~str_extract(title,"(?i)(?<=estrato)\\s{0,1}.[^\\s]*"),
      TRUE~""),
    
    estrato_description=case_when(  
      str_detect(description,"(?i)estrato\\s*socio")~str_extract(description,"(?i)(?<=estrato\\s{0,1}socioeconomico)\\s{0,1}.[^\\s]*"),
      str_detect(description,"(?i)estrato")~str_extract(description,"(?i)(?<=estrato)\\s{0,1}.[^\\s]*"),
      TRUE~""),
    
    estrato=ifelse(estrato_title=="",estrato_description,estrato_title),
    
    estrato_clean=case_when(
      
      str_detect(estrato,"(?i)uno")~"1",
      str_detect(estrato,"(?i)dos")~"2",
      str_detect(estrato,"(?i)tres")~"3",
      str_detect(estrato,"(?i)cuatro")~"4",
      str_detect(estrato,"(?i)cinco")~"5",
      str_detect(estrato,"(?i)seis")~"6",
      str_detect(estrato,"\\d")~str_extract(estrato,"\\d{1}"),
      TRUE~""
      
    ),
    
    estrato_clean=ifelse(estrato_clean=="",NA,as.numeric(estrato_clean)),
    

    #Terraza
    
    terraza_title=ifelse(str_detect(title,"(?i)terraz"),1,0),
    terraza_description=ifelse(str_detect(description,"(?i)terraz"),1,0),
    
    terraza=ifelse(terraza_title==0,terraza_description,terraza_title),
    
    #balcon
    
    balcon_title=ifelse(str_detect(title,"(?i)balc"),1,0),
    balcon_description=ifelse(str_detect(description,"(?i)balc"),1,0),
    
    balcon=ifelse(balcon_title==0,balcon_description,balcon_title),
    
    #garaje
    
    garaje_title=ifelse(str_detect(title,"(?i)garaj|parque[ao]"),1,0),
    garaje_description=ifelse(str_detect(description,"(?i)garaj|parque[ao]"),1,0),
    
    garaje=ifelse(garaje_title==0,garaje_description,garaje_title),
    
    #usada o nueva
    
    
    nueva_title=ifelse(str_detect(title,"(?i)\\bnuev|estren"),1,0),
    nueva_description=ifelse(str_detect(title,"(?i)\\bnuev|estren"),1,0),
    nueva=ifelse(nueva_title==0,nueva_description,nueva_title)
    
    
  ) %>% 
  select(-ends_with(c("_title","_description")))
  

train_original_dscrptin_ftrs %>% filter(nueva==0) %>% view


#Recovering surface

train_original_dscrptin_ftrs_2 <- train_original_dscrptin_ftrs %>% 
  mutate(
    surface_total_title=case_when(
      #1 de las palabras
      is.na(surface_total)&str_count(title,"(?i)[^x]\\s*\\d{2,}\\s*mt")>0~str_extract_all(title,"(?i)\\d*\\s*(?=mt)"),
      is.na(surface_total)&str_count(title,"(?i)[^x]\\s*\\d{2,}\\s*m2")>0~str_extract_all(title,"(?i)\\d*\\s*(?=m2)"),
      is.na(surface_total)&str_count(title,"(?i)[^x]\\s*\\d{2,}\\s*metros")>0~str_extract_all(title,"(?i)\\d*\\s*(?=metros)"),
      is.na(surface_total)&str_count(title,"(?i)[^x]\\s*\\d{2,}\\s*mts")>0~str_extract_all(title,"(?i)\\d*\\s*(?=mts)"),
      is.na(surface_total)&str_count(title,"(?i)[^x]\\s*\\d{2,}\\s*mts2")>0~str_extract_all(title,"(?i)\\d*\\s*(?=mts2)"),
      TRUE~list("")
      
      
    ),


    surface_total_description=case_when(
      
      is.na(surface_total)&str_count(description,"(?i)[^x]\\s*\\d{2,}\\s*mt")>0~str_extract_all(description,"(?i)\\d*\\s*(?=mt)"),
      is.na(surface_total)&str_count(description,"(?i)[^x]\\s*\\d{2,}\\s*m2")>0~str_extract_all(description,"(?i)\\d*\\s*(?=m2)"),
      is.na(surface_total)&str_count(description,"(?i)[^x]\\s*\\d{2,}\\s*metros")>0~str_extract_all(description,"(?i)\\d*\\s*(?=metros)"),
      is.na(surface_total)&str_count(description,"(?i)[^x]\\s*\\d{2,}\\s*mts")>0~str_extract_all(description,"(?i)\\d*\\s*(?=mts)"),
      is.na(surface_total)&str_count(description,"(?i)[^x]\\s*\\d{2,}\\s*mts2")>0~str_extract_all(description,"(?i)\\d*\\s*(?=mts2)"),
      TRUE~list("")
      
      
    ))


train_original_dscrptin_ftrs_2["surface_total_title_clean"] <- ""


for(i in 1:nrow(train_original_dscrptin_ftrs_2)){
  
  if(length(train_original_dscrptin_ftrs_2$surface_total_title[[i]])>1){
    
    
    
    
    train_original_dscrptin_ftrs_2[["surface_total_title_clean"]][i]=train_original_dscrptin_ftrs_2$surface_total_title[[i]] %>% str_remove_all(.,"\\D") %>% as.numeric(.) %>% max(.,na.rm=T)
    
  }else{
    
    train_original_dscrptin_ftrs_2[["surface_total_title_clean"]][i]=NA_real_
    
  }
  
  
  print(i)
  
}


train_original_dscrptin_ftrs_2["surface_total_description_clean"] <- ""


 
for(i in 1:nrow(train_original_dscrptin_ftrs_2)){
  
  if(length(train_original_dscrptin_ftrs_2$surface_total_description[[i]])>1){
    
    
    
    
    train_original_dscrptin_ftrs_2[["surface_total_description_clean"]][i]=train_original_dscrptin_ftrs_2$surface_total_description[[i]] %>% str_remove_all(.,"\\D") %>% as.numeric(.) %>% max(.,na.rm=T)
    
  }else{
    
    train_original_dscrptin_ftrs_2[["surface_total_description_clean"]][i]=NA_real_
    
  }
  
  
  print(i)
  
}


train_original_dscrptin_ftrs_2_2 <- train_original_dscrptin_ftrs_2 %>% 
  
  rowwise() %>% 
  mutate(
    
    surface_total_min=min(c_across(c("surface_total_description_clean","surface_total_title_clean")),na.rm=T)) %>% 
  ungroup() %>% 
  
  mutate(
    
    surface_total_2=ifelse(
      !is.na(surface_total),
      surface_total,
      surface_total_min
  )) %>% 
  
  
  select(-surface_total_description_clean,-surface_total_title_clean,
         -surface_total_description,-surface_total_title,
         -surface_total_min) %>%  
  rename(estrato_2=estrato_clean)


#write.csv(train_original_dscrptin_ftrs_2_2,"train_variables_descpcn_ttl.csv")


# Datos testeo ------------------------------------------------------------

#Nuevas variables obtenidas de description o title
test_original_dscrptin_ftrs <- test_original %>% 
  mutate(
    
    #Estrato: puede estar en descripcion y titulo
    
    title=str_remove_all(title,"[[:punct:]]") %>% stri_trans_general(.,"latin-ascii") %>% str_squish(.),
    description=str_remove_all(description,"[[:punct:]]") %>% stri_trans_general(.,"latin-ascii") %>% str_squish(.),
    
    
    estrato_title=case_when(  
      str_detect(title,"(?i)estrato\\s*socio")~str_extract(title,"(?i)(?<=estrato\\s{0,1}socioeconomico)\\s{0,1}.[^\\s]*"),
      str_detect(title,"(?i)estrato")~str_extract(title,"(?i)(?<=estrato)\\s{0,1}.[^\\s]*"),
      TRUE~""),
    
    estrato_description=case_when(  
      str_detect(description,"(?i)estrato\\s*socio")~str_extract(description,"(?i)(?<=estrato\\s{0,1}socioeconomico)\\s{0,1}.[^\\s]*"),
      str_detect(description,"(?i)estrato")~str_extract(description,"(?i)(?<=estrato)\\s{0,1}.[^\\s]*"),
      TRUE~""),
    
    estrato=ifelse(estrato_title=="",estrato_description,estrato_title),
    
    estrato_clean=case_when(
      
      str_detect(estrato,"(?i)uno")~"1",
      str_detect(estrato,"(?i)dos")~"2",
      str_detect(estrato,"(?i)tres")~"3",
      str_detect(estrato,"(?i)cuatro")~"4",
      str_detect(estrato,"(?i)cinco")~"5",
      str_detect(estrato,"(?i)seis")~"6",
      str_detect(estrato,"\\d")~str_extract(estrato,"\\d{1}"),
      TRUE~""
      
    ),
    
    estrato_clean=ifelse(estrato_clean=="",NA,as.numeric(estrato_clean)),
    
    
    #Terraza
    
    terraza_title=ifelse(str_detect(title,"(?i)terraz"),1,0),
    terraza_description=ifelse(str_detect(description,"(?i)terraz"),1,0),
    
    terraza=ifelse(terraza_title==0,terraza_description,terraza_title),
    
    #balcon
    
    balcon_title=ifelse(str_detect(title,"(?i)balc"),1,0),
    balcon_description=ifelse(str_detect(description,"(?i)balc"),1,0),
    
    balcon=ifelse(balcon_title==0,balcon_description,balcon_title),
    
    #garaje
    
    garaje_title=ifelse(str_detect(title,"(?i)garaj|parque[ao]"),1,0),
    garaje_description=ifelse(str_detect(description,"(?i)garaj|parque[ao]"),1,0),
    
    garaje=ifelse(garaje_title==0,garaje_description,garaje_title),
    
    #usada o nueva
    
    
    nueva_title=ifelse(str_detect(title,"(?i)\\bnuev|estren"),1,0),
    nueva_description=ifelse(str_detect(title,"(?i)\\bnuev|estren"),1,0),
    nueva=ifelse(nueva_title==0,nueva_description,nueva_title)
    
    
  ) %>% 
  select(-ends_with(c("_title","_description")))




#Recovering surface

test_original_dscrptin_ftrs_2 <- test_original_dscrptin_ftrs %>% 
  mutate(
    surface_total_title=case_when(
      #1 de las palabras
      is.na(surface_total)&str_count(title,"(?i)[^x]\\s*\\d{2,}\\s*mt")>0~str_extract_all(title,"(?i)\\d*\\s*(?=mt)"),
      is.na(surface_total)&str_count(title,"(?i)[^x]\\s*\\d{2,}\\s*m2")>0~str_extract_all(title,"(?i)\\d*\\s*(?=m2)"),
      is.na(surface_total)&str_count(title,"(?i)[^x]\\s*\\d{2,}\\s*metros")>0~str_extract_all(title,"(?i)\\d*\\s*(?=metros)"),
      is.na(surface_total)&str_count(title,"(?i)[^x]\\s*\\d{2,}\\s*mts")>0~str_extract_all(title,"(?i)\\d*\\s*(?=mts)"),
      is.na(surface_total)&str_count(title,"(?i)[^x]\\s*\\d{2,}\\s*mts2")>0~str_extract_all(title,"(?i)\\d*\\s*(?=mts2)"),
      TRUE~list("")
      
      
    ),
    
    
    surface_total_description=case_when(
      
      is.na(surface_total)&str_count(description,"(?i)[^x]\\s*\\d{2,}\\s*mt")>0~str_extract_all(description,"(?i)\\d*\\s*(?=mt)"),
      is.na(surface_total)&str_count(description,"(?i)[^x]\\s*\\d{2,}\\s*m2")>0~str_extract_all(description,"(?i)\\d*\\s*(?=m2)"),
      is.na(surface_total)&str_count(description,"(?i)[^x]\\s*\\d{2,}\\s*metros")>0~str_extract_all(description,"(?i)\\d*\\s*(?=metros)"),
      is.na(surface_total)&str_count(description,"(?i)[^x]\\s*\\d{2,}\\s*mts")>0~str_extract_all(description,"(?i)\\d*\\s*(?=mts)"),
      is.na(surface_total)&str_count(description,"(?i)[^x]\\s*\\d{2,}\\s*mts2")>0~str_extract_all(description,"(?i)\\d*\\s*(?=mts2)"),
      TRUE~list("")
      
      
    ))


test_original_dscrptin_ftrs_2["surface_total_title_clean"] <- ""


for(i in 1:nrow(test_original_dscrptin_ftrs_2)){
  
  if(length(test_original_dscrptin_ftrs_2$surface_total_title[[i]])>1){
    
    
    
    
    test_original_dscrptin_ftrs_2[["surface_total_title_clean"]][i]=test_original_dscrptin_ftrs_2$surface_total_title[[i]] %>% str_remove_all(.,"\\D") %>% as.numeric(.) %>% max(.,na.rm=T)
    
  }else{
    
    test_original_dscrptin_ftrs_2[["surface_total_title_clean"]][i]=NA_real_
    
  }
  
  
  print(i)
  
}


test_original_dscrptin_ftrs_2["surface_total_description_clean"] <- ""



for(i in 1:nrow(test_original_dscrptin_ftrs_2)){
  
  if(length(test_original_dscrptin_ftrs_2$surface_total_description[[i]])>1){
    
    
    
    
    test_original_dscrptin_ftrs_2[["surface_total_description_clean"]][i]=test_original_dscrptin_ftrs_2$surface_total_description[[i]] %>% str_remove_all(.,"\\D") %>% as.numeric(.) %>% max(.,na.rm=T)
    
  }else{
    
    test_original_dscrptin_ftrs_2[["surface_total_description_clean"]][i]=NA_real_
    
  }
  
  
  print(i)
  
}


test_original_dscrptin_ftrs_2_2 <- test_original_dscrptin_ftrs_2 %>% 
  
  rowwise() %>% 
  mutate(
    
    surface_total_min=min(c_across(c("surface_total_description_clean","surface_total_title_clean")),na.rm=T)) %>% 
  
  ungroup() %>% 
  
  mutate(
    
    surface_total_2=ifelse(
      !is.na(surface_total),
      surface_total,
      surface_total_min
    )) %>% 
  
  
  select(-surface_total_description_clean,-surface_total_title_clean,
         -surface_total_description,-surface_total_title,
         -surface_total_min) %>%  
  rename(estrato_2=estrato_clean)


#counting nas
colSums(is.na(test_original_dscrptin_ftrs_2_2))

#write.csv(test_original_dscrptin_ftrs_2_2,"test_variables_descpcn_ttl.csv")


# IMPUTACION --------------------------------------------------------------

#manzanas (por limite del repositorio): https://drive.google.com/drive/folders/1KvvhbJi3YKJuAu2zNy6j1jBog7LXfsE1?usp=share_link
manzanas <- st_read(here("stores","MGN_URB_MANZANA.shp"))



manzanas_med <- manzanas %>% filter(COD_MPIO%in%"05001")
manzanas_cali <- manzanas %>% filter(COD_MPIO%in%"76001")
manzanas_bog <- manzanas %>% filter(COD_MPIO%in%"11001")



getmode <- function(v) {
  uniqv <- unique(v)[!is.na(unique(v))]
  v[which.max(tabulate(match(v, uniqv)))]
}


# Bogotá ------------------------------------------------------------------


train_original_dscrptin_ftrs_2_2_bog <- train_original_dscrptin_ftrs_2_2 %>% 
  filter(city%in%"Bogotá D.C") %>% 
  st_as_sf(.,
           coords=c("lon","lat"),
           crs=crs(manzanas_bog)) %>% 
  st_transform(4686)

    
train_original_dscrptin_ftrs_2_2_bog_mnz <- st_join(manzanas_bog,train_original_dscrptin_ftrs_2_2_bog) %>% 
  filter(!is.na(property_id))


train_original_dscrptin_ftrs_2_2_bog_mnz_imp <- train_original_dscrptin_ftrs_2_2_bog_mnz %>% 
  mutate_at(vars(c(surface_total_2,surface_covered,estrato_2,
                    bathrooms,bedrooms,rooms,
                    terraza,balcon,garaje,nueva)),as.numeric) %>% 
  group_by(COD_DANE) %>% 
  mutate(
    
    imp_surface_total_2=mean(surface_total_2,na.rm=T),
    imp_surface_covered=mean(surface_covered,na.rm=T),
    imp_estrato_2=getmode(estrato_2),
    imp_bathrooms=getmode(bathrooms),
    imp_bedrooms=getmode(bedrooms),
    imp_rooms=getmode(rooms),
    imp_property_type=getmode(property_type),
    imp_terraza=getmode(terraza),
    imp_balcon=getmode(balcon),
    imp_garaje=getmode(garaje),
    imp_nueva=getmode(nueva)
    
    
  )


train_original_dscrptin_ftrs_2_2_bog_mnz_imp_2 <- train_original_dscrptin_ftrs_2_2_bog_mnz_imp %>% 
  mutate(
    surface_total_2_imp=ifelse(is.na(surface_total_2),
                               imp_surface_total_2,
                               surface_total_2),
    
    surface_covered_imp=ifelse(is.na(surface_covered),
                               imp_surface_covered,
                               surface_covered),
    
    estrato_2_imp=ifelse(is.na(estrato_2),
                               imp_estrato_2,
                               estrato_2),
    
    bathrooms_imp=ifelse(is.na(bathrooms),
                         imp_bathrooms,
                         bathrooms),
    
    bedrooms_imp=ifelse(is.na(bedrooms),
                         imp_bedrooms,
                         bedrooms),
    
    
    rooms_imp=ifelse(is.na(rooms),
                        imp_rooms,
                        rooms),
    
    property_type_imp=ifelse(is.na(property_type),
                             imp_property_type,
                     property_type),
    
    terraza_imp=ifelse(is.na(terraza),
                             imp_terraza,
                             terraza),
    
    balcon_imp=ifelse(is.na(balcon),
                       imp_balcon,
                       balcon),
    
    garaje_imp=ifelse(is.na(garaje),
                      imp_garaje,
                      garaje),
    
    nueva_imp=ifelse(is.na(nueva),
                      imp_nueva,
                      nueva)
    
    
  ) %>% 
  select(-starts_with("imp"))




#combinar con no imputados

train_original_dscrptin_ftrs_2_2_bog_noimp <- train_original_dscrptin_ftrs_2_2_bog %>% 
  filter(
  !property_id%in%train_original_dscrptin_ftrs_2_2_bog_mnz_imp_2$property_id) %>% 
  mutate_at(vars(c(surface_total_2,surface_covered,estrato_2,
                   bathrooms,bedrooms,rooms,
                   terraza,balcon,garaje,nueva)),as.numeric)


train_original_dscrptin_ftrs_2_2_bog_mnz_imp_noimp <- train_original_dscrptin_ftrs_2_2_bog_mnz_imp_2 %>% 
  as_tibble() %>% 
  bind_rows(train_original_dscrptin_ftrs_2_2_bog_noimp)%>% 
  select(property_id,
         price,
         city,
         contains("imp")
         
  )



  

saveRDS(train_original_dscrptin_ftrs_2_2_bog_mnz_imp_noimp,"train_original_dscrptin_ftrs_2_2_bog_mnz_imp_noimp.Rds")

  
#write.csv(train_original_dscrptin_ftrs_2_2_bog_mnz_imp_2,"train_variables_descpcn_ttl_bog.csv")

# Medellin ----------------------------------------------------------------

train_original_dscrptin_ftrs_2_2_med <- train_original_dscrptin_ftrs_2_2 %>% 
  filter(city%in%"Medellín") %>% 
  st_as_sf(.,
           coords=c("lon","lat"),
           crs=crs(manzanas_med)) %>% 
  st_transform(4686)

train_original_dscrptin_ftrs_2_2_med_mnz <- st_join(manzanas_med,train_original_dscrptin_ftrs_2_2_med)


train_original_dscrptin_ftrs_2_2_med_mnz_imp <- train_original_dscrptin_ftrs_2_2_med_mnz %>% 
  mutate_at(vars(c(surface_total_2,surface_covered,estrato_2,
                   bathrooms,bedrooms,rooms,
                   terraza,balcon,garaje,nueva)),as.numeric) %>% 
  group_by(COD_DANE) %>% 
  mutate(
    
    imp_surface_total_2=mean(surface_total_2,na.rm=T),
    imp_surface_covered=mean(surface_covered,na.rm=T),
    imp_estrato_2=getmode(estrato_2),
    imp_bathrooms=getmode(bathrooms),
    imp_bedrooms=getmode(bedrooms),
    imp_rooms=getmode(rooms),
    imp_property_type=getmode(property_type),
    imp_terraza=getmode(terraza),
    imp_balcon=getmode(balcon),
    imp_garaje=getmode(garaje),
    imp_nueva=getmode(nueva)
    
    
  )


train_original_dscrptin_ftrs_2_2_med_mnz_imp_2 <- train_original_dscrptin_ftrs_2_2_med_mnz_imp %>% 
  mutate(
    surface_total_2_imp=ifelse(is.na(surface_total_2),
                               imp_surface_total_2,
                               surface_total_2),
    
    surface_covered_imp=ifelse(is.na(surface_covered),
                               imp_surface_covered,
                               surface_covered),
    
    estrato_2_imp=ifelse(is.na(estrato_2),
                         imp_estrato_2,
                         estrato_2),
    
    bathrooms_imp=ifelse(is.na(bathrooms),
                         imp_bathrooms,
                         bathrooms),
    
    bedrooms_imp=ifelse(is.na(bedrooms),
                        imp_bedrooms,
                        bedrooms),
    
    
    rooms_imp=ifelse(is.na(rooms),
                     imp_rooms,
                     rooms),
    
    property_type_imp=ifelse(is.na(property_type),
                             imp_property_type,
                             property_type),
    
    terraza_imp=ifelse(is.na(terraza),
                       imp_terraza,
                       terraza),
    
    balcon_imp=ifelse(is.na(balcon),
                      imp_balcon,
                      balcon),
    
    garaje_imp=ifelse(is.na(garaje),
                      imp_garaje,
                      garaje),
    
    nueva_imp=ifelse(is.na(nueva),
                     imp_nueva,
                     nueva)
    
    
  ) %>% 
  select(-starts_with("imp"))


#combinar con no imputados

train_original_dscrptin_ftrs_2_2_med_noimp <- train_original_dscrptin_ftrs_2_2_med %>% 
  filter(
    !property_id%in%train_original_dscrptin_ftrs_2_2_med_mnz_imp_2$property_id) %>% 
  mutate_at(vars(c(surface_total_2,surface_covered,estrato_2,
                   bathrooms,bedrooms,rooms,
                   terraza,balcon,garaje,nueva)),as.numeric)


train_original_dscrptin_ftrs_2_2_med_mnz_imp_noimp <- train_original_dscrptin_ftrs_2_2_med_mnz_imp_2 %>% 
  as_tibble() %>% 
  bind_rows(train_original_dscrptin_ftrs_2_2_med_noimp)%>% 
  select(property_id,
         price,
         city,
         contains("imp")
         
  )





saveRDS(train_original_dscrptin_ftrs_2_2_med_mnz_imp_noimp,"train_original_dscrptin_ftrs_2_2_med_mnz_imp_noimp.Rds")


#write.csv(train_original_dscrptin_ftrs_2_2_med_mnz_imp_2,"train_variables_descrpcn_ttl_med.csv")

# CALI --------------------------------------------------------------------


manzanas_cali <- manzanas_cali %>% st_transform(4686)


test_original_dscrptin_ftrs_2_2_cali <- test_original_dscrptin_ftrs_2_2 %>% 
  st_as_sf(.,
           coords=c("lon","lat"),
           crs=crs(manzanas_cali)) %>% 
  st_transform(4686)

test_original_dscrptin_ftrs_2_2_cali_mnz <- st_join(manzanas_cali,
                                                    test_original_dscrptin_ftrs_2_2_cali
                                                    
                                                    ) %>% 
  filter(!is.na(property_id))


test_original_dscrptin_ftrs_2_2_cali_mnz_imp <- test_original_dscrptin_ftrs_2_2_cali_mnz %>% 
  mutate_at(vars(c(surface_total_2,surface_covered,estrato_2,
                   bathrooms,bedrooms,rooms,
                   terraza,balcon,garaje,nueva)),as.numeric) %>% 
  group_by(COD_DANE) %>% 
  mutate(
    
    imp_surface_total_2=mean(surface_total_2,na.rm=T),
    imp_surface_covered=mean(surface_covered,na.rm=T),
    imp_estrato_2=getmode(estrato_2),
    imp_bathrooms=getmode(bathrooms),
    imp_bedrooms=getmode(bedrooms),
    imp_rooms=getmode(rooms),
    imp_property_type=getmode(property_type),
    imp_terraza=getmode(terraza),
    imp_balcon=getmode(balcon),
    imp_garaje=getmode(garaje),
    imp_nueva=getmode(nueva)
    
    
  )


test_original_dscrptin_ftrs_2_2_cali_mnz_imp_2 <- test_original_dscrptin_ftrs_2_2_cali_mnz_imp %>% 
  mutate(
    surface_total_2_imp=ifelse(is.na(surface_total_2),
                               imp_surface_total_2,
                               surface_total_2),
    
    surface_covered_imp=ifelse(is.na(surface_covered),
                               imp_surface_covered,
                               surface_covered),
    
    estrato_2_imp=ifelse(is.na(estrato_2),
                         imp_estrato_2,
                         estrato_2),
    
    bathrooms_imp=ifelse(is.na(bathrooms),
                         imp_bathrooms,
                         bathrooms),
    
    bedrooms_imp=ifelse(is.na(bedrooms),
                        imp_bedrooms,
                        bedrooms),
    
    
    rooms_imp=ifelse(is.na(rooms),
                     imp_rooms,
                     rooms),
    
    property_type_imp=ifelse(is.na(property_type),
                             imp_property_type,
                             property_type),
    
    terraza_imp=ifelse(is.na(terraza),
                       imp_terraza,
                       terraza),
    
    balcon_imp=ifelse(is.na(balcon),
                      imp_balcon,
                      balcon),
    
    garaje_imp=ifelse(is.na(garaje),
                      imp_garaje,
                      garaje),
    
    nueva_imp=ifelse(is.na(nueva),
                     imp_nueva,
                     nueva)
    
    
  ) %>% 
  select(-starts_with("imp"))


test_original_dscrptin_ftrs_2_2_cali_noimp <- test_original_dscrptin_ftrs_2_2_cali %>% filter(
  !property_id%in%test_original_dscrptin_ftrs_2_2_cali_mnz_imp_2$property_id) %>% 
  mutate_at(vars(c(surface_total_2,surface_covered,estrato_2,
                   bathrooms,bedrooms,rooms,
                   terraza,balcon,garaje,nueva)),as.numeric)


test_original_dscrptin_ftrs_2_2_cali_mnz_imp_noimp <- test_original_dscrptin_ftrs_2_2_cali_mnz_imp_2 %>% 
  as_tibble() %>% 
  bind_rows(test_original_dscrptin_ftrs_2_2_cali_noimp) %>% 
  select(property_id,
         price,
         city,
         contains("imp")
         
         )


#write.csv(test_original_dscrptin_ftrs_2_2_cali_mnz_imp_2,"test_variables_descrpcn_ttl.csv")


saveRDS(test_original_dscrptin_ftrs_2_2_cali_mnz_imp_noimp,"test_original_dscrptin_ftrs_2_2_cali_mnz_imp_noimp.Rds")
