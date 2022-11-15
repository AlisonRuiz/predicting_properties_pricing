
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



# FUNCIONES DE PERDIDA ----------------------------------------------------




#FUNCION PARA TUNEAR HIPERPARAMETROS
pct_propcomp_valprop <- function (data, lev = NULL, model = NULL) {
  
  propiedades_comp=rep(0,nrow(data))
  total_valor_propiedades=sum(data$pred)
  
  total_valor_propiedades_obs=sum(data$obs,na.rm=T)
  
  
  for(i in 1:nrow(data)){
    
    if(!is.na(data$pred[i])&(data$obs[i]-data$pred[i]>40000000)){
      propiedades_comp[i]=0
      
    }else if(!is.na(data$pred[i])&data$obs[i]-data$pred[i]<=40000000){
      propiedades_comp[i]=1
      
    }else{
      propiedades_comp[i]=NA
      
    }
    
  }
  
  total_propiedades_comp=sum(propiedades_comp,na.rm=T)
  
  
  #Proporciones
  
  pct_propiedades_nocomp=(1-(total_propiedades_comp/nrow(data)))
  pct_valor_propiedades=total_valor_propiedades/total_valor_propiedades_obs
  
  sum_pct_propcomp_valprop=pct_valor_propiedades+pct_propiedades_nocomp
  names(sum_pct_propcomp_valprop)= "sum_pct_propcomp_valprop"
  
  
  
  return(sum_pct_propcomp_valprop)
  
  
} 


#FUNCION PARA OBTENER DETALLES

pct_propcomp_valprop_dtll <- function (data, lev = NULL, model = NULL) {
  
  propiedades_comp=rep(0,nrow(data))
  total_valor_propiedades=sum(data$pred)
  
  total_valor_propiedades_obs=sum(data$obs,na.rm=T)
  
  
  for(i in 1:nrow(data)){
    
    if(!is.na(data$pred[i])&(data$obs[i]-data$pred[i]>40000000)){
      propiedades_comp[i]=0
      
    }else if(!is.na(data$pred[i])&data$obs[i]-data$pred[i]<=40000000){
      propiedades_comp[i]=1
      
    }else{
      propiedades_comp[i]=NA
      
    }
    
  }
  
  total_propiedades_comp=sum(propiedades_comp,na.rm=T)
  
  
  #Proporciones
  
  pct_propiedades_nocomp=(1-(total_propiedades_comp/nrow(data)))
  pct_valor_propiedades=total_valor_propiedades/total_valor_propiedades_obs
  
  sum_pct_propcomp_valprop=pct_valor_propiedades+pct_propiedades_nocomp
  names(sum_pct_propcomp_valprop)= "sum_pct_propcomp_valprop"
  
  
  
  return(tibble(sum_pct_propcomp_valprop,
                "pct_propiedades_nocomp"=pct_propiedades_nocomp,
                "pct_valor_propiedades"=pct_valor_propiedades
                
                ))
  
  
} 

#VERSION SPATIAL CV
#pct_propcomp_valprop_scv <- function (truth,estimate) {
#  
#  data=tibble("obs"=truth,"pred"=estimate)
#  
#  propiedades_comp=rep(0,nrow(data))
#  total_valor_propiedades=sum(data$pred)
#  
#  total_valor_propiedades_obs=sum(data$obs,na.rm=T)
#  
#  
#  for(i in 1:nrow(data)){
#    
#    if(!is.na(data$pred[i])&(data$obs[i]-data$pred[i]>40000000)){
#      propiedades_comp[i]=0
#      
#    }else if(!is.na(data$pred[i])&data$obs[i]-data$pred[i]<=40000000){
#      propiedades_comp[i]=1
#      
#    }else{
#      propiedades_comp[i]=NA
#      
#    }
#    
#  }
#  
#  total_propiedades_comp=sum(propiedades_comp,na.rm=T)
#  
#  
#  #Proporciones
#  
#  pct_propiedades_nocomp=(1-(total_propiedades_comp/nrow(data)))
#  pct_valor_propiedades=total_valor_propiedades/total_valor_propiedades_obs
#  
#  sum_pct_propcomp_valprop=pct_valor_propiedades+pct_propiedades_nocomp
#  names(sum_pct_propcomp_valprop)= "pct_propcomp_valprop_scv"
#  
#  
#  
#  return(sum_pct_propcomp_valprop
#         
#         
#  )
#  
#  
#} 
#
#