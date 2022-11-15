
# LIBRERIAS ---------------------------------------------------------------

library(tidyverse)
library(kableExtra)
library(haven)


# FUNCION -----------------------------------------------------------------

#Esta función es de nuestra autoría. Toma datos, etiquetas de columnas y filas y produce una tabla de diferencia de medias
#donde se comparan las variables dependent con base en grupos creamos por variables dicotomas explanatory.

tabla_diferencia_medias <- function(datos,
                                    etiqueta_columnas,
                                    etiqueta_filas,
                                    explanatory,
                                    dependent){
  
  
  for (i in 1:length(dependent)){
    
    
    table_row <- tibble()
    
    for (j in 1:length(explanatory)){
      
      
      test <- t.test(
        datos[datos[[dependent[i]]]==1,explanatory[j]],
        datos[datos[[dependent[i]]]==0,explanatory[j]]
      )
      
      mean_all <- round(mean(datos[[explanatory[j]]],na.rm=T),2) %>% as.character
      count_all <- sum(!is.na(datos[[dependent[i]]]),na.rm=T) %>% as.character 
      count_1 <- sum(datos[[dependent[i]]]==1,na.rm=T)%>% as.character
      count_0 <- sum(datos[[dependent[i]]]==0,na.rm=T)%>% as.character
      
      p_value <- test$p.value
      mean_1 <- round(test$estimate[1],2)
      mean_0 <- round(test$estimate[2],2)
      
      if(p_value<=0.01){
        diff <- paste0(round(mean_1- mean_0,2),"***") 
      }else if(p_value>0.01&p_value<=0.05){
        diff <- paste0(round(mean_1- mean_0,2),"**") 
      }else if(p_value>0.05&p_value<=0.1){
        diff <- paste0(round(mean_1- mean_0,2),"*") 
        
      }else{
        diff <- as.character(round(mean_1- mean_0,2))
      }
      
      sd_all <- paste0("(",round(sd(datos[[explanatory[j]]],na.rm=T),2),")") 
      sd_1 <- paste0("(",round(sd(datos[datos[[dependent[i]]]==1,explanatory[j]][[1]],na.rm=T),2),")")
      sd_0 <- paste0("(",round(sd(datos[datos[[dependent[i]]]==0,explanatory[j]][[1]],na.rm=T),2),")")
      sd_diff <- paste0("[",round(test$stderr,2),"]")
      
      mean_1 <- mean_1 %>% as.character
      mean_0 <- mean_0 %>% as.character
      
      if(i==1){
        table_row <- table_row %>% 
          bind_rows(
            tibble(
              "Variable"=c(etiqueta_filas[j],""), 
              "All"=c(mean_all,sd_all),
              "1"=c(mean_1,sd_1),
              "0"=c(mean_0,sd_0),
              "(1)-(0)"=c(diff,sd_diff)
            ))  
      }else{
        table_row <- table_row %>% 
          bind_rows(
            tibble(
              "1"=c(mean_1,sd_1),
              "0"=c(mean_0,sd_0),
              "(1)-(0)"=c(diff,sd_diff)
            ))}
      
      
      if(j==length(explanatory)&i==1){
        
        table_row <- table_row %>% 
          bind_rows(
            tibble(
              "Variable"=c("Number of units"),
              "All"=c(count_all),
              "1"=c(count_1),
              "0"=c(count_0),
              "(1)-(0)"=c("")
            ))
        
      }else if(j==length(explanatory)&i>1){
        table_row <- table_row %>% 
          bind_rows(
            tibble(
              "1"=c(count_1),
              "0"=c(count_0),
              "(1)-(0)"=c("")
            ))
      }else{}
      
      
      
      
    }
    
    
    if(i==1){
      table_col <- table_row
    }else{
      table_col <- table_col %>% 
        bind_cols(
          table_row)
    }
    
    
  }
  
  
  encabezados <- c("Variable","Mean All\n(1)")
  for(m in seq(3,((length(dependent)*3)+1),3)){
    
    encabezados <- c(encabezados,
                     paste0("Mean Yes\n","(",m,")"),
                     paste0("Mean No\n","(",m+1,")"),
                     paste0("Difference\n","(",m+2,")")
    )
    
  }
  
  
  titulos <- tibble(title=c(" "),span=c(2))
  for(k in 1:length(dependent)){
    
    titulos <- titulos %>% 
      bind_rows(tibble(
        title=c(etiqueta_columnas[k],""),
        span=c(2,1)
      ))
    
  }
  
  second_last <- c(length(explanatory)*2)
  
  table_col %>% 
    kable(.,escape = F,format="latex", booktabs=T,linesep = "",
          col.names=linebreak(encabezados, align = "c"),align="c") %>% 
    kable_styling() %>% 
    add_header_above(titulos) %>% 
    row_spec(second_last, hline_after = T)
  
}
