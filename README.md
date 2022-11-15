# Problem Set 3: Predicting properties pricing
Desarrollado por John Daniel Delgado Vargas, José Julian Parra Montoya y Alison Gissell Ruiz Ruiz.

### Introducción

El objetivo es predecir el valor de propiedades en Cali a partir de un modelo entrenado con valores de propiedades en Bogotá y Medellín, para lo cual fué necesario realizar georefenciación para extraer de Open Street Map lugares cercanos a las propiedades, se extrajo además por medio de expresiones regulares algunas características de las propiedades de sus descripciones y se extrajeron características de las propiedades del censo de 2018.

El análisis se desarrolla en 5 etapas:

* Obtención y selección de variables.
* Entrenamiento del modelo de predicción.

### Tabla de contenido
-  [Document](#document)
-  [Scripts](#scripts)
-  [Stores](#stores)
-  [Views](#views)

### Install

Este proyecto requiere R y las siguientes librerias instaladas

* library(pacman)
* library(ggplot2)#graficar
* library(tidyverse)#organizar datos
* library(xtable)#tablas
* library(openxlsx)#lectura xlsx
* library(janitor)#tabla de frecuencia
* library(data.table)#manejo de dataframes
* library(kableExtra)#tablas
* library(haven)#lectura de dta
* library(scales)#graficos
* library(boot)#bootstrap

Para instalarlas se debe correr el comando install.packages, a continuación un ejemplo de esto.

```bash
install.packages("sandwich")
```

### Data

En la carpeta [`stores`](https://github.com/AlisonRuiz/predicting_properties_pricing/tree/main/stores) se encuentran archivos RDS del DANE relacionados con el Censo del 2018

### Scripts

El proyecto cuenta con los siguientes scripts de R:

* [`feauture_extraction.R`](https://github.com/AlisonRuiz/predicting_properties_pricing/blob/main/scripts/feauture_extraction.R)
* [`Dane.R`](https://github.com/AlisonRuiz/predicting_properties_pricing/blob/main/scripts/Dane.R)
* [`Dane_Mapas.R`](https://github.com/AlisonRuiz/predicting_properties_pricing/blob/main/scripts/Dane_Mapas.R)
* [`xg_boost_dane.R`](https://github.com/AlisonRuiz/predicting_properties_pricing/blob/main/scripts/xg_boost_dane.R)

### Informe

El informe se encuentra en la carpeta [`document`](https://github.com/AlisonRuiz/predicting_properties_pricing/tree/main/stores/document) se encuentra en formato .tex y .pdf. En este archivo se resumen los resultados y se explica su interpretación.
