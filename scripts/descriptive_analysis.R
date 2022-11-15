
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

select <- dplyr::select
scalebar <- ggsn::scalebar




# MAPA BOGOTA -------------------------------------------------------------

bog <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key="boundary", 
                  value="administrative") %>% 
  osmdata_sf()


#This doesnt retrieve administrative levels

#bog <- getbb(place_name = "Bogota Colombia", 
#                featuretype = "boundary:administrative", 
#                format_out = "sf_polygon")


bog_upz <- bog$osm_multipolygons %>% 
  subset(admin_level==8)

#Importando datos de precios
train_original_sf_bog <- st_as_sf(train_original%>% 
                                    dplyr::filter(city%in%"Bogotá D.C"),
                                  coords=c("lon","lat"),
                                  crs=crs(bog_upz)) %>% 
  st_transform(4326)

train_original_sf_bog_price <- train_original_sf_bog %>% 
  select(price, geometry ) 


bog_upz_price <- st_join(
  bog_upz,
  train_original_sf_bog_price
) 

#sin loop
bog_upz_medianprice <- bog_upz_price %>% 
  group_by(name) %>% 
  summarise(median_price=median(price,na.rm=T)) %>% 
  ungroup() %>% 
  distinct()


## add osm layer
osm_layer <- get_stamenmap(bbox = as.vector(st_bbox(bog)), 
                           maptype="toner", source="osm", zoom=13) #en lugar de toner podría usarse satellital, toner es solo vias
#el zoom indica la precisión.

#ggmap permite usar la capa de osm. Para esto se debe tambien disminuir el alpha del poligono o la
#transparencia
ggmap(osm_layer) + 
  geom_sf(data=train_original_sf_bogota , aes(fill=price) , alpha=0.3 , inherit.aes=F) +
  scale_fill_viridis(option = "D" , name = "Variable") +
  scalebar(data = bog , dist = 5 , transform = T , dist_unit = "km") +
  north(data = bog , location = "topleft") + theme_linedraw() + labs(x="" , y="")





# MAPA MEDELLIN ---------------------------------------------------------------

med <- opq(bbox = getbb("Medellin Colombia")) %>%
  add_osm_feature(key="boundary", 
                  value="administrative") %>% 
  osmdata_sf()


med_comunas <- med$osm_multipolygons %>% 
  subset(admin_level==8)

#Importando datos de precios
train_original_sf_med <- st_as_sf(train_original%>% 
                                    dplyr::filter(city%in%"Medellín"),
                                  coords=c("lon","lat"),
                                  crs=crs(med_comunas)) %>% 
  st_transform(4326)

train_original_sf_med_price <- train_original_sf_med %>% 
  select(price, geometry ) 


med_comunas_price <- st_join(
  med_comunas,
  train_original_sf_med_price
) 

#sin loop
med_upz_medianprice <- med_com_price %>% 
  group_by(name) %>% 
  summarise(median_price=median(price,na.rm=T)) %>% 
  ungroup() %>% 
  distinct()





med_upz_medianprice_2 <- med_upz_medianprice %>% 
  mutate(median_price=median_price/1000,
         price_category=case_when(
           median_price<300000~"[0, 300 Mill.)",
           median_price>=300000&median_price<400000~"[300 Mill., 400 Mill.)",
           median_price>=400000&median_price<700000~"[400 Mill., 700 Mill.)",
           median_price>=700000&median_price<1000000~"[700 Mill., 1000 Mill.)")
  )

library(RColorBrewer)

my.cols <- brewer.pal(4, "Greens")

med_upz_medianprice_2 %>% 
  ggplot() + 
  geom_sf(aes(fill=factor(price_category)),color="black")+
  scale_fill_manual("Precio", values = my.cols, guide = "legend") +
  
  #  scale_fill_distiller("",palette="Greens",labels=comma)+
  scalebar(data=med_upz_medianprice,dist=4,transform=T,
           st.size=3,
           location = "bottomleft",
           dist_unit="km")+
  north(data=med_upz_medianprice,location="topright")+
  xlab("")+
  ylab("")+
  theme_bw()+
  theme(text=element_text(size=14, color="black"),
        panel.grid=element_blank(),
        panel.spacing = unit(1, "lines"),
        legend.position="left",
        legend.text=element_text(size=7),
        legend.title=element_text(size=9),
        axis.text.x=element_text(colour="black",size=8),
        axis.text.y=element_text(colour="black",size=8),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(face="bold"))

ggsave(dpi="retina",height=5,width=8,"mapa_medellin.png")






upz <- bog_upz_price$name %>% unique()

bog_upz_price <- st_transform(bog_upz_price, 4326)


x <- bog_upz_price %>%
  filter(name%in%upz[6]) %>% 
  select(geometry)

x[[1]]
y[[1]]


y <- bog_upz_price %>%
  filter(name%in%upz[5]) %>% 
  select(geometry)
  


slice_sample(prop=0.01) %>%
  ungroup() %>% 
  ggplot()+
  geom_sf()

bog_upz_price %>%
  filter(name%in%upz[5]) %>% 
  slice_sample(prop=0.01) %>%
  ungroup() %>% 
  ggplot()+
  geom_sf()




leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    data=bog_upz_price %>%
      filter(name%in%upz[4]),
    color="black"
    
  )

chapinero <- getbb(place_name = "UPZ San Cristobal Norte, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    data=chapinero
  )


