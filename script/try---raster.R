
library(tidyverse)
library(sf)
library(raster)
library(readxl)
library(writexl)


str_name<-'D:/R/test/不分幅_全台及澎湖/dem_20m.tif' 
imported_raster=raster(str_name )


plot(imported_raster)

s <- stack(imported_raster)



S.all<- list.files("./data/clean/Site/", pattern = "Site_", full.names = T) %>% 
  lapply(., function(x){
    read_xlsx(x, sheet = 1, cell_cols("A:K")) 
  }) %>% 
  bind_rows


S.all %>%  #確認每一旅次內的每一個樣點資料只有1筆
  group_by(., Year, Survey, Site_N, Point) %>% 
  summarise( n=n()) %>% 
  filter(n >1)


S.all %>%
  dplyr::select(X,Y) %>% 
  unique() %>% 
  add_column(NO = 1:nrow(.)) %>%
  write.csv(., "./data/clean/gis/S_all.csv", row.names = F)

p<- S.all %>% 
  #filter(!(substr(Site_N,1,1) %in% "K")) %>% 
  mutate(x = as.numeric(X)) %>% 
  mutate(y = as.numeric(Y)) %>% 
  st_as_sf(., coords = c("x", "y"), crs = 4326) %>% 
  st_transform(crs(imported_raster))




pp<- extract(imported_raster, p, method='simple')
summary(pp)
data.frame(Altitude = pp, S_all_X = p$X,  S_all_Y = p$Y) %>% View(.)



