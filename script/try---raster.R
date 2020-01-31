
library(raster)
library(data.table)
library(magrittr)
library(readxl)
library(tidyr)
library(purrr)
library(dplyr)
library(writexl)
library(rgdal)


str_name<-'./data/raw/不分幅_全台及澎湖/dem_20m.tif' 
imported_raster=raster(str_name)


plot(imported_raster)

s <- stack(imported_raster)



S.all<- list.files("./data/clean/Site/", pattern = "Site_", full.names = T) %>% 
  lapply(., function(x){
    read_xlsx(x, sheet = 1, cell_cols("A:K")) %>% 
      setDT
  }) %>% 
  do.call(rbind, .) %>% 
  setDT

S.all %>% .[, .N, by = list(Year, Survey, Site_N, Point)] %>% .[ N >1,]
S.all %>% .[, list(X, Y)] %>% unique %>% .[, NO := 1 : nrow(.)] %>% write.csv(., "./data/clean/gis/S_all.csv", row.names = F)

S.all<- S.all %>% 
  .[, X := as.numeric(X)] %>% 
  .[, Y := as.numeric(Y)]
S.all.o <- S.all
coordinates(S.all) <- ~X + Y
proj4string(S.all) <- CRS("+init=epsg:4326")
S.all %<>% spTransform(crs(imported_raster))


pp<- extract(imported_raster, S.all, method='simple')
summary(pp)
data.frame(Altitude = pp, S_all_X = S.all.o$X,  S_all_Y = S.all.o$Y) %>% View(.)

