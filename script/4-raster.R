
library(tidyverse)
library(sf)
library(raster)
library(readxl)
library(writexl)


str_name<-'D:/R/test/不分幅_全台及澎湖/dem_20m.tif' 
imported_raster=raster(str_name )


plot(imported_raster)

s <- stack(imported_raster)



S.all<- read_xlsx( "./data/clean/forest_combind_data_1523.xlsx") %>% 
  filter(! is.na(Month)| ! is.na(Day)| ! is.na(Hour)| ! is.na(Minute)) %>% 
  add_column(NO = 1:nrow(.))

S.all %>%  #確認每一旅次內的每一個樣點資料只有1筆
  group_by(., Year, Survey, Site_N, Point) %>% 
  summarise( n=n()) %>% 
  filter(n >1)




p<- S.all %>%
  filter(!is.na(X_97))  %>% 
  mutate(x = as.numeric(X_97)) %>% 
  mutate(y = as.numeric(Y_97)) %>% 
  st_as_sf(., coords = c("X_97", "Y_97"), crs = 3826) %>% 
  st_transform(crs(imported_raster))




pp<- extract(imported_raster, p, method='simple') %>% 
  data.frame(NO=p$NO, Altitude = .)
summary(pp)

S.all <- 
  S.all %>% 
  left_join(pp, by = c("NO")) %>% 
  dplyr::select(-"NO")

write_xlsx(S.all, "./data/clean/full_combind_data_1523.xlsx")
