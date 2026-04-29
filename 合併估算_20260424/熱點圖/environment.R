library(sf)
library(tidyverse)
library(fs)

sf_path <- "D:/R/SHP圖層/Taiwan_environmental_dataset-master/Shapefile"


envo_ls <- 
dir_ls(sf_path, recurse = T) %>% 
  .[str_detect(., "shp")] %>% 
  .[str_detect(., "121")] %>% 
  .[str_detect(., "2010s|Other|DTM") ] 


envo_ls_1 <- 
st_read(envo_ls[1]) %>% st_transform(crs = 3826)  

envo_ls_2 <- 
  st_read(envo_ls[2]) %>% st_transform(crs = 3826)  

envo_ls_3 <- 
  st_read(envo_ls[3]) %>% st_transform(crs = 3826)  

envo_ls_4 <- 
  st_read(envo_ls[4]) %>% st_transform(crs = 3826)  

environment_sf <- 
envo_ls_1 %>% 
  left_join(st_drop_geometry(envo_ls_2) ,
            by = c('GRIDID','x','y'), suffix = c("", ".y")) %>% 
  left_join(st_drop_geometry(envo_ls_3) ,
            by = c('GRIDID','x','y'), suffix = c("", ".y")) %>% 
  left_join(st_drop_geometry(envo_ls_4) ,
            by = c('GRIDID','x','y'), suffix = c("", ".y"))


rm(sf_path)
rm(envo_ls)
rm(envo_ls_1)
rm(envo_ls_2)
rm(envo_ls_3)
rm(envo_ls_4)
