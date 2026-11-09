library(sf)
library(tidyverse)
library(fs)



library(terra)
sf_path <- "D:/R/SHP圖層/Taiwan_environmental_dataset-master/GeoTIFF"


envo_ls <- 
  dir_ls(sf_path, recurse = T, type = "file") %>% 
  .[str_detect(., "TIF")] %>% 
  .[str_detect(., "121")] %>% 
  .[str_detect(., "2010s|Other|DTM") ] %>% 
  .[str_detect(., "zip", negate = T) ] 


environment_sp <- rast(envo_ls)
#-------------------

rm(sf_path)
rm(envo_ls)
