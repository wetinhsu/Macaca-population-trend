
library(tidyverse)
library(sf)
#--------------------------------------------------------------------
n_nest <- 
  function(x, y){  #st_point, nc.b,
    
    nearest <- 
      st_nearest_feature(x, y, by_element = TRUE) 
    
    y %>% 
      slice(nearest) %>%
      dplyr::select(TypeName)%>% 
      mutate(Distance = st_distance(x, ., by_element = TRUE)%>% 
               as.numeric(.))%>% 
      st_drop_geometry 
    
    
  }


#--------------------------------------------------------------------
#read forest spatial data

path <- "D:/R/test/第四次森林資源調查全島森林林型分布圖"

nc.b <- st_read(paste0(path,"/","全島森林林型分布圖.shp"), crs=3826) %>% 
  arrange(TypeName, st_geometry_type(geometry), FunctionTy,Function) %>% 
  filter(!TypeName %in% c("待成林地", "裸露地", "陰影"))
