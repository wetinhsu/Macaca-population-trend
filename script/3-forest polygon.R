#---- load library

library(data.table)
library(readxl)
library(writexl)
library(tidyverse)
library(sf)

#-----------------------------
#read point data

S.all<- list.files("./data/clean/Site/", pattern = "Site_", full.names = T) %>% 
  lapply(., function(x){
    read_xlsx(x, sheet = 1, cell_cols("A:K"), col_types = "text") %>% 
      setDT
  }) %>% 
  do.call(rbind, .) %>% 
  setDT  %>% 
  .[, Year := as.character(Year)]

#transform to spatial data
st.all<- 
  S.all %>% 
  .[!is.na(X)|!is.na(Y),] %>% 
  .[, list(x=X, y=Y, X, Y)] %>% 
  unique %>% 
  .[, NO := 1 : nrow(.)] %>% 
  .[, X := as.numeric(X)] %>% 
  .[, Y := as.numeric(Y)] %>% 
  st_as_sf(., coords = c("X", "Y"), crs = 4326) %>% 
  st_transform(., 3826)

#st.all %<>% add_column(TypeName = NA) %>% add_column(Distance = NA) 

#----------------
#read forest spatial data, merge polgons by TypeName, area by TypeName

path <- "D:/R/test/第四次森林資源調查全島森林林型分布圖"

nc <- st_read(paste0(path,"/","全島森林林型分布圖.shp"),
              crs=3826) %>% 
  dplyr::arrange(TypeName, st_geometry_type(geometry), FunctionTy,Function)


nc.b <- 
  nc %>% 
  dplyr::filter(!TypeName %in% c("待成林地", "裸露地", "陰影"))


#---------------------
#calculate distance


Sys.time()
nearest <-
  st_nearest_feature(st.all, nc.b,
                     by_element = TRUE)
Sys.time()
dist <-  
  st_distance(st.all, nc.b[nearest,],
              by_element = TRUE) %>% 
  as.numeric(.)
Sys.time()



#--------------------------------
#combind distance to point data

S.all <- 
st.all %>%
  st_drop_geometry %>% 
  mutate(Distance = dist) %>% 
  bind_cols(nc.b[nearest,] %>% 
              st_drop_geometry %>%
              select(TypeName) ) %>% 
  left_join(S.all, .,  by = c("X" = "x", "Y" = "y")) %>% 
  select(-NO)


M.all <- read_excel("./data/clean/Macaca/Macaca_1521_v1.xlsx", col_types = "text") %>% 
  setDT %>% 
  .[, Year := as.character(Year)]

M.data<- M.all %>% 
  full_join(S.all, by = c("Year", "Site_N", "Point", "Survey", "Month")) %>% 
  setDT %>% 
  .[order(Year,Site_N),] %>% 
  .[, X := as.numeric(X)] %>% 
  .[, Y := as.numeric(Y)]


write_xlsx(M.data, "./data/clean/forest_combind_data_1521.xlsx")
