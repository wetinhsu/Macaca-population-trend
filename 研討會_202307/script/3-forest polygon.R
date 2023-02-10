#---- load library
library(readxl)
library(writexl)
library(tidyverse)
library(sf)

#-----------------------------
#read point data

S.all<- "D:/R/test/Macaca-population-trend/研討會_202307/data/clean/Site_1521_v1.xlsx" %>%
  read_xlsx(., sheet = 1, cell_cols("A:K"), col_types = "text")%>% 
  mutate(Year = as.character(Year))%>% 
  mutate_at(c("X", "Y"), as.numeric)  

#transform to spatial data
st.all<- 
  S.all %>% 
  filter(!is.na(X)|!is.na(Y)) %>% 
  dplyr::select(X, Y) %>% 
  unique %>% 
  mutate(NO = 1 : nrow(.)) %>% 
  mutate_at(c("X", "Y"), as.numeric) %>% 

  st_as_sf(., coords = c("X", "Y"), crs = 4326, remove = F) %>% 
  st_transform(., 3826)

#st.all %<>% add_column(TypeName = NA) %>% add_column(Distance = NA) 

#----------------
#read forest spatial data, merge polgons by TypeName, area by TypeName

path <- "D:/R/test/第四次森林資源調查全島森林林型分布圖"

nc <- st_read(paste0(path,"/","全島森林林型分布圖.shp")) %>% 
  st_transform(crs=3826)  %>% 
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
              dplyr::select(TypeName) ) %>% 
  
  left_join(S.all, .,  by = c("X", "Y")) %>% 
  dplyr::select(-NO)


M.all <- read_excel("./研討會_202307/data/clean/Macaca_1521_v1.xlsx", col_types = "text")

M.data<- M.all %>% 
  full_join(S.all, by = c("Year", "Site_N", "Point", "Survey")) %>% 
  mutate_at(c("X", "Y"), as.numeric) %>% 
  arrange(Year,Site_N)

M.data %>%  #確認每一旅次內的每一個樣點資料只有1筆
  group_by(Year, Survey, Site_N, Point) %>% 
  summarise( n=n()) %>% 
  filter(n >1) %>% View

write_xlsx(M.data, "./研討會_202307/data/clean/forest_combind_data_1521.xlsx")
