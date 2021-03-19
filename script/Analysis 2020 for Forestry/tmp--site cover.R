#---- load library

library(tidyverse)
library(readxl)
library(writexl)
library(sf)

#------------

Point <- 
  read_xlsx("./data/clean/Point_info/all_20201116.xlsx",
            sheet = "樣點表") %>% 
  setNames(., c("BBS_Site", "Macaca_Site", "Point", "Conty", "Name", "SP",
                "X.67", "Y.67", "X.97", "Y.97", "X.84", "Y.84", "X", "Y", "Note", "Loction")) %>% 
  select(-X.67, -Y.67, -X.84, -Y.84, -X, -Y, -Note, -Loction)   #X.84=度分秒 X=小數度

M.point <-  #Macaca
  Point %>% 
  filter(substr(Macaca_Site, 1, 1) %in% "M" ) %>% 
  group_by(Macaca_Site) %>% 
  mutate(X_center = mean(X.97), Y_center = mean(Y.97)) %>% 
  ungroup()

B.point <- #BBS
  Point %>% 
  filter(substr(BBS_Site, 1, 1) %in% c("A","B", "C") )%>% 
  group_by(BBS_Site) %>% 
  mutate(X_center = mean(X.97), Y_center = mean(Y.97))%>% 
  ungroup()


shp_M_point <- #Macaca樣點圖層
M.point %>% 
  st_as_sf(., coords = c("X.97", "Y.97"), crs = 3826) 

shp_M_center <- #Macaca樣區中心圖層
  M.point %>% 
  st_as_sf(., coords = c("X_center", "Y_center"), crs = 3826)%>% 
  select(Macaca_Site, geometry) %>% unique()


shp_B_point <- #BBS樣點圖層
  B.point %>% 
  st_as_sf(., coords = c("X.97", "Y.97"), crs = 3826) 

shp_B_center <- #BBS樣區中心圖層
  B.point %>% 
  st_as_sf(., coords = c("X_center", "Y_center"), crs = 3826)%>% 
  select(BBS_Site, geometry) %>% unique()


#-------

M_Buffer_500 <- 
shp_M_center %>% 
  st_buffer(., dist = 500)


B_Buffer_500 <- 
  shp_B_center %>% 
  st_buffer(., dist = 500)

M_Buffer_1000 <- 
  shp_M_center %>% 
  st_buffer(., dist = 1000)


B_Buffer_1000 <- 
  shp_B_center %>% 
  st_buffer(., dist = 1000)

M_Buffer_3000 <- 
  shp_M_center %>% 
  st_buffer(., dist = 3000)


B_Buffer_3000 <- 
  shp_B_center %>% 
  st_buffer(., dist = 3000)

#-------

st_intersection(shp_M_center , shp_B_center) %>%   #樣區中心完全重疊的樣區有54個
  st_drop_geometry() %>% 
  select(BBS_Site) %>%
  unique %>% 
  dim

st_intersection(M_Buffer_500 , B_Buffer_500) %>%
  st_drop_geometry() %>% 
  select(BBS_Site) %>%
  unique %>% 
  dim

st_intersection(M_Buffer_1000, B_Buffer_1000) %>% 
  st_drop_geometry() %>% 
  select(BBS_Site) %>%
  unique %>% 
  dim

st_intersection(M_Buffer_3000, B_Buffer_3000) %>%  
  st_drop_geometry() %>% 
  select(BBS_Site) %>%
  unique %>% 
  dim


I3000<- 
  st_intersection(B_Buffer_3000, M_Buffer_3000) %>%  
  st_drop_geometry()  

 
  I3000  %>%
  select(BBS_Site) %>%
  unique %>% 
    dim


  I3000 %>%  select(Macaca_Site) %>%
  unique  %>% dim



  I1000<- 
    st_intersection(B_Buffer_1000, M_Buffer_1000) %>%  
    st_drop_geometry()  
  
  
  I1000  %>%
    select(BBS_Site) %>%
    unique %>% 
    dim
  
  
  I1000 %>%  select(Macaca_Site) %>%
    unique  %>% dim


  I500<- 
    st_intersection(B_Buffer_500, M_Buffer_500) %>%  
    st_drop_geometry()  
  
  
  I500  %>%
    select(BBS_Site) %>%
    unique %>% 
    dim
  
  
  I500 %>%  select(Macaca_Site) %>%
    unique  %>% dim

