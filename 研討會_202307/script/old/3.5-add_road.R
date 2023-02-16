
library(tidyverse)
library(sf)
library(raster)
library(readxl)
library(writexl)


#----------------
#read forest spatial data, merge polgons by TypeName, area by TypeName

path <- "D:/R/道路_WGS84/道路分級"

Rd_1W <- st_read(paste0(path,"/","1W.shp"), options="ENCODING=BIG5") %>% 
  st_transform(crs=4326)  %>% 
  st_transform(crs=3826)  %>% 
  mutate(Rd_factor = "1W")
Rd_2W <- st_read(paste0(path,"/","2W.shp"), options="ENCODING=BIG5") %>% 
  st_transform(crs=4326)  %>% 
  st_transform(crs=3826)  %>% 
  mutate(Rd_factor = "2W")
Rd_3W <- st_read(paste0(path,"/","3W.shp"), options="ENCODING=BIG5") %>% 
  st_transform(crs=4326)  %>% 
  st_transform(crs=3826)  %>% 
  mutate(Rd_factor = "3W")
Rd_4W <- st_read(paste0(path,"/","4W.shp"), options="ENCODING=BIG5") %>% 
  st_transform(crs=4326)  %>% 
  st_transform(crs=3826)  %>% 
  mutate(Rd_factor = "4W")



#登山步道、古道 (那些車不會走的道路)
Rd_5W <-
"./研討會_202307/data/refer/道路等級判斷.xlsx" %>%
  read_xlsx(., sheet = 1,  col_types = "text")%>% 
  mutate(Rd_factor = "5W")

Rd_5W.a <- 
  Rd_5W %>% filter(is.na(樣點代號)) %>% 
  dplyr::select(-樣點代號)

Rd_5W.b <- 
  Rd_5W %>% filter(!is.na(樣點代號)) 

#--------------------------

S.all<- "./研討會_202307/data/clean/forest_combind_data_1521.xlsx" %>%
  read_xlsx(., sheet = 1,  col_types = "text")%>% 
  mutate(Year = as.character(Year))%>% 
  mutate_at(c("X", "Y", "Distance"), as.numeric)  

#transform to spatial data
st.all<- 
  S.all %>% 
  filter(!is.na(X)|!is.na(Y)) %>% 
  dplyr::select(X, Y) %>% 
  unique %>% 
  mutate(NO = 1 : nrow(.)) %>% 
  mutate_at(c("X", "Y"), as.numeric) %>% 
  
  st_as_sf(., coords = c("X", "Y"), crs = 4326, remove = F) %>% 
  st_transform(crs=3826)  

#calculate distance


Sys.time()
nearest_1W <-
  st_nearest_feature(st.all, Rd_1W,
                     by_element = TRUE)
Sys.time()
dist_1W <-  
  st_distance(st.all, Rd_1W[nearest_1W,],
              by_element = TRUE) %>% 
  as.numeric(.)
Sys.time()

nearest_2W <-
  st_nearest_feature(st.all, Rd_2W,
                     by_element = TRUE)
Sys.time()
dist_2W <-  
  st_distance(st.all, Rd_2W[nearest_2W,],
              by_element = TRUE) %>% 
  as.numeric(.)
Sys.time()

nearest_3W <-
  st_nearest_feature(st.all, Rd_3W,
                     by_element = TRUE)
Sys.time()
dist_3W <-  
  st_distance(st.all, Rd_3W[nearest_3W,],
              by_element = TRUE) %>% 
  as.numeric(.)
Sys.time()

nearest_4W <-
  st_nearest_feature(st.all, Rd_4W,
                     by_element = TRUE)
Sys.time()
dist_4W <-  
  st_distance(st.all, Rd_4W[nearest_4W,],
              by_element = TRUE) %>% 
  as.numeric(.)
Sys.time()






S.all_2 <-
  st.all %>%
  st_drop_geometry %>%
  mutate(Distance_1W = dist_1W,
         Distance_2W = dist_2W,
         Distance_3W = dist_3W,
         Distance_4W = dist_4W) %>%


  reshape2::melt(id = 1:3) %>%
  filter(!value %in% c("1W", "2W", "3W", "4W")) %>%
  mutate(value = value %>% as.numeric(.)) %>%
  split(., .$NO) %>%
  lapply(., function(x){
    x %>%
      filter(value %in% min(value)) %>%
      arrange(variable) %>%
      slice(1) %>%
      setNames(., c("X", "Y","NO", "Rd_factor", "Rd_Distance")) %>%
      mutate(Rd_factor = str_remove_all(Rd_factor, "Distance_"))
  }) %>%
  bind_rows() %>%


  left_join(S.all, .,  by = c("X", "Y")) %>%
  dplyr::select(-NO) %>% 
  
  mutate(Rd_factor = ifelse(Site_N %in% Rd_5W.a$樣區編號, "5W", Rd_factor),
         Rd_Distance = ifelse(Site_N %in% Rd_5W.a$樣區編號, NA, Rd_Distance)) %>% 
  
  left_join(., Rd_5W.b, by = c("Site_N" = "樣區編號",
                               "Point" = "樣點代號"),
            suffix = c("", ".y")) %>% 
  mutate(Rd_factor = ifelse(!is.na(Rd_factor.y), "5W", Rd_factor),
         Rd_Distance = ifelse(!is.na(Rd_factor.y), NA, Rd_Distance)) %>% 
  
  dplyr::select(-備註, -Rd_factor.y)
  





table(S.all_2$Rd_factor)


write_xlsx(S.all_2, "./研討會_202307/data/clean/Road_combind_data_1521.xlsx")
