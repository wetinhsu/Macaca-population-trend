
library(tidyverse)
library(sf)
library(DBI)

#--------------------------------------------------------------------
n_nest <- 
  function(x, y){  #st_point, nc.b,
    
    nearest <- 
      st_nearest_feature(x, y, by_element = TRUE) 
    
    y %>% 
      slice(nearest) %>%
      select(TypeName)%>% 
      mutate(Distance = st_distance(x, ., by_element = TRUE)%>% 
               as.numeric(.))%>% 
      st_drop_geometry 
    
    
  }

#--------------------------------------------------------------------
#read BBS Point XYdata
con <-  dbConnect(RSQLite::SQLite(), dbname="D:/R/test/DB/P_BBS.db")
list_Point<-
  dbReadTable(con, "list_Point") %>% 
  arrange(樣區編號, 獼猴樣區編號,as.numeric(樣點代號)) 


dbDisconnect(con)


dfo  <- data.table::fread("D:/R/test/bbs_handover_temp_v20190123 - WT/dfs2.csv",
                          head=T, encoding = "UTF-8", na.strings = "")   #UTF-8
#--------------------------------------------------------------------
#read forest spatial data

path <- "D:/R/SHP圖層/第四次森林資源調查全島森林林型分布圖"

nc.b <- st_read(paste0(path,"/","全島森林林型分布圖.shp"), crs=3826) %>% 
  arrange(TypeName, st_geometry_type(geometry), FunctionTy,Function) %>% 
  filter(!TypeName %in% c("待成林地", "裸露地", "陰影"))


#--------------------------------------------------------------------
# filter and transform to spatial data
st_point <- 
  list_Point %>% 
  filter(!is.na(樣區編號)) %>% 
  select(PointID, X_97, Y_97) %>% 
  filter(!is.na(X_97)|!is.na(Y_97))%>% 
  filter(!X_97%in% "")%>% 
  mutate_at(c("X_97", "Y_97"), as.numeric) %>% 
  st_as_sf(., coords = c("X_97", "Y_97"), crs = 3826) 

#--------------------------------------------------------------------
#calculate distance
Sys.time()

st_point_1 <- 
  st_point %>% 
  mutate(n_nest (.,  nc.b))%>%
  st_drop_geometry 

Sys.time()

#--------------------------------------------------------------------
library(readxl)
M.all <- read_excel("./data/clean/Macaca/Macaca_1524_v1.xlsx", col_types = "text") 

S.all <- 
#  read_excel("data/clean/Site/Site_2022_v1.xlsx", col_types = "text") %>% 
  
  bind_rows(
    
    S1524%>% mutate_if(is.numeric, as.character)
    
  )


M.data <- 
  M.all %>% 
  full_join(.,S.all, by = c("Year", "Site_N", "Point", "Survey")) %>% 
  mutate(PointID = as.integer(PointID)) %>% 
  left_join(.,st_point_1 , by = "PointID") 


write_xlsx(M.data, "./data/clean/forest_combind_data_1524.xlsx")

