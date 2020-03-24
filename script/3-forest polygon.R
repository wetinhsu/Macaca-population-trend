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
  .[, list(x=X, y=Y, X, Y)] %>% 
  unique %>% 
  .[, NO := 1 : nrow(.)] %>% 
  .[, X := as.numeric(X)] %>% 
  .[, Y := as.numeric(Y)] %>% 
  st_as_sf(., coords = c("X", "Y"), crs = 4326) %>% 
  st_transform(., 3826)

st.all %<>% add_column(TypeName = NA) %>% add_column(distance = NA) 

#----------------
#read forest spatial data, merge polgons by TypeName, area by TypeName

path <- "D:/R/test/第四次森林資源調查全島森林林型分布圖"

nc <- st_read(paste0(path,"/","全島森林林型分布圖.shp"),
              crs="+init=epsg:3826") %>% 
  arrange(TypeName, st_geometry_type(geometry), FunctionTy,Function)

nc.a<-
  nc$TypeName %>% 
  as.character %>%
  unique %>% 
  lapply(.,
         function(x){
           tmp <- 
             nc %>%
             filter(TypeName %in% x) %>% 
             st_combine() 
           
           st_sf(TypeName= x, geom = st_sfc(tmp), crs = 3826)
         }) %>% 
  do.call(rbind,.)

 
bb <- 
  nc %>%
  st_drop_geometry %>% 
  group_by(TypeName) %>%
  summarise(area = sum(Area_ha) %>% round(.,4))

nc.b <- 
  left_join(nc.a, bb) %>% 
  filter(!TypeName %in% c("待成林地", "裸露地", "陰影"))

#---------------------
#calculate distance

Sys.time()
st.dis2<- st_distance(st.all,nc.b)  #3619筆耗時約50分鐘
Sys.time()

TypeName <- apply(st.dis2,1,which.min) %>% nc.b$TypeName[.] %>% as.character
distance<- apply(st.dis2,1,min)

st.all$TypeName <- TypeName
st.all$distance <- distance

#--------------------------------
#combind distance to point data

S.all <- 
st.all %>%
  st_drop_geometry %>%
  left_join(S.all, .,  by = c("X" = "x", "Y" = "y")) %>% 
  select(-NO)


M.all <- read_excel("./data/clean/Macaca/Macaca_1519_v2.xlsx", col_types = "text") %>% 
  setDT %>% 
  .[, Year := as.character(Year)]

M.data<- M.all %>% 
  full_join(S.all, by = c("Year", "Site_N", "Point", "Survey")) %>% 
  setDT %>% 
  .[order(Year,Site_N),] %>% 
  .[, X := as.numeric(X)] %>% 
  .[, Y := as.numeric(Y)]


write_xlsx(M.data, "./data/clean/forest_combind_data_V1.xlsx")
