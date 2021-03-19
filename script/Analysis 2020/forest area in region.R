##各分區的森林面積

#---- load library

library(readxl)
library(writexl)
library(tidyverse)
library(sf)


#-----------------------------------------------------
#county
path2 <- "D:/R/test/COUNTY_MOI_1081121"

TW <- st_read(paste0(path2,"/","COUNTY_MOI_1081121.shp")) %>% 
  st_transform(.,4326) %>% 
  filter(!COUNTYNAME %in% c("連江縣", "澎湖縣","金門縣")  ) %>% 
  st_transform(.,3826) 
  

North <- c("宜蘭縣","基隆市","台北市","臺北市",
                   "新北市","台北縣","臺北縣",
                   "桃園縣","桃園市","新竹市",
                   "新竹縣","苗栗縣")

Center1 <- c("台中市","臺中市",
                     "台中縣","臺中縣",
                     "彰化縣","南投縣","南投市")

Center2 <- c("雲林縣","嘉義縣","嘉義市",
                     "台南市","臺南市",
                     "台南縣","臺南縣")

South <- c("高雄縣","高雄市",
                     "屏東縣")

East1 <- c("花蓮縣")  

East2 <- c("台東縣","臺東縣")

TW <- TW %>% 
  mutate(Region2 = ifelse(COUNTYNAME %in% North,"North",
                          ifelse(COUNTYNAME %in% Center1,"Center1 ",
                                 ifelse(COUNTYNAME %in% Center2,"Center2",
                                        ifelse(COUNTYNAME %in% South,"South",
                                               ifelse(COUNTYNAME %in% East1,"East1",
                                                      ifelse(COUNTYNAME %in% East2,"East2",NA)))))))

#----------------
path3 <- "./data/clean/gis"
EL50 <- st_read(paste0(path3,"/","elev50.shp")) %>% 
  st_transform(.,3826) %>% 
  st_combine() %>% 
  st_as_sf() %>% 
  st_buffer(dist = 0)

summary(EL50)
ggplot(EL50)+geom_sf()




#----------------
#read forest spatial data, merge polgons by TypeName, area by TypeName

path <- "D:/R/test/第四次森林資源調查全島森林林型分布圖"

nc <- st_read(paste0(path,"/","全島森林林型分布圖.shp")) %>% 
  st_transform(.,3826) %>% 
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


Sys.time()
test2 <- 
TW $COUNTYNAME  %>% 
  as.character %>%
  unique %>% 
  lapply(.,
         function(x){
           TW %>%
             filter(COUNTYNAME %in% x) %>% 
             st_intersection(EL_50, .)
             st_intersection(nc, .) 
           
         }) %>% 
  do.call(rbind,.)

Sys.time()

(test3 <- 
test2 %>% 
  mutate(geo_area = st_area(.) %>% as.numeric) %>% 
  st_drop_geometry %>% 
  mutate(TypeName.1 = ifelse(TypeName %in% "闊葉樹林型", "闊葉林",
                             ifelse(TypeName %in% "針葉樹林型", "針葉林",
                                    ifelse(TypeName %in% "竹林","竹林",
                                           ifelse(TypeName %in% c("竹針混淆林", "竹針闊混淆林", "竹闊混淆林", "針闊葉樹混", "針闊葉樹混淆"),"混淆林", NA))))) %>% 
  
  group_by(Region2, TypeName.1) %>% 
  summarise(geo_area =sum(geo_area)) %>% 
  reshape2::dcast(TypeName.1 ~ Region2, value.var = "geo_area"))
