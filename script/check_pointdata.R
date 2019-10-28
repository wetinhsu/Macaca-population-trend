
#---- load library

library(data.table)
library(magrittr)
library(readxl)
library(tidyr)
library(purrr)
library(dplyr)
library(writexl)
library(rgdal)

point.list <- read_xlsx("data/raw/all point_20191023.xlsx",
          sheet = 1)  %>% 
  setDT %>% 
  .[!duplicated(.)]  %>% 
  setDT %>%
  setnames(., c("Site_N", "Point", "County", "Name", "Pointid",
                "X.67", "Y.67", "X.97", "Y.97", "X.dms", "Y.dms", "X.84", "Y.84",
                "Note1", "Note2"))


twd97to84 <- function(X, Y) {
  data <- data.frame(x = X, y = Y)
  aa<- 
    cbind(as.numeric(data$x),as.numeric(data$y))%>%
    SpatialPointsDataFrame(coords=., data=data, proj4string=CRS("+init=epsg:3826")) %>%
    spTransform(.,  CRS("+init=epsg:4326")) %>% coordinates  

}
twd67to84 <- function(X, Y) {
  data <- data.frame(x = X, y = Y)
  aa<- 
    cbind(as.numeric(data$x),as.numeric(data$y))%>%
    SpatialPointsDataFrame(coords=., data=data, proj4string=CRS("+init=epsg:3828")) %>%
    spTransform(.,  CRS("+init=epsg:4326")) %>% coordinates  
  
}

twd67to84(data.frame(X=268230, Y=2666489)) %>% print
  

S16[cood %in% "TWD67/TM2", list(X,Y)]  %>% 
split(.,  seq(nrow(.))) %>%
lapply(.,twd67to84) %>%
  do.call(., rbind)


twd67to84(data.frame(X=194639, Y=2533215)) %>% print

twd67to84 <- function(XY) {
  aa<- 
    cbind(as.numeric(XY$X),as.numeric(XY$Y))%>%
    SpatialPointsDataFrame(coords=., data=XY, proj4string=CRS("+init=epsg:3828")) %>%
    spTransform(.,  CRS("+init=epsg:4326")) %>% coordinates  
}


#============================

S16 <- 
  lapply(paste0("./data/raw/BBSdata/", 2016), function(x){
    list.files(x, pattern = "BBSdata_", full.names = T) %>%  
      read_xlsx(., sheet = "birddata", cell_cols("D:AF")) %>% 
      setDT %>%
      .[!(時段 %in% c("NA", "Supplementary", "")),] %>%
      .[ 分析 %in% "Y", list(年, 樣區編號, 樣點編號, 座標系統, X座標, Y座標, 調查旅次編號)] %>%
      setnames(., c("Year", "Site_N", "Point", "cood", "X", "Y", "Survey")) %>%
      .[!duplicated(.)]  
  } ) %>% 
  do.call(rbind, .) %>% 
  data.table

S16[S16=="NA"] <- NA
S16[S16==""] <- NA



S16 %>% setDT %>%
  .[cood %in% "TWD97/TM2", ]



tt<- S16 %>%
  .[Survey %in% 1, `2016_1` := 1] %>%
  .[Survey %in% 2, `2016_2` := 1] %>%
  
  .[cood %in% "TWD67/TM2", X.67 := X] %>%
  .[cood %in% "TWD67/TM2", Y.67 := Y] %>%
  .[cood %in% "TWD97/TM2", X.97 := X] %>%
  .[cood %in% "TWD97/TM2", Y.97 := Y] %>%
  .[cood %in% "WGS84/經緯度(度分秒)", X.dms := X] %>%
  .[cood %in% "WGS84/經緯度(度分秒)", Y.dms := Y] %>%
  .[cood %in% "WGS84/經緯度(度分)", X.84 := as.numeric(substr(X,1,3)) + (1/60)*as.numeric(substr(X,5,nchar(X)-4))] %>%
  .[cood %in% "WGS84/經緯度(度分)", Y.84:= as.numeric(substr(Y,1,2)) + (1/60)*as.numeric(substr(Y,4,nchar(X)-3))] %>%
  .[cood %in% "WGS84/經緯度(度)", X.84 := as.numeric(X)] %>%
  .[cood %in% "WGS84/經緯度(度)", Y.84 := as.numeric(Y)] %>%
  .[, c("Survey", "cood", "X", "Y") := NULL] %>%
  
  
  #.[, list(Site_N, Point)] %>%  
  #.[!duplicated(.)]  %>%
  left_join(point.list, by = c("Site_N", "Point")) %>%
  setDT

tt %>%
  .[X.84 %in% NA,] %>%
  .[, list(Site_N, Point)] %>%  
  S16[., on = c("Site_N", "Point")]




