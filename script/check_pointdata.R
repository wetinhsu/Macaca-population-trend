
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
                "Note1", "Note2"))%>%
  .[, Site_N := as.character(Site_N)] %>%
  .[, Point := as.numeric(Point)]

twd67to84 <- function(XY) {
  aa<- 
    cbind(as.numeric(XY$X),as.numeric(XY$Y))%>%
    SpatialPointsDataFrame(coords=., data=XY, proj4string=CRS("+init=epsg:3828")) %>%
    spTransform(.,  CRS("+init=epsg:4326")) %>% coordinates  
}

twd97to84 <- function(XY) {
  aa<- 
    cbind(as.numeric(XY$X),as.numeric(XY$Y))%>%
    SpatialPointsDataFrame(coords=., data=XY, proj4string=CRS("+init=epsg:3826")) %>%
    spTransform(.,  CRS("+init=epsg:4326")) %>% coordinates  
}


#============================

S1517 <- 
  lapply(paste0("./data/raw/BBSdata/", 2015:2017), function(x){
    list.files(x, pattern = "BBSdata_", full.names = T) %>%  
      read_xlsx(., sheet = "birddata", cell_cols("D:AF")) %>% 
      setDT %>%
      .[時段 %in% c("0-3minutes", "3-6minutes"),] %>%
      .[調查旅次編號 %in% c(1,2)] %>%
      .[ 分析 %in% "Y", list(年, 樣區編號, 樣點編號, 座標系統, X座標, Y座標, 調查旅次編號)] %>%
      setnames(., c("Year", "Site_N", "Point", "cood", "X", "Y", "Survey")) %>%
      .[!duplicated(.)]  
  } ) %>% 
  do.call(rbind, .) %>% 
  data.table%>%
  .[, Site_N := as.character(Site_N)] %>%
  .[, Point := as.numeric(Point)]


S1517[S1517=="NA"] <- NA
S1517[S1517==""] <- NA



aa<- S1517 %>% .[,.N, by =list(Site_N, Point, Year, Survey)] %>%
  dcast(., Site_N + Point~ Year+ Survey, value.var="N")%>% 
  setDT %>%
  .[, Site_N := as.character(Site_N)] %>%
  .[, Point := as.numeric(Point)]



point_Forest <- read_excel("data/clean/point_Forest_1519.xlsx") %>% 
  setDT %>%
  .[, Site_N := as.character(Site_N)] %>%
  .[, Point := as.numeric(Point)]


ff<- point_Forest %>% full_join(aa, by = c("Site_N", "Point"))

ff.1 <- ff %>%setDT %>% .[  is.na(`2015_1.x`)&
                              is.na(`2015_2.x`) &
                              is.na(`2016_1.x`) & 
                              is.na(`2016_2.x`) & 
                              is.na(`2017_1.x`) & 
                              is.na(`2017_2.x`) & 
                              is.na(`2018_1`) & 
                              is.na(`2018_2`) & 
                              is.na(`2019_1`) & 
                              is.na(`2019_2`),] %>%
  left_join(point.list, by = c("Site_N", "Point")) %>%
  setDT %>%
  .[!is.na(Pointid),] #用樣點表的座標


ff.2 <- point_Forest %>% anti_join(aa, ., by = c("Site_N", "Point"))%>%
  anti_join(., point.list, by = c("Site_N", "Point"))%>%
  setDT %>%
  .[(is.na(`2017_1`) & is.na(`2017_2`)),]  %>%
  left_join(S1517[, list(Site_N, Point,cood, X ,Y)],  by = c("Site_N", "Point"))%>% setDT %>% 
  .[!duplicated(.)]   #需要抓BBS bird的座標


























S16.1<- S16[cood %in% "TWD67/TM2", list(X,Y)]  %>% 
  split(.,  seq(nrow(.))) %>%
  lapply(.,twd67to84) %>% 
  do.call( rbind, .) %>% 
  as.data.table(.) %>%
  setnames(., c("X_new","Y_new"))  %>%
  cbind(S16[cood %in% "TWD67/TM2",], .)

S16.2<- S16[cood %in% "TWD97/TM2", list(X,Y)]  %>% 
  split(.,  seq(nrow(.))) %>%
  lapply(.,twd97to84) %>% 
  do.call( rbind, .) %>% 
  as.data.table(.) %>%
  setnames(., c("X_new","Y_new"))  %>%
  cbind(S16[cood %in% "TWD97/TM2",], .)

S16.3<- S16[cood %like% "分",  ] 

X_new <- S16[cood %like% "分",  X]%>% 
  as.character(.) %>%
  char2dms(., chd = "°", chm = "'", chs = "\"") %>%
  as.numeric(.)  

Y_new <- S16[cood %like% "分",  Y] %>% 
  as.character(.) %>% 
  char2dms(., chd = "°", chm = "'", chs = "\"") %>%
  as.numeric(.)     
    
    

S16[cood %like% "分",  Y] %>%strsplit(.,"\"")
  
  
  


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




