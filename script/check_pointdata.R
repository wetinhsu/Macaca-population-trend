
#---- load library

library(data.table)
library(magrittr)
library(readxl)
library(tidyr)
library(purrr)
library(dplyr)
library(writexl)
library(rgdal)


#---- forest type
library(rgdal)
library(rgeos)
library(magrittr)
library(data.table)

setwd("C:/Users/wetin/Desktop/R/第四次森林資源調查全島森林林型分布圖")

Sys.time()
forest4th <- 
  readOGR(".",
          "全島森林林型分布圖", 
          use_iconv = TRUE, encoding = "UTF-8") %>% 
  spTransform(CRS("+init=epsg:3826"))

Sys.time()

#==============================

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

S15 <- 
  lapply(paste0("./data/raw/BBSdata/", 2015), function(x){
    list.files(x, pattern = "BBSdata_", full.names = T) %>%  
      read_xlsx(., sheet = "birddata", cell_cols("D:AF")) %>% 
      setDT %>%
      #.[時段 %in% c("0-3minutes", "3-6minutes"),] %>%
      .[!(時段 %in% "Supplementary"),] %>%
      .[調查旅次編號 %in% c(1,2)] %>%
      .[ 分析 %in% "Y", list(年, 樣區編號, 樣點編號, 座標系統, X座標, Y座標, 調查旅次編號)] %>%
      setnames(., c("Year", "Site_N", "Point", "cood", "X", "Y", "Survey")) %>%
      .[!duplicated(.)]  
  } ) %>% 
  do.call(rbind, .) %>% 
  data.table%>%
  .[ !(Site_N %in% "A09-31" & X %like% "63"),] %>%
  .[, Site_N := as.character(Site_N)] %>%
  .[, Point := as.numeric(Point)]


S15[S15=="NA"] <- NA
S15[S15==""] <- NA

S15 %<>% 
  .[cood %like% "分", 
           X_new := as.numeric(char2dms(X, chd = "°", chm = "'", chs = "\""))]
S15 %<>% 
  .[cood %like% "分",
    Y_new := as.numeric(char2dms(Y, chd = "°", chm = "'", chs = "\""))]


  S15 %<>% .[cood %in% "WGS84/經緯度(度)",
            X_new := as.numeric(X)]
  S15 %<>% .[cood %in% "WGS84/經緯度(度)",
            Y_new := as.numeric(Y)]
  
  S15 %>% setDT %>%
    .[duplicated(., by = c("Site_N", "Point", "Survey"))]
  
  aa<- S15 %>% .[,.N, by =list(Site_N, Point, Year, Survey)] %>%
    dcast(., Site_N + Point~ Year+ Survey, value.var="N")%>% 
    setDT %>%
    .[, Site_N := as.character(Site_N)] %>%
    .[, Point := as.numeric(Point)]
  
  apply(aa[,3:4] ,2,sum,na.rm=T)
#============  
  
  S16 <- 
    lapply(paste0("./data/raw/BBSdata/", 2016), function(x){
      list.files(x, pattern = "BBSdata_", full.names = T) %>%  
        read_xlsx(., sheet = "birddata", cell_cols("D:AF")) %>% 
        setDT %>%
        #.[時段 %in% c("0-3minutes", "3-6minutes"),] %>%
        .[!(時段 %in% "Supplementary"),] %>%
        .[調查旅次編號 %in% c(1,2)] %>%
        .[ 分析 %in% "Y", list(年, 樣區編號, 樣點編號, 座標系統, X座標, Y座標, 調查旅次編號)] %>%
        setnames(., c("Year", "Site_N", "Point", "cood", "X", "Y", "Survey")) %>%
        .[!duplicated(.)]  
    } ) %>% 
    do.call(rbind, .) %>% 
    data.table%>%
    .[  Y %like% "25'1'", Y:=as.character("25°1'64.77\"")] %>%
    .[, Site_N := as.character(Site_N)] %>%
    .[, Point := as.numeric(Point)]
  
  
  S16[S16=="NA"] <- NA
  S16[S16==""] <- NA
  
  S16 %<>% 
    .[cood %like% "分", 
      X_new := as.numeric(char2dms(X, chd = "°", chm = "'", chs = "\""))] %>% 
  
    .[cood %like% "分",
      Y_new := as.numeric(char2dms(Y, chd = "°", chm = "'", chs = "\""))] %>% 
  
  
  .[cood %in% "WGS84/經緯度(度)",
             X_new := as.numeric(X)] %>% 
  .[cood %in% "WGS84/經緯度(度)",
               Y_new := as.numeric(Y)]
  
  
  S16 %>% setDT %>%
    .[duplicated(., by = c("Site_N", "Point", "Survey"))]
  
  bb<- S16 %>% .[!duplicated(., by = c("Site_N", "Point", "Survey"))] %>%
    .[,.N, by =list(Site_N, Point, Year, Survey)] %>% 
    dcast(., Site_N + Point~ Year+ Survey, value.var="N")%>% 
    setDT %>%
    .[, Site_N := as.character(Site_N)] %>%
    .[, Point := as.numeric(Point)]  
  
  apply(bb[,3:4] ,2,sum,na.rm=T)
  
#============
  
  
  S17 <- 
    lapply(paste0("./data/raw/BBSdata/", 2017), function(x){
      list.files(x, pattern = "BBSdata_", full.names = T) %>%  
        read_xlsx(., sheet = "birddata", cell_cols("D:AF")) %>% 
        setDT %>%
        #.[時段 %in% c("0-3minutes", "3-6minutes"),] %>%
        .[!(時段 %in% "Supplementary"),] %>%
        .[調查旅次編號 %in% c(1,2)] %>%
        .[ 分析 %in% "Y", list(年, 樣區編號, 樣點編號, 座標系統, X座標, Y座標, 調查旅次編號)] %>%
        setnames(., c("Year", "Site_N", "Point", "cood", "X", "Y", "Survey")) %>%
        .[!duplicated(.)]  
    } ) %>% 
    do.call(rbind, .) %>% 
    data.table%>%
    .[, Site_N := as.character(Site_N)] %>%
    .[, Point := as.numeric(Point)]
  
  
  S17[S17=="NA"] <- NA
  S17[S17==""] <- NA
  
  S17 %<>% 
    .[cood %like% "分", 
      X_new := as.numeric(char2dms(X, chd = "°", chm = "'", chs = "\""))] %>% 
    
    .[cood %like% "分",
      Y_new := as.numeric(char2dms(Y, chd = "°", chm = "'", chs = "\""))] %>% 
    
    
    .[cood %in% "WGS84/經緯度(度)",
      X_new := as.numeric(X)] %>% 
    .[cood %in% "WGS84/經緯度(度)",
      Y_new := as.numeric(Y)]
  
  
  S17 %>% setDT %>%
    .[duplicated(., by = c("Site_N", "Point", "Survey"))]
  
  cc<- S17 %>% .[!duplicated(., by = c("Site_N", "Point", "Survey"))] %>%
    .[,.N, by =list(Site_N, Point, Year, Survey)] %>% 
    dcast(., Site_N + Point~ Year+ Survey, value.var="N")%>% 
    setDT %>%
    .[, Site_N := as.character(Site_N)] %>%
    .[, Point := as.numeric(Point)]  
  
  apply(cc[,3:4] ,2,sum,na.rm=T)
  
#=============
  
  point_Forest <- read_excel("data/clean/point_Forest_1519.xlsx") %>% 
    setDT %>%
    .[, Site_N := as.character(Site_N)] %>%
    .[, Point := as.numeric(Point)]
  
  
  point_Forest.1 <- point_Forest%>%.[duplicated(., by = c("Site_N", "Point"))] %>% 
    .[, list(Site_N, Point)]%>%
    anti_join(point_Forest, ., by = c("Site_N", "Point"))
  
dd <- aa %>% full_join(bb, by = c("Site_N", "Point") ) %>%
  full_join(cc, by = c("Site_N", "Point") )  %>%
  left_join(point_Forest.1 , by = c("Site_N", "Point"))
  
  
dd.0 <- dd %>% setDT %>% .[is.na(TypeName_O),] %>%
    left_join(point.list, by = c("Site_N", "Point") )

dd.1 <- dd.0 %>% setDT %>% .[is.na(Pointid),] %>%
  left_join(S15, by = c("Site_N", "Point"))

dd.2 <- dd.1 %>% setDT %>% .[is.na(cood),] %>%
  left_join(S16, by = c("Site_N", "Point")) %>%
  setDT %>%
  .[, Survey.y := NULL] %>% unique

#===================================


dd.2.Fo <- 
  dd.2 %>% setDT %>% .[,list(Site_N,Point, 
                             x=as.numeric(X_new),y=as.numeric(Y_new))] 
coordinates(dd.2.Fo) <- ~x + y


proj4string(dd.2.Fo) <- CRS("+init=epsg:4326")
dd.2.Fo %<>% spTransform(CRS("+init=epsg:3826"))



## Set up container for results
n <- length(dd.2.Fo)
nearest.Type <- character(n)
nearest.dist <- numeric(n)

## For each point, find name of nearest polygon (in this case, Belgian cantons)

Sys.time()

for (i in seq_along(nearest.Type)) {
  print(i)
  Distance <-gDistance(dd.2.Fo[i,], forest4th, byid=TRUE)
  nearest.Type[i] <- forest4th@data$TypeName[which.min(Distance)] %>% as.character
  nearest.dist[i] <- min(Distance)
  
}

Sys.time()



dd.2.Fo.dat <- 
  data.frame(Site_N = dd.2.Fo@data$Site_N,
             Point = dd.2.Fo@data$Point,
             TypeName = nearest.Type,
             Distance = nearest.dist)




#==============================================


dd.1.Fo <- 
  dd.1 %>% setDT %>% .[!is.na(cood),list(Site_N,Point, 
                             x=as.numeric(X_new.y),y=as.numeric(Y_new.y))] 
coordinates(dd.1.Fo) <- ~x + y


proj4string(dd.1.Fo) <- CRS("+init=epsg:4326")
dd.1.Fo %<>% spTransform(CRS("+init=epsg:3826"))



## Set up container for results
n <- length(dd.1.Fo)
nearest.Type <- character(n)
nearest.dist <- numeric(n)

## For each point, find name of nearest polygon (in this case, Belgian cantons)

Sys.time()

for (i in seq_along(nearest.Type)) {
  print(i)
  Distance <-gDistance(dd.1.Fo[i,], forest4th, byid=TRUE)
  nearest.Type[i] <- forest4th@data$TypeName[which.min(Distance)] %>% as.character
  nearest.dist[i] <- min(Distance)
  
}

Sys.time()



dd.1.Fo.dat <- 
  data.frame(Site_N = dd.1.Fo@data$Site_N,
             Point = dd.1.Fo@data$Point,
             TypeName = nearest.Type,
             Distance = nearest.dist)


#==============================================


dd.0.Fo <- 
  dd.0 %>% setDT %>% .[!is.na(Pointid),list(Site_N,Point, 
                                         x=as.numeric(X.84),y=as.numeric(Y.84))] 
coordinates(dd.0.Fo) <- ~x + y


proj4string(dd.0.Fo) <- CRS("+init=epsg:4326")
dd.0.Fo %<>% spTransform(CRS("+init=epsg:3826"))



## Set up container for results
n <- length(dd.0.Fo)
nearest.Type <- character(n)
nearest.dist <- numeric(n)

## For each point, find name of nearest polygon (in this case, Belgian cantons)

Sys.time()

for (i in seq_along(nearest.Type)) {
  print(i)
  Distance <-gDistance(dd.0.Fo[i,], forest4th, byid=TRUE)
  nearest.Type[i] <- forest4th@data$TypeName[which.min(Distance)] %>% as.character
  nearest.dist[i] <- min(Distance)
  
}

Sys.time()



dd.0.Fo.dat <- 
  data.frame(Site_N = dd.0.Fo@data$Site_N,
             Point = dd.0.Fo@data$Point,
             TypeName = nearest.Type,
             Distance = nearest.dist)


rbind(dd.0.Fo.dat, dd.1.Fo.dat, dd.2.Fo.dat) %>% setDT %>%
  .[,list(Site_N, Point)] %>%.[duplicated(.)] 


setDT(point_Forest.1)

fin <- dd %>% setDT %>% 
  .[,list(Site_N, Point, TypeName_O, Distance_O,
           `2015_1.x`,`2015_2.x`,
           `2015_1.x`,`2016_2.x`,
           `2017_1.x`,`2017_2.x`)]%>%

  left_join(rbind(dd.0.Fo.dat, dd.1.Fo.dat, dd.2.Fo.dat), by = c("Site_N", "Point")) %>% 
  left_join(point_Forest.1[ ],
            by = c("Site_N", "Point")) %>%
  setDT %>%
  .[is.na(TypeName), TypeName:= TypeName_O] %>%
  .[is.na(Distance), Distance:= Distance_O] 


