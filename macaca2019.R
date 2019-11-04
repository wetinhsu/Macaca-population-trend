
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
setwd("C:/Users/wetin/Desktop/R/Macaca-population-trend")


point.list <- 
  read_xlsx("Macaca 2019/data/0_林管處_raw data/0_初檢核/bind.xlsx",
                        sheet = "樣點",  cell_cols("A:L"))  %>% 
  setDT %>% 
  .[!duplicated(.)]  %>% 
  setDT

point <- point.list
names(point)

coordinates(point) <- ~TWD97_X + TWD97_Y
proj4string(point) <- CRS("+init=epsg:3826")


## Set up container for results
n <- length(point)
nearest.Type <- character(n)
nearest.dist <- numeric(n)



Sys.time()

for (i in seq_along(nearest.Type)) {
  print(i)
  Distance <-gDistance(point[i,], forest4th, byid=TRUE)
  nearest.Type[i] <- forest4th@data$TypeName[which.min(Distance)] %>% as.character
  nearest.dist[i] <- min(Distance)
  
}

Sys.time()

point.dat <- 
  data.frame(Site_N = point@data$`樣區\r\n編號`,
             Point = point@data$`樣點\r\n代號`,
             TypeName = nearest.Type,
             Distance = nearest.dist)


Sys.time()


write_xlsx(point.dat,"Macaca 2019/data/0_林管處_raw data/0_初檢核/point_Forest.xlsx")





#==============================
setwd("C:/Users/wetin/Desktop/R/Macaca-population-trend")


point.list <- 
  read_xlsx("Macaca 2019/data/0_林管處_raw data/0_初檢核/bind.xlsx",
            sheet = "Sheet1",  range = "C1:W3132")  %>% 
  setDT %>% 
  .[,list(樣區編號, 旅次, 樣點編號,
             TWD97_X = `X座標(TWD97)`,
             TWD97_Y = `Y座標(TWD97)`)]%>%
  .[!duplicated(TWD97_X,TWD97_Y)]  %>%
  setDF

point <- point.list
names(point)

coordinates(point) <- ~TWD97_X + TWD97_Y
proj4string(point) <- CRS("+init=epsg:3826")


## Set up container for results
n <- length(point)
nearest.Type <- character(n)
nearest.dist <- numeric(n)



Sys.time()

for (i in seq_along(nearest.Type)) {
  print(i)
  Distance <-gDistance(point[i,], forest4th, byid=TRUE)
  nearest.Type[i] <- forest4th@data$TypeName[which.min(Distance)] %>% as.character
  nearest.dist[i] <- min(Distance)
  
}

Sys.time()

point.dat <- 
  data.frame(Site_N = point@data$`樣區編號`,
             Point = point@data$`樣點編號`,
             trip = point@data$`旅次`,
             TypeName = nearest.Type,
             Distance = nearest.dist)


Sys.time()


write_xlsx(point.dat,"Macaca 2019/data/0_林管處_raw data/0_初檢核/survey_Forest.xlsx")

#==============================
setwd("C:/Users/wetin/Desktop/R/Macaca-population-trend")


point.list <- 
  read_xlsx("Macaca 2019/data/0_林管處_raw data/0_初檢核/part2.xlsx",
            sheet = 1)  %>% 
  setDT %>% 
  .[,list(樣區編號, 旅次, 樣點編號,
              TWD97_X = `X座標(TWD97)`,
              TWD97_Y = `Y座標(TWD97)`)]%>%
  .[!duplicated(.)]  %>%
  setDF

point <- point.list
names(point)

coordinates(point) <- ~TWD97_X + TWD97_Y
proj4string(point) <- CRS("+init=epsg:3826")


## Set up container for results
n <- length(point)
nearest.Type <- character(n)
nearest.dist <- numeric(n)



Sys.time()

for (i in seq_along(nearest.Type)) {
  print(i)
  Distance <-gDistance(point[i,], forest4th, byid=TRUE)
  nearest.Type[i] <- forest4th@data$TypeName[which.min(Distance)] %>% as.character
  nearest.dist[i] <- min(Distance)
  
}

Sys.time()

point.dat <- 
  data.frame(Site_N = point@data$`樣區編號`,
             Point = point@data$`樣點編號`,
             trip = point@data$`旅次`,
             TypeName = nearest.Type,
             Distance = nearest.dist)


Sys.time()


write_xlsx(point.dat,"Macaca 2019/data/0_林管處_raw data/0_初檢核/survey_Forest-part2.xlsx")
