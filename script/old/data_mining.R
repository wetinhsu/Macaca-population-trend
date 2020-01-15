
#---- Fill na forest type
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


#---- load library

library(data.table)
library(magrittr)
library(readxl)
library(tidyr)
library(purrr)
library(dplyr)
library(writexl)

#---- import data

setwd("C:/Users/wetin/Desktop/R/Macaca-population-trend")

# 2015
S15 <- 
  read_xlsx("data/raw/Macaca_2015_2017_analysis.xlsx",
            sheet = 2) %>% 
  setDT %>% 
  .[Year %in% 2015, 
    list(Site_N, Point, TypeName_O, Distance_O,
         X_new, Y_new,
         Year, Survey)] %>% 
  dcast(Site_N + Point + TypeName_O + Distance_O + X_new + Y_new~
          Year + Survey,
        fill = NA, fun = length)
# 2016
S16 <- 
  read_xlsx("data/raw/Macaca_2015_2017_analysis.xlsx",
            sheet = 2) %>% 
  setDT %>% 
  .[Year %in% 2016, 
    list(Site_N, Point, TypeName_O, Distance_O,
         X_new, Y_new,
         Year, Survey)] %>%
  .[ !duplicated(.),] %>%
  dcast(Site_N + Point + TypeName_O + Distance_O +X_new + Y_new~
          Year + Survey,
        fill = NA, fun = length)%>% 
  
  .[ Site_N %in% "AA" , Site_N := "A29-26"] %>%
  .[ Site_N %in% "AB" , Site_N := "A29-27"] %>%
  .[ Site_N %in% "AC" , Site_N := "A05-20"] %>%
  .[ Site_N %in% "AD" , Site_N := "A33-36"] %>%
  .[ Site_N %in% "AE" , Site_N := "A17-21"] %>%
  .[ Site_N %in% "AF" , Site_N := "A33-35"] %>%
  .[ Site_N %in% "AG" , Site_N := "A04-56"] %>%
  .[ Site_N %in% "AH" , Site_N := "A05-21"] %>%
  .[ Site_N %in% "AI" , Site_N := "A33-37"]

# 2017
S17 <- 
  read_xlsx("data/raw/2017獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 2) %>% 
  setDT %>% 
  .[, list(`樣點編號`,
           X_2017 = WGS84_X...11,
           Y_2017 = WGS84_Y...12)] %>% 
  separate("樣點編號", c("Site_a", "Site_b","Point"), "-") %>% 
  .[, Site_N := paste(Site_a, Site_b, sep = "-")] %>% 
  .[, c("Site_a", "Site_b") := NULL] %>% 
  .[, Point := as.numeric(Point)]
S17.survey.condition <- 
  read_xlsx("data/raw/2017獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 1) %>% 
  setDT %>% 
  .[, list(`樣區\r\n編號`, 第一旅次, 第二旅次)] %>% 
  setnames(c("Site_N" ,"2017_1", "2017_2")) %>% 
  .[, list(`2017_1` = ifelse(is.na(`2017_1`), 0, 1),
           `2017_2` = ifelse(is.na(`2017_2`), 0, 1)),
    by = Site_N] %>% 
  unique
  
S17 %<>% S17.survey.condition[., on = "Site_N"]  


# 2018
S18 <- 
  read_xlsx("data/raw/2018獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 2,
            col_types = "text") %>% 
  setDT %>% 
  .[, list(`樣點編號`,
           X_2018 = as.numeric(WGS84_X...10),
           Y_2018 = as.numeric(WGS84_Y...11))] %>% 
  separate("樣點編號", c("Site_a", "Site_b","Point"), "-") %>% setDT %>%
  .[, Site_N := paste(Site_a, Site_b, sep = "-")]  %>%
  .[, c("Site_a", "Site_b") := NULL] %>% 
  .[, Point := as.numeric(Point)]
S18.survey.condition <- 
  read_xlsx("data/raw/2018獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 1) %>% 
  setDT %>% 
  .[, list(`樣區\r\n編號`, 第一旅次, 第二旅次)] %>% 
  setnames(c("Site_N" ,"2018_1", "2018_2")) %>% 
  .[, list(`2018_1` = ifelse(is.na(`2018_1`), 0, 1),
           `2018_2` = ifelse(is.na(`2018_2`), 0, 1)),
    by = Site_N]

S18 %<>% S18.survey.condition[., on = "Site_N"]  



# 2019
S19 <- 
  read_xlsx("data/raw/2019獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 2,
            col_types = "text") %>% 
  setDT %>% 
  .[, list(`樣點編號`,
           X_2019 = as.numeric(WGS84_X...10),
           Y_2019 = as.numeric(WGS84_Y...11))] %>% 
  separate("樣點編號", c("Site_a", "Site_b","Point"), "-") %>% setDT %>%
  .[, Site_N := paste(Site_a, Site_b, sep = "-")]  %>%
  .[, c("Site_a", "Site_b") := NULL] %>% 
  .[, Point := as.numeric(Point)]
S19.survey.condition <- 
  read_xlsx("data/raw/2019獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 1) %>% 
  setDT %>% 
  .[, list(`樣區\r\n編號`, 第一旅次, 第二旅次)] %>% 
  setnames(c("Site_N" ,"2019_1", "2019_2")) %>% 
  .[, list(`2019_1` = ifelse(is.na(`2019_1`), 0, 1),
           `2019_2` = ifelse(is.na(`2019_2`), 0, 1)),
    by = Site_N]

S19 %<>% S19.survey.condition[., on = "Site_N"]  




#---- Deal with coordinate and forest type

S1518 <- read_excel("data/clean/point_Forest_1518.xlsx") %>%
  setDT %>%
  .[ ,list(Site_N, Point, TypeName_O = TypeName_O.x, Distance_O)]%>%
  .[ !duplicated(.),] %>%
  .[ Site_N %in% "AA" , Site_N := "A29-26"] %>%
  .[ Site_N %in% "AB" , Site_N := "A29-27"] %>%
  .[ Site_N %in% "AC" , Site_N := "A05-20"] %>%
  .[ Site_N %in% "AD" , Site_N := "A33-36"] %>%
  .[ Site_N %in% "AE" , Site_N := "A17-21"] %>%
  .[ Site_N %in% "AF" , Site_N := "A33-35"] %>%
  .[ Site_N %in% "AG" , Site_N := "A04-56"] %>%
  .[ Site_N %in% "AH" , Site_N := "A05-21"] %>%
  .[ Site_N %in% "AI" , Site_N := "A33-37"]



# left_join S17, S18 with S1516
site_info <- 
  reduce(list( S1518, S15, S16, S17, S18,S19), # 
         full_join, 
         by = c("Site_N", "Point"),suffix = c("", ".y")) %>% 
  setDT
## coordinate
  ## 2015(_new.x), 2016(_new.y), 2017(_2017), 2018(_2018)
## forest type
  ## 2015(TypeName_O.x), 2016(TypeName_O.y)

site_info.F <- 
  site_info %>%  
  # fill na 2015 forest type
  .[is.na(TypeName_O), TypeName_O := TypeName_O.y] %>% 
  .[is.na(Distance_O), Distance_O := Distance_O.y] %>% 
  .[, c("TypeName_O.y", "Distance_O.y") := NULL]  %>%
  # fill na coordinates (2016, 2017, 2018)
  .[is.na(TypeName_O), TypeName_O := TypeName_O.y.y] %>% 
  .[is.na(Distance_O), Distance_O := Distance_O.y.y] %>% 
  .[is.na(X_new), X_new := X_new.y] %>% 
  .[is.na(Y_new), Y_new := Y_new.y] %>% 
  .[, c("TypeName_O.y.y", "Distance_O.y.y","X_new.y", "Y_new.y") := NULL] %>% 
  
  .[is.na(X_new), X_new := X_2017] %>% 
  .[is.na(Y_new), Y_new := Y_2017] %>%
  .[, c("X_2017", "Y_2017") := NULL] %>% 
  .[is.na(X_new), X_new := X_2018] %>% 
  .[is.na(Y_new), Y_new := Y_2018] %>%
  .[, c("X_2018", "Y_2018") := NULL]%>% 
  .[is.na(X_new), X_new := X_2019] %>% 
  .[is.na(Y_new), Y_new := Y_2019] %>%
  .[, c("X_2019", "Y_2019") := NULL] %>%
  setDT


# extract point without forest type and turn to SP
no.Fo <- 
  site_info.F[is.na(TypeName_O)]
coordinates(no.Fo) <- ~X_new + Y_new
proj4string(no.Fo) <- CRS("+init=epsg:4326")
no.Fo %<>% spTransform(CRS("+init=epsg:3826"))



## Set up container for results
n <- length(no.Fo)
nearest.Type <- character(n)
nearest.dist <- numeric(n)

## For each point, find name of nearest polygon (in this case, Belgian cantons)


Sys.time()

for (i in seq_along(nearest.Type)) {
  print(i)
  Distance <-gDistance(no.Fo[i,], forest4th, byid=TRUE)
  nearest.Type[i] <- forest4th@data$TypeName[which.min(Distance)] %>% as.character
  nearest.dist[i] <- min(Distance)
  
}

Sys.time()



no.Fo.dat <- 
  data.frame(Site_N = no.Fo@data$Site_N,
             Point = no.Fo@data$Point,
             TypeName = nearest.Type,
             Distance = nearest.dist)

no.Fo.dat$Site_N <- as.character(no.Fo.dat$Site_N)
no.Fo.dat$TypeName <- as.character(no.Fo.dat$TypeName)
# merge new forest type data with site_info.F 
site_info.final <- 
  left_join(site_info.F, no.Fo.dat, by = c("Site_N", "Point")) %>% #
  setDT %>% 
  .[is.na(TypeName_O), TypeName_O:= TypeName] %>% 
  .[is.na(Distance_O), Distance_O := Distance] %>% 
  .[, c("TypeName", "Distance") := NULL] %>% 
  .[Site_N == "A29-27" & Point == "7", TypeName_O := "竹闊混淆林"] %>% 
  unique

# export as xlsx
write_xlsx(site_info.final, 
           "data/clean/point_Forest_1519.xlsx")

